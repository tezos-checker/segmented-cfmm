-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.YToX where

import Prelude

import Data.Map ((!))
import Hedgehog hiding (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lorentz hiding (assert, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)
import Tezos.Core (timestampPlusSeconds, timestampToSeconds)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.SegCFMM.Contract
import Test.Util
import Util.Named

test_swapping_within_a_single_tick_range :: TestTree
test_swapping_within_a_single_tick_range =
  propOnNetwork "swapping within a single tick range"
    do
      tokenTypes <- forAll $ Gen.element allTokenTypeCombinations
      feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)

      -- With 1_e7 liquidity, we can deposit a little more than 500_000 Y tokens.
      -- So we'll generate up to 10 swaps of 50_000 tokens each.
      swaps <- forAll $
        Gen.list (Range.linear 1 10) $
          Gen.integral (Range.linear 0 50_000)

      pure (tokenTypes, feeBps, protoFeeBps, swaps)
    ( defaultTokenTypes
    , 7_00
    , 7_00
    , [1, 10, 100, 10_000]
    )
    \(tokenTypes, feeBps, protoFeeBps, swaps) -> do
      let liquidity = 1_e7
      let lowerTickIndex = -1000
      let upperTickIndex = 1000

      -- When the Y token is not CTEZ, we expect the contract to behave as if the protocol fee had been set to zero.
      let effectiveProtoFeeBps = if snd tokenTypes == CTEZ then protoFeeBps else 0

      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      swapReceiver <- newAddress auto
      feeReceiver <- newAddress auto
      transferMoney liquidityProvider 10_e6

      (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      -- Add some slots to the buffers to make the tests more meaningful.
      call cfmm (Call @"Increase_observation_count") 10

      withSender liquidityProvider $ setPosition cfmm liquidity (lowerTickIndex, upperTickIndex)

      for_ swaps \dy -> do
        initialSt <- getStorage cfmm
        initialBalanceSwapperX <- balanceOf x swapper
        initialBalanceSwapperY <- balanceOf y swapper
        initialBalanceSwapReceiverX <- balanceOf x swapReceiver
        initialBalanceSwapReceiverY <- balanceOf y swapReceiver

        withSender swapper $ ytox cfmm dy swapReceiver

        -- Advance the time 1 sec to make sure the buffer is updated to reflect the swaps.
        advanceSecs 1 [cfmm]

        finalSt <- getStorage cfmm

        -- The contract's `sqrt_price` has moved accordingly.
        let expectedFee = calcSwapFee feeBps dy
        let expectedNewPrice = calcNewPriceY (sSqrtPriceRPC initialSt) (sLiquidityRPC initialSt) (dy - expectedFee) effectiveProtoFeeBps
        adjustScale @30 (sSqrtPriceRPC finalSt) @== expectedNewPrice
        when (dy > 0 && feeBps > 0) do checkCompares expectedFee (>=) 1

        -- Check fee growth
        let expectedFeeGrowth =
              sFeeGrowthRPC initialSt +
                PerToken 0 (mkX @Natural @128 expectedFee `div` X liquidity)
        sFeeGrowthRPC finalSt @== expectedFeeGrowth

        -- The right amount of tokens was subtracted from the `swapper`'s balance
        let expectedDx = receivedX (sSqrtPriceRPC initialSt) (sSqrtPriceRPC finalSt) (sLiquidityRPC initialSt)
        balanceOf x swapper @@== initialBalanceSwapperX
        balanceOf y swapper @@== initialBalanceSwapperY - dy
        -- The right amount of tokens was sent to the `receiver`.
        balanceOf x swapReceiver @@== initialBalanceSwapReceiverX + fromIntegral @Integer @Natural expectedDx
        balanceOf y swapReceiver @@== initialBalanceSwapReceiverY

      -- `feeReceiver` receives the expected fees.
      collectFees cfmm feeReceiver 0 liquidityProvider
      balanceOf x feeReceiver @@== 0
      let expectedFees =
            swaps
            <&> (\dy -> calcSwapFee feeBps dy)
            & sum
      -- `update_position` rounds the fee down, so it's possible 1 X token is lost.
      receivedFee <- balanceOf y feeReceiver
      receivedFee `isInRangeNat` expectedFees $ (1, 0)

test_many_small_swaps :: TestTree
test_many_small_swaps =
  forAllTokenTypeCombinations "placing many small swaps is (mostly) equivalent to placing 1 big swap" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    -- Note that this property only holds in the absence of fees.
    -- When there _is_ a fee and a user swaps a very small amount of tokens,
    -- the fee is rounded up to 1 token.
    -- Over the course of many small swaps, this effect compounds and ends up
    -- making a big difference.
    let feeBps = 0
    let protoFeeBps = 0

    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    let swapCount = 1_000
    let swapAmount = 10

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto

    let accounts = [liquidityProvider, swapper]
    (x, y) <- originateTokenContracts accounts ((fst tokenTypes, FA2.TokenId 0), (snd tokenTypes, FA2.TokenId 1))
    let origParams = def { opTokens = Just (x, y), opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
    (cfmm1, _) <- prepareSomeSegCFMM accounts tokenTypes origParams
    (cfmm2, _) <- prepareSomeSegCFMM accounts tokenTypes origParams

    for_ [cfmm1, cfmm2] \cfmm -> do
      -- Add some slots to the buffers to make the tests more meaningful.
      call cfmm (Call @"Increase_observation_count") 10
      withSender liquidityProvider $ setPosition cfmm liquidity (lowerTickIndex, upperTickIndex)

    withSender swapper $ do
      -- 1 big swap
      ytox cfmm1 (swapCount * swapAmount) swapper
      -- many small swaps
      replicateM_ (fromIntegral swapCount) do
        ytox cfmm2 swapAmount swapper

    -- Advance the time 1 sec to make sure the buffer is updated to reflect the swaps.
    advanceSecs 1 [cfmm1, cfmm2]
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    -- The two storages should be mostly identical.
    -- The price might be slightly different, due to the compounding of rounding errors,
    -- so we take some precision away to account for this difference.
    st1 <- getFullStorage cfmm1
    st2 <- getFullStorage cfmm2
    let sqrtPrice1 = adjustScale @80 $ adjustScale @60 $ sSqrtPrice st1
    let sqrtPrice2 = adjustScale @80 $ adjustScale @60 $ sSqrtPrice st2

    sqrtPrice1 @== sqrtPrice2
    st1 { sSqrtPrice = sqrtPrice1 } @== st2 { sSqrtPrice = sqrtPrice2 }

    -- Due to `dx` being rounded down, it's possible the swapper loses *up to* 1 X token
    -- on every swap.
    -- So the 2nd contract may hold up to 1000 more X tokens than the 1st contract.
    cfmm1XBalance <- balanceOf x cfmm1
    cfmm2XBalance <- balanceOf x cfmm2
    cfmm2XBalance `isInRangeNat` cfmm1XBalance $ (0, swapCount)

    -- The two contracts should hold the same exact amount of Y tokens
    cfmm1YBalance <- balanceOf y cfmm1
    cfmm2YBalance <- balanceOf y cfmm2
    cfmm1YBalance @== cfmm2YBalance

test_crossing_ticks :: TestTree
test_crossing_ticks =
  forAllTokenTypeCombinations "executing a swap within a single tick range or across many ticks should be (mostly) equivalent" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let feeBps = 200 -- 2%

    -- The number of seconds to wait before executing the swap
    let waitTime = 3

    let liquidity = 1_e6
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    feeReceiver1 <- newAddress auto
    feeReceiver2 <- newAddress auto

    let accounts = [liquidityProvider, swapper]
    (x, y) <- originateTokenContracts accounts ((fst tokenTypes, FA2.TokenId 0), (snd tokenTypes, FA2.TokenId 1))
    let origParams = def { opTokens = Just (x, y), opModifyConstants = set cFeeBpsL feeBps }
    (cfmm1, _) <- prepareSomeSegCFMM accounts tokenTypes origParams
    (cfmm2, _) <- prepareSomeSegCFMM accounts tokenTypes origParams

    -- Add some slots to the buffers to make the tests more meaningful.
    for_ [cfmm1, cfmm2] \cfmm -> call cfmm (Call @"Increase_observation_count") 10

    -- Place 1 big position
    withSender liquidityProvider do
      setPosition cfmm1 liquidity (lowerTickIndex, upperTickIndex)
      -- Place many small positions with the same liquidity
      for_ [-1000, -900 .. 900] \lowerTickIndex' -> do
        setPosition cfmm2 liquidity (lowerTickIndex', lowerTickIndex' + 100)

    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    cfmm1InitialBalanceX <- balanceOf x cfmm1
    cfmm1InitialBalanceY <- balanceOf y cfmm1
    cfmm2InitialBalanceX <- balanceOf x cfmm2
    cfmm2InitialBalanceY <- balanceOf y cfmm2

    -- Place a small swap to move the tick past 0 and advance the time to fill the
    -- buffer with _something_ other than zeros.
    withSender swapper do
      for_ [cfmm1, cfmm2] \cfmm -> ytox cfmm 200 swapper
    advanceSecs waitTime [cfmm1, cfmm2]

    -- Place 1 big swap to push the tick all the way up to `upperTickIndex`
    initialSt2 <- getFullStorage cfmm2
    withSender swapper do
      for_ [cfmm1, cfmm2] \cfmm -> do
        ytox cfmm 50_000 swapper
    finalTime <- getNow

    -- Advance the time 1 sec to make sure the buffer is updated to reflect the swaps.
    advanceSecs 1 [cfmm1, cfmm2]
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    st1 <- getFullStorage cfmm1
    st2 <- getFullStorage cfmm2

    -- Sanity check: In order for this test to be meaningful, we need the `curTickIndex`
    -- to have moved close to `upperTickIndex` and have crossed several initialized ticks.
    sCurTickIndex st1 `isInRange` upperTickIndex $ (50, 0)

    -- Current tick should be the same.
    sCurTickIndex st1 @== sCurTickIndex st2

    -- "Fee growth" should be fairly similar.
    -- It can be slightly higher for the 2nd contract,
    -- because each time we cross an initialized tick, the fee can be rounded up once.
    -- Because in the 2nd scenario we're crossing 10 ticks, we allow for a difference of up to 10 extra Y tokens in fees.
    let PerToken feeGrowthX1 feeGrowthY1 = sFeeGrowth st1
    let PerToken feeGrowthX2 feeGrowthY2 = sFeeGrowth st2
    feeGrowthX1 @== 0
    feeGrowthX2 @== 0
    let marginOfError = pickX (mkX @_ @128 10) `div` liquidity
    feeGrowthY2 `isInRangeNat` feeGrowthY1 $ (0, marginOfError)


    let calcBalanceDelta initial final = fromIntegral @Natural @Integer final - fromIntegral @Natural @Integer initial
    cfmm1BalanceDeltaX <- balanceOf x cfmm1 <&> calcBalanceDelta cfmm1InitialBalanceX
    cfmm1BalanceDeltaY <- balanceOf y cfmm1 <&> calcBalanceDelta cfmm1InitialBalanceY
    cfmm2BalanceDeltaX <- balanceOf x cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceX
    cfmm2BalanceDeltaY <- balanceOf y cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceY
    -- The two contract should have received the exact same amount of Y tokens
    cfmm1BalanceDeltaY @== cfmm2BalanceDeltaY
    -- The 2nd contract may have given out fewer X tokens for two reasons:
    --   1. due to the potential increase in fees explained above
    --   2. due to the rounding up of `dy` when crossing a tick
    -- We had a margin error of 10 for each possible cause.
    cfmm2BalanceDeltaX `isInRange` cfmm1BalanceDeltaX $ (0, 10 + 10)

    -- Collected fees should be fairly similar.
    -- As explained above, the contract may charge up to 10 extra tokens.
    -- However, when an LP collects fees for a position, the distribution of fees can be rounded down,
    -- so we allow for a margin of error of +/-10 Y tokens.
    collectAllFees cfmm1 feeReceiver1
    collectAllFees cfmm2 feeReceiver2
    balanceOf x feeReceiver1 @@== 0
    balanceOf x feeReceiver2 @@== 0
    feeReceiver1BalanceY <- balanceOf y feeReceiver1
    feeReceiver2BalanceY <- balanceOf y feeReceiver2
    feeReceiver2BalanceY `isInRangeNat` feeReceiver1BalanceY $ (10, 10)

    -- The global accumulators of both contracts should be the same.
    sCumulativesBuffer st1 @== sCumulativesBuffer st2

        -- Check that the ticks' states were updated correctly after being crossed.
    let crossedTicks = [100, 200 .. 900] <&> \idx -> bmMap (sTicks st2) ! idx
    for_ crossedTicks \ts -> do
      tsSecondsPerLiquidityOutside ts @== mkX waitTime `div` X liquidity
      tsSecondsOutside ts @== timestampToSeconds finalTime
      tsTickCumulativeOutside ts @== fromIntegral @TickIndex @Integer (sCurTickIndex initialSt2) * fromIntegral waitTime
      tsFeeGrowthOutside ts @/= 0

test_fee_split :: TestTree
test_fee_split =
  forAllTokenTypeCombinations "fees are correctly assigned to each position" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let feeBps = 50_00 -- 50%

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    feeReceiver1 <- newAddress auto
    feeReceiver2 <- newAddress auto
    (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def { opModifyConstants = set cFeeBpsL feeBps }

    withSender liquidityProvider do
      setPosition cfmm 1_e6 (-100, 100)
      setPosition cfmm 1_e6 (100, 200)

    withSender swapper do
      -- Place a small x-to-y swap.
      -- It's small enough to be executed within the [-100, 100] range,
      -- so the X fee is paid to position1 only.
      xtoy cfmm 1_000 swapper

      -- Place a big y-to-x swap.
      -- It's big enough to cross from the [-100, 100] range into the [100, 200] range,
      -- so the Y fee is paid to both position1 and position2.
      ytox cfmm 20_000 swapper

    checkAllInvariants cfmm

    -- position1 should have earned both X and Y fees.
    collectFees cfmm feeReceiver1 0 liquidityProvider
    balanceOf x feeReceiver1 @@/= 0
    balanceOf y feeReceiver1 @@/= 0

    -- position2 should have earned Y fees only.
    collectFees cfmm feeReceiver2 1 liquidityProvider
    balanceOf x feeReceiver2 @@== 0
    balanceOf y feeReceiver2 @@/= 0

test_must_exceed_min_dx :: TestTree
test_must_exceed_min_dx =
  forAllTokenTypeCombinations "swap fails if the user would receiver less than min_dx" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
    withSender liquidityProvider $ setPosition cfmm 1_e7 (-1000, 1000)

    withSender swapper do
      call cfmm (Call @"Y_to_x") YToXParam
        { ypDy = 1
        , ypDeadline = validDeadline
        , ypMinDx = 1000
        , ypToDx = swapper
        }
        & expectFailedWith (smallerThanMinAssetErr (#min .! 1000, #actual .! 0))

test_fails_if_its_past_the_deadline :: TestTree
test_fails_if_its_past_the_deadline =
  forAllTokenTypeCombinations "swap fails if it's past the deadline" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
    withSender liquidityProvider $ setPosition cfmm 1_e7 (-1000, 1000)

    withSender swapper do
      now <- getNow
      let expiredDeadline = now `timestampPlusSeconds` -1
      call cfmm (Call @"Y_to_x") YToXParam
        { ypDy = 1
        , ypDeadline = expiredDeadline
        , ypMinDx = 0
        , ypToDx = swapper
        }
        & expectFailedWith (pastDeadlineErr (#deadline .! expiredDeadline, #executed_at .! now))

test_swaps_are_noops_when_liquidity_is_zero :: TestTree
test_swaps_are_noops_when_liquidity_is_zero =
  forAllTokenTypeCombinations "After crossing into a 0-liquidity range, swaps are no-ops" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
    withSender liquidityProvider $ setPosition cfmm 10_000 (-100, 100)

    withSender swapper do
      -- Place a swap big enough to exhaust the position's liquidity
      ytox cfmm 200 swapper

      let
        isNoOp op = do
          initialSt <- getFullStorage cfmm
          initialBalanceX <- balanceOf x cfmm
          initialBalanceY <- balanceOf y cfmm
          op
          getFullStorage cfmm @@== initialSt
          balanceOf x cfmm @@== initialBalanceX
          balanceOf y cfmm @@== initialBalanceY

      isNoOp $ xtoy cfmm 100 swapper
      isNoOp $ ytox cfmm 100 swapper
