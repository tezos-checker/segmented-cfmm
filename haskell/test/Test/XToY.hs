-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.XToY where

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
import Test.Tasty.Hedgehog (testProperty)
import Tezos.Core (timestampPlusSeconds)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.Util

test_swapping_within_a_single_tick_range :: TestTree
test_swapping_within_a_single_tick_range =
  testProperty "swapping within a single tick range" $ property do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    -- With the liquidity above, we can deposit a little more than 500_000 Y tokens.
    -- So we'll generate up to 10 swaps of 50_000 tokens each.
    swaps <- forAll $
      Gen.list (Range.linear 1 10) $
        Gen.integral (Range.linear 0 50_000)

    feeBps <- forAll $ Gen.integral (Range.linear 0 10_000)

    clevelandProp do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      swapReceiver <- newAddress auto
      feeReceiver <- newAddress auto

      (cfmm, (TokenInfo xTokenId xToken, TokenInfo yTokenId yToken)) <- prepareSomeSegCFMM' [liquidityProvider, swapper] Nothing Nothing (set cFeeBpsL feeBps)
      -- Add some slots to the buffers to make the tests more meaningful.
      call cfmm (Call @"Increase_observation_count") 10

      withSender liquidityProvider do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }
        checkAllInvariants cfmm

      for_ swaps \dx -> do
        initialSt <- getFullStorage cfmm
        initialBalanceSwapperX <- balanceOf xToken xTokenId swapper
        initialBalanceSwapperY <- balanceOf yToken yTokenId swapper
        initialBalanceSwapReceiverX <- balanceOf xToken xTokenId swapReceiver
        initialBalanceSwapReceiverY <- balanceOf yToken yTokenId swapReceiver

        withSender swapper do
          call cfmm (Call @"X_to_y") XToYParam
            { xpDx = dx
            , xpDeadline = validDeadline
            , xpMinDy = 0
            , xpToDy = swapReceiver
            }
        -- Advance the time 1 sec to make sure the buffer is updated to reflect the swaps.
        advanceSecs 1 [cfmm]
        checkAllInvariants cfmm

        finalSt <- getFullStorage cfmm

        -- The contract's `sqrt_price` has moved accordingly.
        let expectedFee = calcSwapFee feeBps dx
        let expectedNewPrice = calcNewPriceX (sSqrtPrice initialSt) (sLiquidity initialSt) (dx - expectedFee)
        adjustScale @30 (sSqrtPrice finalSt) @== expectedNewPrice
        when (dx > 0 && feeBps > 0) do checkCompares expectedFee (>=) 1

        -- Check fee growth
        let expectedFeeGrowth =
              sFeeGrowth initialSt +
                PerToken (mkX @Natural @128 expectedFee `div` X liquidity) 0
        sFeeGrowth finalSt @== expectedFeeGrowth

        -- The right amount of tokens was subtracted from the `swapper`'s balance
        let expectedDy = receivedY (sSqrtPrice initialSt) (sSqrtPrice finalSt) (sLiquidity initialSt)
        balanceOf xToken xTokenId swapper @@== initialBalanceSwapperX - dx
        balanceOf yToken yTokenId swapper @@== initialBalanceSwapperY
        -- The right amount of tokens was sent to the `receiver`.
        balanceOf xToken xTokenId swapReceiver @@== initialBalanceSwapReceiverX
        balanceOf yToken yTokenId swapReceiver @@== initialBalanceSwapReceiverY + fromIntegral @Integer @Natural expectedDy

      -- `feeReceiver` receives the expected fees.
      collectAllFees cfmm feeReceiver
      balanceOf yToken yTokenId feeReceiver @@== 0
      let expectedFees =
            swaps
            <&> (\dx -> calcSwapFee feeBps dx)
            & sum
      -- `update_position` rounds the fee down, so it's possible 1 X token is lost.
      receivedFee <- balanceOf xToken xTokenId feeReceiver
      receivedFee `isInRangeNat` expectedFees $ (1, 0)

test_many_small_swaps :: TestTree
test_many_small_swaps =
  nettestScenarioOnEmulatorCaps "placing many small swaps is (mostly) equivalent to placing 1 big swap" do
    -- Note that this property only holds in the absence of fees.
    -- When there _is_ a fee and a user swaps a very small amount of tokens,
    -- the fee is rounded up to 1 token.
    -- Over the course of many small swaps, this effect compounds and ends up
    -- making a big difference.
    let feeBps = 0

    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    let swapCount = 1_000
    let swapAmount = 10

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto

    let accounts = [liquidityProvider, swapper]
    tokens@(TokenInfo xTokenId xToken, TokenInfo yTokenId yToken) <- forEach (FA2.TokenId 0, FA2.TokenId 1) $ originateFA2 accounts
    (cfmm1, _) <- prepareSomeSegCFMM' accounts (Just tokens) Nothing (set cFeeBpsL feeBps)
    (cfmm2, _) <- prepareSomeSegCFMM' accounts (Just tokens) Nothing (set cFeeBpsL feeBps)

    -- Add some slots to the buffers to make the tests more meaningful.
    for_ [cfmm1, cfmm2] \cfmm -> call cfmm (Call @"Increase_observation_count") 10

    withSender liquidityProvider do
      for_ [cfmm1, cfmm2] \cfmm -> do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    withSender swapper $ do
      -- 1 big swap
      call cfmm1 (Call @"X_to_y") XToYParam
        { xpDx = (swapCount * swapAmount)
        , xpDeadline = validDeadline
        , xpMinDy = 0
        , xpToDy = swapper
        }

      -- many small swaps
      replicateM_ (fromIntegral swapCount) do
        call cfmm2 (Call @"X_to_y") XToYParam
          { xpDx = swapAmount
          , xpDeadline = validDeadline
          , xpMinDy = 0
          , xpToDy = swapper
          }

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
    sCurTickIndex st1 @== sCurTickIndex st2
    st1 { sSqrtPrice = sqrtPrice1 } @== st2 { sSqrtPrice = sqrtPrice2 }

    -- Due to `dy` being rounded down, it's possible the swapper loses *up to* 1 Y token
    -- on every swap.
    -- So the 2nd contract may hold up to 1000 more Y tokens than the 1st contract.
    cfmm1YBalance <- balanceOf yToken yTokenId cfmm1
    cfmm2YBalance <- balanceOf yToken yTokenId cfmm2
    cfmm2YBalance `isInRangeNat` cfmm1YBalance $ (0, swapCount)

    -- The two contracts should hold the same exact amount of X tokens
    cfmm1XBalance <- balanceOf xToken xTokenId cfmm1
    cfmm2XBalance <- balanceOf xToken xTokenId cfmm2
    cfmm1XBalance @== cfmm2XBalance


test_crossing_ticks :: TestTree
test_crossing_ticks =
  nettestScenarioOnEmulatorCaps "executing a swap within a single tick range or across many ticks should be (mostly) equivalent" do
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
    tokens@(TokenInfo xTokenId xToken, TokenInfo yTokenId yToken) <- forEach (FA2.TokenId 0, FA2.TokenId 1) $ originateFA2 accounts
    (cfmm1, _) <- prepareSomeSegCFMM' accounts (Just tokens) Nothing (set cFeeBpsL feeBps)
    (cfmm2, _) <- prepareSomeSegCFMM' accounts (Just tokens) Nothing (set cFeeBpsL feeBps)

    -- Add some slots to the buffers to make the tests more meaningful.
    for_ [cfmm1, cfmm2] \cfmm -> call cfmm (Call @"Increase_observation_count") 10

    withSender liquidityProvider do
      -- Place 1 big position
      call cfmm1 (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidity
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
          }

      -- Place many small positions with the same liquidity
      let positionSize = 100
      for_ [lowerTickIndex, lowerTickIndex + positionSize .. upperTickIndex - positionSize] \lowerTickIndex' -> do
        call cfmm2 (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex'
            , sppUpperTickIndex = lowerTickIndex' + positionSize
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    cfmm1InitialBalanceX <- balanceOf xToken xTokenId cfmm1
    cfmm1InitialBalanceY <- balanceOf yToken yTokenId cfmm1
    cfmm2InitialBalanceX <- balanceOf xToken xTokenId cfmm2
    cfmm2InitialBalanceY <- balanceOf yToken yTokenId cfmm2

    -- Place a small swap to move the tick past 0 and advance the time to fill the
    -- buffer with _something_ other than zeros.
    withSender swapper do
      for_ [cfmm1, cfmm2] \cfmm -> do
        call cfmm (Call @"X_to_y") XToYParam
          { xpDx = 200
          , xpDeadline = validDeadline
          , xpMinDy = 0
          , xpToDy = swapper
          }
    advanceSecs waitTime [cfmm1, cfmm2]


    -- Place 1 big swap to push the tick all the way down to `lowerTickIndex`
    initialSt2 <- getFullStorage cfmm2
    withSender swapper do
      for_ [cfmm1, cfmm2] \cfmm -> do
        call cfmm (Call @"X_to_y") XToYParam
          { xpDx = 50_000
          , xpDeadline = validDeadline
          , xpMinDy = 0
          , xpToDy = swapper
          }

    -- Advance the time 1 sec to make sure the buffer is updated to reflect the swaps.
    advanceSecs 1 [cfmm1, cfmm2]
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    st1 <- getFullStorage cfmm1
    st2 <- getFullStorage cfmm2

    -- Sanity check: In order for this test to be meaningful, we need the `curTickIndex`
    -- to have moved close to `lowerTickIndex` and have crossed several initialized ticks.
    sCurTickIndex st1 `isInRange` lowerTickIndex $ (0, 50)

    -- Current tick should be the same.
    sCurTickIndex st1 @== sCurTickIndex st2

    -- "Fee growth" should be fairly similar.
    -- It can be slightly higher for the 2nd contract,
    -- because each time we cross an initialized tick, the fee can be rounded up once.
    -- Because in the 2nd scenario we're crossing 10 ticks, we allow for a difference of up to 10 extra X tokens in fees.
    let PerToken feeGrowthX1 feeGrowthY1 = sFeeGrowth st1
    let PerToken feeGrowthX2 feeGrowthY2 = sFeeGrowth st2
    feeGrowthY1 @== 0
    feeGrowthY2 @== 0
    let marginOfError = pickX (mkX @_ @128 10) `div` liquidity
    feeGrowthX2 `isInRangeNat` feeGrowthX1 $ (0, marginOfError)


    let calcBalanceDelta initial final = fromIntegral @Natural @Integer final - fromIntegral @Natural @Integer initial
    cfmm1BalanceDeltaX <- balanceOf xToken xTokenId cfmm1 <&> calcBalanceDelta cfmm1InitialBalanceX
    cfmm1BalanceDeltaY <- balanceOf yToken yTokenId cfmm1 <&> calcBalanceDelta cfmm1InitialBalanceY
    cfmm2BalanceDeltaX <- balanceOf xToken xTokenId cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceX
    cfmm2BalanceDeltaY <- balanceOf yToken yTokenId cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceY
    -- The two contract should have received the exact same amount of X tokens
    cfmm1BalanceDeltaX @== cfmm2BalanceDeltaX
    -- The 2nd contract may have given out fewer Y tokens (due to the potential increase in fees)
    cfmm2BalanceDeltaY `isInRange` cfmm1BalanceDeltaY $ (0, 10)

    -- Collected fees should be fairly similar.
    -- As explained above, the contract may charge up to 10 extra tokens.
    -- However, when an LP collects fees for a position, the distribution of fees can be rounded down,
    -- so we allow for a margin of error of +/-10 X tokens.
    collectAllFees cfmm1 feeReceiver1
    collectAllFees cfmm2 feeReceiver2
    balanceOf yToken yTokenId feeReceiver1 @@== 0
    balanceOf yToken yTokenId feeReceiver2 @@== 0
    feeReceiver1BalanceX <- balanceOf xToken xTokenId feeReceiver1
    feeReceiver2BalanceX <- balanceOf xToken xTokenId feeReceiver2
    feeReceiver2BalanceX `isInRangeNat` feeReceiver1BalanceX $ (10, 10)

    -- The global accumulators of both contracts should be the same.
    sCumulativesBuffer st1 @== sCumulativesBuffer st2

    -- Check that the ticks' states were updated correctly after being crossed.
    let crossedTicks = [-900, -800 .. -100] <&> \idx -> bmMap (sTicks st2) ! idx
    for_ crossedTicks \ts -> do
      tsSecondsPerLiquidityOutside ts @== mkX waitTime `div` X liquidity
      tsSecondsOutside ts @== waitTime
      tsTickCumulativeOutside ts @== fromIntegral @TickIndex @Integer (sCurTickIndex initialSt2) * fromIntegral waitTime
      tsFeeGrowthOutside ts @/= 0

test_fee_split :: TestTree
test_fee_split =
  nettestScenarioOnEmulatorCaps "fees are correctly assigned to each position" do
    let feeBps = 50_00 -- 50%

    let liquidityDelta = 1_e6

    liquidityProvider <- newAddress auto
    let position1Bounds = (-100, 100)
    let position2Bounds = (-200, -100)

    swapper <- newAddress auto
    feeReceiver1 <- newAddress auto
    feeReceiver2 <- newAddress auto
    (cfmm, (TokenInfo xTokenId xToken, TokenInfo yTokenId yToken)) <- prepareSomeSegCFMM' [liquidityProvider, swapper] Nothing Nothing (set cFeeBpsL feeBps)

    withSender liquidityProvider do
      for_ [position1Bounds, position2Bounds] \(lowerTickIndex, upperTickIndex) -> do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidityDelta
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }

    withSender swapper do
      -- Place a small y-to-x swap.
      -- It's small enough to be executed within the [-100, 100] range,
      -- so the Y fee is paid to position1 only.
      call cfmm (Call @"Y_to_x") YToXParam
        { ypDy = 1_000
        , ypDeadline = validDeadline
        , ypMinDx = 0
        , ypToDx = swapper
        }

      -- Place a big x-to-y swap.
      -- It's big enough to cross from the [-100, 100] range into the [-200, -100] range,
      -- so the X fee is paid to both position1 and position2.
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 20_000
        , xpDeadline = validDeadline
        , xpMinDy = 0
        , xpToDy = swapper
        }
    checkAllInvariants cfmm

    -- position1 should have earned both X and Y fees.
    collectFees cfmm feeReceiver1 0 liquidityProvider
    balanceOf xToken xTokenId feeReceiver1 @@/= 0
    balanceOf yToken yTokenId feeReceiver1 @@/= 0

    -- position2 should have earned X fees only.
    collectFees cfmm feeReceiver2 1 liquidityProvider
    balanceOf xToken xTokenId feeReceiver2 @@/= 0
    balanceOf yToken yTokenId feeReceiver2 @@== 0

test_must_exceed_min_dy :: TestTree
test_must_exceed_min_dy =
  nettestScenarioOnEmulatorCaps "swap fails if the user would receiver less than min_dy" do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper]

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidity
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
          }

    withSender swapper do
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 1
        , xpDeadline = validDeadline
        , xpMinDy = 1000
        , xpToDy = swapper
        }
        & expectFailedWith smallerThanMinAssetErr

test_fails_if_its_past_the_deadline :: TestTree
test_fails_if_its_past_the_deadline =
  nettestScenarioOnEmulatorCaps "swap fails if it's past the deadline" do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper]

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidity
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
          }

    withSender swapper do
      now <- getNow
      let expiredDeadline = now `timestampPlusSeconds` -1
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 1
        , xpDeadline = expiredDeadline
        , xpMinDy = 0
        , xpToDy = swapper
        }
        & expectFailedWith pastDeadlineErr

test_swaps_are_noops_when_liquidity_is_zero :: TestTree
test_swaps_are_noops_when_liquidity_is_zero =
  nettestScenarioOnEmulatorCaps "After crossing into a 0-liquidity range, swaps are no-ops" do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    (cfmm, (TokenInfo xTokenId xToken, TokenInfo yTokenId yToken)) <- prepareSomeSegCFMM [liquidityProvider, swapper]

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = -100
          , sppUpperTickIndex = 100
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 10_000
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
          }

    withSender swapper do
      -- Place a swap big enough to exhaust the position's liquidity
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 200
        , xpDeadline = validDeadline
        , xpMinDy = 0
        , xpToDy = swapper
        }

      let
        isNoOp op = do
          initialSt <- getFullStorage cfmm
          initialBalanceX <- balanceOf xToken xTokenId cfmm
          initialBalanceY <- balanceOf yToken yTokenId cfmm
          op
          getFullStorage cfmm @@== initialSt
          balanceOf xToken xTokenId cfmm @@== initialBalanceX
          balanceOf yToken yTokenId cfmm @@== initialBalanceY

      isNoOp $ call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 100
        , xpDeadline = validDeadline
        , xpMinDy = 0
        , xpToDy = swapper
        }
      isNoOp $ call cfmm (Call @"Y_to_x") YToXParam
        { ypDy = 100
        , ypDeadline = validDeadline
        , ypMinDx = 0
        , ypToDx = swapper
        }

test_push_cur_tick_index_just_below_witness :: TestTree
test_push_cur_tick_index_just_below_witness =
  nettestScenarioOnEmulatorCaps "invariants hold when pushing the cur_tick_index just below cur_tick_witness" do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      (cfmm, _) <- prepareSomeSegCFMM' [liquidityProvider, swapper] Nothing Nothing (set cFeeBpsL 200)

      withSender liquidityProvider do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = -100
            , sppUpperTickIndex = 100
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = 10000
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = -200
            , sppUpperTickIndex = -100
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = 30000
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
            }

      withSender swapper do
        -- Explanation:
        -- We have 2 positions: one currently in-range with boundaries at [-100, 100],
        -- and another currently out-of-range with boundaries at [-200, -100].
        --
        -- If we deposit 52 X tokens, the cur_tick_index would move to -100 but NOT cross it.
        --
        -- If we deposit 53 X tokens, we'll exhaust the first position's liquidity,
        -- and therefore cross the tick -100.
        -- After having crossed the tick, we'll have 1 X token left to swap.
        -- But since a 1 token fee will be charged, 0 X tokens will be
        -- deposited and 0 Y tokens will be withdrawn.
        --
        -- We want to make sure invariants are not broken when this edge case occurs.
        call cfmm (Call @"X_to_y") XToYParam
          { xpDx = 53
          , xpDeadline = validDeadline
          , xpMinDy = 0
          , xpToDy = swapper
          }

        -- sanity check
        st <- getFullStorage cfmm
        sCurTickIndex st @== -101

        checkAllInvariants cfmm
