-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.ProtocolFee.XToY
  ( test_swapping_within_a_single_tick_range
  , test_many_small_swaps
  , test_crossing_ticks
  , test_fee_split
  , test_must_exceed_min_dy
  , test_fails_if_its_past_the_deadline
  , test_swaps_are_noops_when_liquidity_is_zero
  , test_push_cur_tick_index_just_below_witness
  , test_protocol_fees_are_burned
  ) where

import Prelude

import Data.Map (fromList, (!))
import Fmt (Buildable(build), indentF, unlinesF)
import Hedgehog hiding (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Indigo.Contracts.ManagedLedger as FA1_2
import Lorentz hiding (assert, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.ManagedLedgerInterface as FA1_2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Tezos.Core (timestampPlusSeconds)
import Util.Named ((.!))

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.SegCFMM.Contract (TokenType(..))
import Test.SegCFMM.Storage (defaultStorage)
import Test.Util

-- TODO TCFMM-41: all tests below are duplicate of the ones in 'Test.XToY'.
-- The difference is only in the use of an FA1.2 contract and as a consequence
-- in the utility functions used.
-- These tests should be merged (with the missing combinations as well).

mkStorage
  :: ContractHandler FA2.FA2SampleParameter FA2.Storage -> FA2.TokenId
  -> ContractHandler FA1_2.Parameter FA1_2.Storage -> FA2.TokenId
  -> Natural
  -> Natural
  -> Storage
mkStorage xToken xTokenId yToken yTokenId feeBps ctezBurnFeeBps =
  defaultStorage
    { sConstants = (sConstants defaultStorage)
      { cXTokenAddress = toAddress xToken
      , cXTokenId = xTokenId
      , cYTokenAddress = toAddress yToken
      , cYTokenId = yTokenId
      , cFeeBps = feeBps
      , cCtezBurnFeeBps = ctezBurnFeeBps
      }
    }

-- | Retrieve the FA2 balance for a given account and token.
getFA12Balance
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr, FA1_2.ParameterC param)
  => ContractHandler param storage -> addr -> m Natural
getFA12Balance fa1_2 account = do
  consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @Natural)
  call fa1_2 (Call @"GetBalance") (mkView (#owner .! toAddress account) consumer)
  getStorage consumer >>= \case
    [bal] -> pure bal
    consumerStorage -> failure $ unlinesF
      [ "Expected consumer storage to have exactly 1 balance response."
      , "Consumer storage:"
      , indentF 2 $ build consumerStorage
      ]

test_swapping_within_a_single_tick_range :: TestTree
test_swapping_within_a_single_tick_range =
  testProperty "swapping within a single tick range" $ property do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000
    let userTokenBalance = 1_e15

    -- With the liquidity above, we can deposit a little more than 500_000 Y tokens.
    -- So we'll generate up to 10 swaps of 50_000 tokens each.
    swaps <- forAll $
      Gen.list (Range.linear 1 10) $
        Gen.integral (Range.linear 0 50_000)

    feeBps <- forAll $ Gen.integral (Range.linear 0 10_000)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 10_000)

    clevelandProp do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      swapReceiver <- newAddress auto
      feeReceiver <- newAddress auto
      let accounts = [liquidityProvider, swapper]

      let xTokenId = FA2.TokenId 0
      let yTokenId = FA2.TokenId 0 -- doesn't actually matter
      let xFa2storage = FA2.Storage
            { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      ctezAdmin <- newAddress auto
      let yCtezStorage = FA1_2.mkStorage ctezAdmin $
            fromList $ accounts <&> (, userTokenBalance)
      xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
      yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

      cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps
      -- Add some slots to the buffers to make the tests more meaningful.
      call cfmm (Call @"Increase_observation_count") 10

      for_ accounts \account ->
        withSender account do
          call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
          call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

      withSender liquidityProvider do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
            }
        checkAllInvariants cfmm

      for_ swaps \dx -> do
        initialSt <- getFullStorage cfmm
        initialBalanceSwapperX <- balanceOf xToken xTokenId swapper
        initialBalanceSwapperY <- getFA12Balance yToken swapper
        initialBalanceSwapReceiverX <- balanceOf xToken xTokenId swapReceiver
        initialBalanceSwapReceiverY <- getFA12Balance yToken swapReceiver

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
        -- Note: because protocol fees are subtracted after conversion, their
        -- presence does not affect swap fees nor price variations.
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
        let expectedDy = receivedY' (sSqrtPrice initialSt) (sSqrtPrice finalSt) (sLiquidity initialSt) protoFeeBps
        balanceOf xToken xTokenId swapper @@== initialBalanceSwapperX - dx
        getFA12Balance yToken swapper @@== initialBalanceSwapperY
        -- The right amount of tokens was sent to the `receiver`.
        balanceOf xToken xTokenId swapReceiver @@== initialBalanceSwapReceiverX
        getFA12Balance yToken swapReceiver @@== initialBalanceSwapReceiverY + fromIntegral @Integer @Natural expectedDy

      -- `feeReceiver` receives the expected fees.
      collectAllFees cfmm feeReceiver
      getFA12Balance yToken feeReceiver @@== 0
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
    -- Note: this is true for protocol fees as well.
    let liquidity = 1_e7
    let userTokenBalance = 1_e15
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    let swapCount = 1_000
    let swapAmount = 10

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm1 <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId 0 0
    cfmm2 <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId 0 0
    -- Add some slots to the buffers to make the tests more meaningful.
    for_ [cfmm1, cfmm2] \cfmm -> call cfmm (Call @"Increase_observation_count") 10

    for_ accounts \account ->
      for_ [cfmm1, cfmm2] \cfmm ->
        withSender account do
          call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
          call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

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
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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
      for_ (genericReplicate swapCount swapAmount) \dx ->
        call cfmm2 (Call @"X_to_y") XToYParam
          { xpDx = dx
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
    cfmm1YBalance <- getFA12Balance yToken cfmm1
    cfmm2YBalance <- getFA12Balance yToken cfmm2
    cfmm2YBalance `isInRangeNat` cfmm1YBalance $ (0, swapCount)

    -- The two contracts should hold the same exact amount of X tokens
    cfmm1XBalance <- balanceOf xToken xTokenId cfmm1
    cfmm2XBalance <- balanceOf xToken xTokenId cfmm2
    cfmm1XBalance @== cfmm2XBalance


test_crossing_ticks :: TestTree
test_crossing_ticks =
  nettestScenarioOnEmulatorCaps "executing a swap within a single tick range or across many ticks should be (mostly) equivalent" do
    let feeBps = 200 -- 2%
    let protoFeeBps = 100 -- 1%

    -- The number of seconds to wait before executing the swap
    let waitTime = 3

    let liquidity = 1_e6
    let userTokenBalance = 1_e15
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    feeReceiver1 <- newAddress auto
    feeReceiver2 <- newAddress auto
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm1 <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps
    cfmm2 <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps
    -- Add some slots to the buffers to make the tests more meaningful.
    for_ [cfmm1, cfmm2] \cfmm -> call cfmm (Call @"Increase_observation_count") 10

    for_ accounts \account ->
      for_ [cfmm1, cfmm2] \cfmm ->
        withSender account do
          call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
          call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

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
          , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
            }
    checkAllInvariants cfmm1
    checkAllInvariants cfmm2

    cfmm1InitialBalanceX <- balanceOf xToken xTokenId cfmm1
    cfmm1InitialBalanceY <- getFA12Balance yToken cfmm1
    cfmm2InitialBalanceX <- balanceOf xToken xTokenId cfmm2
    cfmm2InitialBalanceY <- getFA12Balance yToken cfmm2

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
    cfmm1BalanceDeltaY <- getFA12Balance yToken cfmm1 <&> calcBalanceDelta cfmm1InitialBalanceY
    cfmm2BalanceDeltaX <- balanceOf xToken xTokenId cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceX
    cfmm2BalanceDeltaY <- getFA12Balance yToken cfmm2 <&> calcBalanceDelta cfmm2InitialBalanceY
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
    getFA12Balance yToken feeReceiver1 @@== 0
    getFA12Balance yToken feeReceiver2 @@== 0
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
    let feeBps = 5000 -- 50%
    let protoFeeBps = 1000 -- 10%

    let liquidityDelta = 1_e6
    let userTokenBalance = 1_e15

    liquidityProvider <- newAddress auto
    let position1Bounds = (-100, 100)
    let position2Bounds = (-200, -100)

    swapper <- newAddress auto
    feeReceiver1 <- newAddress auto
    feeReceiver2 <- newAddress auto
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

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
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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
    getFA12Balance yToken feeReceiver1 @@/= 0

    -- position2 should have earned X fees only.
    collectFees cfmm feeReceiver2 1 liquidityProvider
    balanceOf xToken xTokenId feeReceiver2 @@/= 0
    getFA12Balance yToken feeReceiver2 @@== 0

test_must_exceed_min_dy :: TestTree
test_must_exceed_min_dy =
  nettestScenarioOnEmulatorCaps "swap fails if the user would receiver less than min_dy" do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000
    let userTokenBalance = 1_e15
    let feeBps = 100
    let protoFeeBps = 100

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let accounts = [liquidityProvider, swapper]

    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidity
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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
    let userTokenBalance = 1_e15
    let feeBps = 100
    let protoFeeBps = 100

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let accounts = [liquidityProvider, swapper]

    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidity
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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
    let userTokenBalance = 1_e15
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId 200 100
    checkAllInvariants cfmm

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = -100
          , sppUpperTickIndex = 100
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 10_000
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
          }

    withSender swapper do
      -- Place a swpa big enough to exhaust the position's liquidity
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
          initialBalanceY <- getFA12Balance yToken cfmm
          op
          getFullStorage cfmm @@== initialSt
          balanceOf xToken xTokenId cfmm @@== initialBalanceX
          getFA12Balance yToken cfmm @@== initialBalanceY

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
      let userTokenBalance = 1_e15
      let accounts = [liquidityProvider, swapper]
      let xTokenId = FA2.TokenId 0
      let yTokenId = FA2.TokenId 0 -- doesn't actually matter
      let xFa2storage = FA2.Storage
            { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      ctezAdmin <- newAddress auto
      let yCtezStorage = FA1_2.mkStorage ctezAdmin $
            fromList $ accounts <&> (, userTokenBalance)
      xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
      yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

      cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId 200 100
      checkAllInvariants cfmm

      for_ accounts \account ->
        withSender account do
          call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
          call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

      withSender liquidityProvider do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = -100
            , sppUpperTickIndex = 100
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = 10000
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
            }
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = -200
            , sppUpperTickIndex = -100
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = 30000
            , sppDeadline = validDeadline
            , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
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

test_protocol_fees_are_burned :: TestTree
test_protocol_fees_are_burned =
  nettestScenarioOnEmulatorCaps "protocol fees are effectively burned" do
    let feeBps = 0
    let protoFeeBps = 5_000 -- 50%

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let userTokenBalance = 1_e15
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 0 -- doesn't actually matter
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userTokenBalance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    ctezAdmin <- newAddress auto
    let yCtezStorage = FA1_2.mkStorage ctezAdmin $
          fromList $ accounts <&> (, userTokenBalance)
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "ctez" yCtezStorage FA1_2.managedLedgerContract

    cfmm <- originateSegCFMM FA2 CTEZ $ mkStorage xToken xTokenId yToken yTokenId feeBps protoFeeBps
    checkAllInvariants cfmm

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Approve") (#spender .! (toAddress cfmm), #value .! userTokenBalance)

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = -100
          , sppUpperTickIndex = 100
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 10_000
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken userTokenBalance userTokenBalance
          }

    -- The cfmm contract has a non-zero initial CTEZ balance
    cfmmBalance0 <- getFA12Balance yToken cfmm
    checkCompares cfmmBalance0 (>=) 1

    -- Perform a swap that does not exhaust the position's liquidity
    withSender swapper do
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 10
        , xpDeadline = validDeadline
        , xpMinDy = 1
        , xpToDy = swapper
        }

    -- The cfmm contract still has a CTEZ balance that we can use to make a swap
    -- Note: the reason why at least 4 tokens are required here is simply because
    -- otherwise the test might be invalidated by the rounding involved
    cfmmBalance1 <- getFA12Balance yToken cfmm
    checkCompares cfmmBalance1 (>=) 4

    -- This swap would exhaust the position liquidity and take ~50% of the
    -- remaining CTEZ balance, but it will fail because some of that balance are
    -- protocol fees put aside from the previous swaps.
    -- Note: this would have succeeded if the previous swap didn't happen.
    withSender swapper do
     call cfmm (Call @"X_to_y") XToYParam
       { xpDx = 100
       , xpDeadline = validDeadline
       , xpMinDy = (cfmmBalance1 - 1) `div` 2
       , xpToDy = swapper
       } & expectFailedWith smallerThanMinAssetErr

    -- Trying to only exhaust the position liquidity however is still possible
    withSender swapper do
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 100
        , xpDeadline = validDeadline
        , xpMinDy = 1
        , xpToDy = swapper
        }

    -- The cfmm contract now has ~50% of the initial balance...
    cfmmBalance2 <- getFA12Balance yToken cfmm
    checkCompares cfmmBalance2 (>=) (cfmmBalance0 `div` 2)

    -- ... none of which can be swapped, as it's all protocol fees
    withSender swapper do
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 100
        , xpDeadline = validDeadline
        , xpMinDy = 1
        , xpToDy = swapper
        } & expectFailedWith smallerThanMinAssetErr
