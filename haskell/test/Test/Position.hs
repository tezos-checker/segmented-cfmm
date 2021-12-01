-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Position where

import Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import Hedgehog hiding (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lorentz hiding (assert, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Micheline (toExpression)
import Morley.Nettest
import Morley.Nettest.Abstract (TransferFailure(FailedWith))
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Tezos.Core (timestampPlusSeconds)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.SegCFMM.Contract
import Test.Util
import Util.Named

-- | Cumulatives buffer after some calls to contract that do not
-- change \"much\".
cumulativesBuffer1 :: Timestamp -> CumulativesBuffer
cumulativesBuffer1 now =
  let initVal = initCumulativesBuffer 0
  in initVal
    { cbFirst = 1, cbLast = 1
    , cbMap = mkBigMap $ one
        ( 1
        , initTimedCumulatives{ tcTime = now }
        )
    }

test_equal_ticks :: TestTree
test_equal_ticks =
  forAllTokenTypeCombinations "setting a position with lower_tick=upper_tick fails" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider $ setPosition cfmm 1 (100, 100)
      & expectFailedWith tickOrderErr

test_wrong_tick_order :: TestTree
test_wrong_tick_order =
  forAllTokenTypeCombinations "setting a position with lower_tick>upper_tick fails" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider $ setPosition cfmm 1 (100, 99)
      & expectFailedWith tickOrderErr

test_setting_a_position_with_zero_liquidity_is_a_noop :: TestTree
test_setting_a_position_with_zero_liquidity_is_a_noop =
  forAllTokenTypeCombinations "setting a position with zero liquidity is a no-op" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, tokens) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def
    balanceConsumers <- originateBalanceConsumers tokens
    initialSt <- getFullStorage cfmm

    withSender liquidityProvider $ setPosition cfmm 0 (-100, 100)

    -- The storage shouldn't have changed (with few exceptions).
    now <- getNow
    getFullStorage cfmm @@== initialSt
      { sCumulativesBuffer = cumulativesBuffer1 now
      }
    balancesOf balanceConsumers cfmm @@== (0, 0)

test_deposit_and_withdrawal_is_a_noop :: TestTree
test_deposit_and_withdrawal_is_a_noop =
  forAllTokenTypeCombinations "depositing and withdrawing the same amount of liquidity is a no-op" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, tokens) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def
    balanceConsumers <- originateBalanceConsumers tokens
    initialSt <- getFullStorage cfmm

    withSender liquidityProvider do
      setPosition cfmm 1_e7 (-10, 15)
      updatePosition cfmm liquidityProvider -1_e7 0

    -- The storage shouldn't have changed (with few exceptions).
    now <- getNow
    getFullStorage cfmm @@== initialSt
      { sNewPositionId = sNewPositionId initialSt + 1
      , sCumulativesBuffer = cumulativesBuffer1 now
      }
    -- The contract's balance should be 0.
    -- There is a margin of error, so the contract may end up with at most 1 token.
    (xBalance, yBalance) <- balancesOf balanceConsumers cfmm
    checkCompares xBalance elem [0, 1]
    checkCompares yBalance elem [0, 1]

test_adding_liquidity_twice :: TestTree
test_adding_liquidity_twice =
  forAllTokenTypeCombinations "adding liquidity twice is the same as adding it once" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) $ do
    let liquidityDelta = 1_e5
    liquidityProvider <- newAddress auto
    let accounts = [liquidityProvider]
    tokens <- originateTokenContracts accounts ((fst tokenTypes, FA2.TokenId 0), (snd tokenTypes, FA2.TokenId 1))
    (cfmm1, _) <- prepareSomeSegCFMM accounts tokenTypes def { opTokens = Just tokens }
    (cfmm2, _) <- prepareSomeSegCFMM accounts tokenTypes def { opTokens = Just tokens }
    balanceConsumers <- originateBalanceConsumers tokens

    withSender liquidityProvider do
      -- Add liquidity twice to cfmm1
      setPosition cfmm1 liquidityDelta (-25, 15)
      updatePosition cfmm1 liquidityProvider (toInteger liquidityDelta) 0
      -- Add twice the liquidity once to cfmm2
      setPosition cfmm2 (2 * liquidityDelta) (-25, 15)

    -- The two contracts should have the same storage and the same balance.
    -- There may be a -/+1 margin of the error in the balance calculations.
    st1 <- getFullStorage cfmm1
    st2 <- getFullStorage cfmm2
    st1 @== st2
    ((cfmm1XBalance, cfmm2XBalance), (cfmm1YBalance, cfmm2YBalance)) <- balancesOfMany balanceConsumers (cfmm1, cfmm2)
    cfmm2XBalance `isInRangeNat` cfmm1XBalance $ (1, 1)
    cfmm2YBalance `isInRangeNat` cfmm1YBalance $ (1, 1)

test_witnesses_must_be_valid :: TestTree
test_witnesses_must_be_valid =
  forAllTokenTypeCombinations "witnesses must be valid" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider do
      -- lower_tick_witness has not been initialized
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex + 1
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith tickNotExistErr

      -- upper_tick_witness has not been initialized
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex + 1
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith tickNotExistErr

      -- lower_tick_witness has been initialized, but is greater than the lower_tick
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = maxTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith invalidWitnessErr

      -- upper_tick_witness has been initialized, but is greater than the upper_tick
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = maxTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith invalidWitnessErr

test_fails_if_its_past_the_deadline :: TestTree
test_fails_if_its_past_the_deadline =
  forAllTokenTypeCombinations "fails if it's past the deadline" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15

    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider do
      now <- getNow
      let expiredDeadline = now `timestampPlusSeconds` -1
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = expiredDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith (pastDeadlineErr (#deadline .! expiredDeadline, #executed_at .! now))

      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = now
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }

      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = toInteger liquidityDelta
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = expiredDeadline
          , uppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith (pastDeadlineErr (#deadline .! expiredDeadline, #executed_at .! now))

test_fails_if_its_not_multiple_tick_spacing :: TestTree
test_fails_if_its_not_multiple_tick_spacing =
  nettestScenarioCaps "fails if a tick index is not a multiple of 'tick_spacing'" do
    let liquidityDelta = 10000000

    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] defaultTokenTypes def { opModifyConstants = set cTickSpacingL 10 }

    withSender liquidityProvider do
      let lowerTickIndex = -10
      let upperTickIndex = 20
      let invalidLowerTickIndex = -9
      let invalidUpperTickIndex = 11
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = invalidLowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith incorrectTickSpacingErr

      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = invalidUpperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith incorrectTickSpacingErr

      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }

test_cannot_set_position_over_max_tick :: TestTree
test_cannot_set_position_over_max_tick =
  forAllTokenTypeCombinations "cannot set a position with upper_tick > max_tick" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do

    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider $ setPosition cfmm 1 (-10, maxTickIndex + 1)
      & expectFailedWith tickNotExistErr

test_maximum_tokens_contributed :: TestTree
test_maximum_tokens_contributed =
  forAllTokenTypeCombinations "cannot transfer more than maximum_tokens_contributed" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = -10
          , sppUpperTickIndex = 10
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 10000000
          , sppDeadline = validDeadline
          , sppMaximumTokensContributed = PerToken 1 1
          }
          & expectFailedWith (highTokensErr (#max .! 1, #actual 4999))

      -- Also check "Update_position"
      setPosition cfmm 1 (-10, 10)
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = 10000000
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = validDeadline
          , uppMaximumTokensContributed = PerToken 1 1
          }
          & expectFailedWith (highTokensErr (#max .! 1, #actual 4999))

test_lowest_and_highest_ticks_cannot_be_garbage_collected :: TestTree
test_lowest_and_highest_ticks_cannot_be_garbage_collected =
  forAllTokenTypeCombinations "lowest and highest ticks cannot be garbage collected" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def
    initialSt <- getFullStorage cfmm

    withSender liquidityProvider do
      setPosition cfmm 1 (minTickIndex, maxTickIndex)
      updatePosition cfmm liquidityProvider (-1) 0

    -- The storage shouldn't have changed (with few exceptions).
    now <- getNow
    getFullStorage cfmm @@== initialSt
      { sNewPositionId = sNewPositionId initialSt + 1
      , sCumulativesBuffer = cumulativesBuffer1 now
      }

test_withdrawal_overflow :: TestTree
test_withdrawal_overflow =
  forAllTokenTypeCombinations "cannot withdraw more liquidity from a position than it currently has" \tokenTypes ->
  nettestScenarioOnNetworkCaps (show tokenTypes) do
    let liquidityDelta = 10_000
    let lowerTickIndex = -10
    let upperTickIndex = 10

    liquidityProvider1 <- newAddress auto
    liquidityProvider2 <- newAddress auto
    transferMoney liquidityProvider1 10_e6
    transferMoney liquidityProvider2 10_e6
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider1, liquidityProvider2] tokenTypes def

    -- Add some liquidity with `liquidityProvider1`
    withSender liquidityProvider1 $ setPosition cfmm liquidityDelta (lowerTickIndex, upperTickIndex)

    -- Add some liquidity with `liquidityProvider2`,
    -- and then attempt to remove more than what was added.
    --
    -- There should still be some liquidity left in the contract (thanks to `liquidityProvider1`),
    -- but the position's liquidity should drop below 0 (and fail).
    withSender liquidityProvider2 do
      setPosition cfmm liquidityDelta (lowerTickIndex, upperTickIndex)
      updatePosition cfmm liquidityProvider2 (-(toInteger liquidityDelta) - 1) 1
        & expectFailedWith positionLiquidityBelowZeroErr

test_LPs_get_fees :: TestTree
test_LPs_get_fees =
  propOnNetwork "Liquidity Providers earn fees from swaps"
    do
      tokenTypes <- forAll $ Gen.element allTokenTypeCombinations
      feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData
      pure (tokenTypes, feeBps, protoFeeBps, swaps)
    ( defaultTokenTypes
    , 20_00
    , 7_00
    , [SwapData XToY 1000, SwapData YToX 3000, SwapData XToY 400]
    )
    \(tokenTypes, feeBps, protoFeeBps, swaps) -> do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      feeReceiver <- newAddress auto
      transferMoney liquidityProvider 10_e6
      (cfmm, tokens) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens

      withSender liquidityProvider $ setPosition cfmm 1_e7 (-10000, 10000)

      (xFees, yFees) <- fmap unzip .
        withSender swapper $ inBatch do
          for swaps \(SwapData swapDirection swapAmt) -> do
            case swapDirection of
              XToY -> do
                xtoy cfmm swapAmt swapper
                pure $ (calcSwapFee feeBps swapAmt, 0)
              YToX -> do
                ytox cfmm swapAmt swapper
                pure $ (0, calcSwapFee feeBps swapAmt)

      collectFees cfmm feeReceiver 0 liquidityProvider
      (feeReceiverBalanceX, feeReceiverBalanceY) <- balancesOf balanceConsumers feeReceiver

      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      feeReceiverBalanceX `isInRangeNat` (sum xFees) $ (1, 0)
      feeReceiverBalanceY `isInRangeNat` (sum yFees) $ (1, 0)

test_fees_are_proportional_to_liquidity :: TestTree
test_fees_are_proportional_to_liquidity =
  forAllTokenTypeCombinations "Liquidity Providers earn fees proportional to their liquidity" \tokenTypes ->
  testProperty (show tokenTypes) $ property do

    let position1Liquidity = 1_e7
    let position2Liquidity = position1Liquidity * 3

    feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData

    clevelandProp do
      liquidityProvider1 <- newAddress auto
      liquidityProvider2 <- newAddress auto

      swapper <- newAddress auto
      feeReceiver1 <- newAddress auto
      feeReceiver2 <- newAddress auto
      let accounts = [liquidityProvider1, liquidityProvider2, swapper]
      (cfmm, tokens) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens

      for_ [(liquidityProvider1, position1Liquidity), (liquidityProvider2, position2Liquidity)] \(lp, liquidity) ->
        withSender lp $ setPosition cfmm liquidity (-10_000, 10_000)

      (xFees, yFees) <- unzip <$> for swaps \(SwapData swapDirection swapAmt) -> do
        withSender swapper do
          case swapDirection of
            XToY -> do
              xtoy cfmm swapAmt swapper
              pure $ (calcSwapFee feeBps swapAmt, 0)
            YToX -> do
              ytox cfmm swapAmt swapper
              pure $ (0, calcSwapFee feeBps swapAmt)
      checkAllInvariants cfmm

      collectFees cfmm feeReceiver1 0 liquidityProvider1
      collectFees cfmm feeReceiver2 1 liquidityProvider2
      (feeReceiver1BalanceX, feeReceiver1BalanceY) <- balancesOf balanceConsumers feeReceiver1
      (feeReceiver2BalanceX, feeReceiver2BalanceY) <- balancesOf balanceConsumers feeReceiver2

      -- Position 2 has triple the liquidity of Position 1,
      -- so `feeReceiver1` should get 1/4 of all earned fees and `feeReceiver2` should get 3/4.
      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      feeReceiver1BalanceX `isInRangeNat` (sum xFees `div` 4) $ (1, 0)
      feeReceiver1BalanceY `isInRangeNat` (sum yFees `div` 4) $ (1, 0)
      feeReceiver2BalanceX `isInRangeNat` (sum xFees * 3 `div` 4) $ (1, 0)
      feeReceiver2BalanceY `isInRangeNat` (sum yFees * 3 `div` 4) $ (1, 0)

      checkAllInvariants cfmm

test_LPs_do_not_receive_past_fees :: TestTree
test_LPs_do_not_receive_past_fees =
  forAllTokenTypeCombinations "Liquidity Providers do not receive past fees" \tokenTypes ->
  testProperty (show tokenTypes) $ property do

    feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    beforeSwaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData
    afterSwaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData

    clevelandProp do
      liquidityProvider1 <- newAddress auto
      liquidityProvider2 <- newAddress auto

      swapper <- newAddress auto
      feeReceiver1 <- newAddress auto
      feeReceiver2 <- newAddress auto
      let accounts = [liquidityProvider1, liquidityProvider2, swapper]
      (cfmm, tokens) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens

      let placeSwaps swaps =
            bimap sum sum .
            unzip <$>
              for swaps \(SwapData swapDirection swapAmt) -> do
                withSender swapper do
                  case swapDirection of
                    XToY -> do
                      xtoy cfmm swapAmt swapper
                      pure $ (calcSwapFee feeBps swapAmt, 0)
                    YToX -> do
                      ytox cfmm swapAmt swapper
                      pure $ (0, calcSwapFee feeBps swapAmt)

      withSender liquidityProvider1 $ setPosition cfmm 1_e7 (-10_000, 10_000)
      (xFeesBefore, yFeesBefore) <- placeSwaps beforeSwaps
      withSender liquidityProvider2 $ setPosition cfmm 1_e7 (-10_000, 10_000)
      (xFeesAfter, yFeesAfter) <- placeSwaps afterSwaps

      checkAllInvariants cfmm

      collectFees cfmm feeReceiver1 0 liquidityProvider1
      collectFees cfmm feeReceiver2 1 liquidityProvider2
      (feeReceiver1BalanceX, feeReceiver1BalanceY) <- balancesOf balanceConsumers feeReceiver1
      (feeReceiver2BalanceX, feeReceiver2BalanceY) <- balancesOf balanceConsumers feeReceiver2

      -- Fees from `beforeSwaps` should all go to Position 1.
      -- Fees from `afterSwaps` should be evenly split between Position 1 and Position 2.
      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      feeReceiver1BalanceX `isInRangeNat` (xFeesBefore + (xFeesAfter `div` 2)) $ (1, 0)
      feeReceiver1BalanceY `isInRangeNat` (yFeesBefore + (yFeesAfter `div` 2)) $ (1, 0)
      feeReceiver2BalanceX `isInRangeNat` (xFeesAfter `div` 2) $ (1, 0)
      feeReceiver2BalanceY `isInRangeNat` (yFeesAfter `div` 2) $ (1, 0)

      checkAllInvariants cfmm

test_fees_are_discounted :: TestTree
test_fees_are_discounted =
  propOnNetwork "Accrued fees are discounted when adding liquidity to an existing position"
    do
      tokenTypes <- forAll $ Gen.element allTokenTypeCombinations
      feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData
      pure (tokenTypes, feeBps, protoFeeBps, swaps)
    ( defaultTokenTypes
    , 20_00
    , 7_00
    , [SwapData XToY 1000, SwapData YToX 3000, SwapData XToY 400]
    )
    \(tokenTypes, feeBps, protoFeeBps, swaps) -> do
      liquidityProvider <- newAddress auto

      swapper <- newAddress auto
      feeReceiver <- newAddress auto
      let liquidityDelta = 1_e7
      let lowerTickIndex = -10_000
      let upperTickIndex = 10_000
      (cfmm, tokens) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens

      withSender liquidityProvider $ setPosition cfmm liquidityDelta (lowerTickIndex, upperTickIndex)

      (xFees , yFees) <-
          bimap sum sum .
          unzip <$>
            withSender swapper do
              inBatch do
                for swaps \(SwapData swapDirection swapAmt) -> do
                  case swapDirection of
                    XToY -> do
                      xtoy cfmm swapAmt swapper
                      pure $ (calcSwapFee feeBps swapAmt, 0)
                    YToX -> do
                      ytox cfmm swapAmt swapper
                      pure $ (0, calcSwapFee feeBps swapAmt)

      (initialBalanceLpX, initialBalanceLpY) <- balancesOf balanceConsumers liquidityProvider

      withSender liquidityProvider $ updatePosition cfmm feeReceiver (toInteger liquidityDelta) 0

      ( (finalBalanceLpX, finalBalanceFeeReceiverX),
        (finalBalanceLpY, finalBalanceFeeReceiverY))
        <- balancesOfMany balanceConsumers (liquidityProvider, feeReceiver)

      -- The fees earned during the swaps should be discounted from the
      -- tokens needed to make the deposit.
      -- Due to rounding, it's possible the LP will receive 1 fewer tokens than expected.
      st <- getStorage cfmm
      let PerToken xDelta yDelta = liquidityDeltaToTokensDelta (fromIntegral liquidityDelta) lowerTickIndex upperTickIndex (sCurTickIndexRPC st) (sSqrtPriceRPC st)
      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      -- Due to the floating-point math used in `liquidityDeltaToTokensDelta`, it's possible there
      -- will be an additional +/- 1 error.
      finalBalanceLpX `isInRangeNat` (initialBalanceLpX + xFees - fromIntegral @Integer @Natural xDelta) $ (2, 1)
      finalBalanceLpY `isInRangeNat` (initialBalanceLpY + yFees - fromIntegral @Integer @Natural yDelta) $ (2, 1)

      -- `feeReceiver` should not receive any fees.
      finalBalanceFeeReceiverX @== 0
      finalBalanceFeeReceiverY @== 0

test_ticks_are_updated :: TestTree
test_ticks_are_updated =
  forAllTokenTypeCombinationsOnNetwork "Ticks' states are updated correctly when an overlapping position is created" \tokenTypes -> do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    transferMoney liquidityProvider 10_e6

    let liquidityDelta = 1_e5
    let feeBps = 50_00

    let ti1 = 0
    let ti2 = 50
    let ti3 = 100
    let ti4 = 150

    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
      { opModifyConstants = set cFeeBpsL feeBps }

    withSender liquidityProvider $ inBatch do
      setPosition cfmm liquidityDelta (ti1, ti3)
      setPosition cfmm liquidityDelta (ti2, ti4)
      pure ()

    -- Place a small swap to move the tick a little bit
    -- and make sure `tick_cumulative` is not 0.
    withSender swapper $ ytox cfmm 100 swapper

    -- Advance the time a few secs to make sure accumulators
    -- like `seconds_per_liquidity_cumulative` change to non-zero values.
    advanceSecs 2 [cfmm]

    -- Place a swap big enough to cross tick `ti2` and therefore
    -- change the value of the `*_outside` fields to something other than zero.
    withSender swapper $ ytox cfmm 1_000 swapper

    initialStorage <- getStorage cfmm
    initialState <- getBigMapValue (initialStorage & sTicksRPC) ti2

    -- Place a new position on `ti2` in order to update its state.
    withSender liquidityProvider $ setPosition cfmm liquidityDelta (ti2, ti3)

    -- Check that `ti2`'s state has been updated.
    finalStorage <- getStorage cfmm
    finalState <- getBigMapValue (finalStorage & sTicksRPC) ti2

    tsNPositions finalState @== tsNPositions initialState + 1
    tsLiquidityNet finalState @== tsLiquidityNet initialState + fromIntegral @Natural @Integer liquidityDelta
    tsSqrtPrice finalState @== tsSqrtPrice initialState

    -- Accumulators should stay unchanged.
    tsFeeGrowthOutside finalState @== tsFeeGrowthOutside initialState
    tsSecondsOutside finalState @== tsSecondsOutside initialState
    tsSecondsPerLiquidityOutside finalState @== tsSecondsPerLiquidityOutside initialState
    tsTickCumulativeOutside finalState @== tsTickCumulativeOutside initialState

test_many_small_liquidations :: TestTree
test_many_small_liquidations =
  propOnNetwork "Liquidating a position in small steps is (mostly) equivalent to doing it all at once"
    do
      tokenTypes <- forAll $ Gen.element allTokenTypeCombinations
      feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
      swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData
      pure (tokenTypes, feeBps, protoFeeBps, swaps)
    ( defaultTokenTypes
    , 50_00
    , 7_00
    , [SwapData XToY 1000, SwapData YToX 3000, SwapData XToY 400]
    )
    \(tokenTypes, feeBps, protoFeeBps, swaps) -> do
      liquidityProvider1 <- newAddress auto
      liquidityProvider2 <- newAddress auto
      swapper <- newAddress auto
      receiver1 <- newAddress auto
      receiver2 <- newAddress auto
      let liquidityDelta = 1_e7
      let accounts = [liquidityProvider1, liquidityProvider2, swapper]
      (cfmm, tokens) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens

      for_ [liquidityProvider1, liquidityProvider2] \liquidityProvider ->
        withSender liquidityProvider do
          setPosition cfmm liquidityDelta (-10_000, 10_000)

      withSender swapper $ inBatch do
        for_ swaps \(SwapData swapDirection swapAmt) -> do
          case swapDirection of
            XToY -> do
              xtoy cfmm swapAmt swapper
              pure $ (calcSwapFee feeBps swapAmt, 0)
            YToX -> do
              ytox cfmm swapAmt swapper
              pure $ (0, calcSwapFee feeBps swapAmt)

      -- Liquidate the position all at once
      withSender liquidityProvider1 $ updatePosition cfmm receiver1 (- toInteger liquidityDelta) 0

      -- Liquidate the position in small steps
      withSender liquidityProvider2 do
        -- Doing all 10 calls in one batch may go over the gas limit,
        -- so we do it in 2 batches of 5 instead.
        replicateM_ 2 do
          inBatch $ replicateM_ 5 do
            updatePosition cfmm receiver2 (- toInteger liquidityDelta `div` 10) 1

      ( (balanceReceiver1X, balanceReceiver2X),
        (balanceReceiver1Y, balanceReceiver2Y))
        <- balancesOfMany balanceConsumers (receiver1, receiver2)

      -- Liquidating in 10 smaller steps may lead
      -- to `receiver2` receiving up to 10 fewer tokens due to rounding errors.
      balanceReceiver2X `isInRangeNat` balanceReceiver1X $ (10, 0)
      balanceReceiver2Y `isInRangeNat` balanceReceiver1Y $ (10, 0)

test_position_initialization :: TestTree
test_position_initialization =
  forAllTokenTypeCombinations "position is initialized correctly" \tokenTypes ->
  testProperty (show tokenTypes) $ property do
    createPositionData <- forAll genNonOverlappingPositions
    feeBps <- forAll $ Gen.integral (Range.exponential 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.exponential 0 100_00)

    swapDirections <- forAll $ replicateM (length createPositionData) genSwapDirection

    clevelandProp do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      (cfmm, tokens) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      balanceConsumers <- originateBalanceConsumers tokens
      checkAllInvariants cfmm

      for_ (createPositionData `zip` swapDirections) \(cpd, swapDirection) -> do
        let CreatePositionData lowerTickIndex upperTickIndex liquidityDelta waitTime = cpd

        -- Perform a swap to move the tick a bit.
        -- This ensures the global accumulators (like fee_growth) aren't always 0.
        withSender swapper do
          (initialBalanceX, initialBalanceY) <- balancesOf balanceConsumers cfmm
          case swapDirection of
            XToY -> do
              let amt = initialBalanceX `div` 2
              safeSwap amt \amt' -> xtoy cfmm amt' swapper
            YToX -> do
              let amt = initialBalanceY `div` 2
              safeSwap amt \amt' -> ytox cfmm amt' swapper

        -- Advance the time a few secs to make sure the buffer is updated to reflect the swaps.
        advanceSecs waitTime [cfmm]
        checkAllInvariants cfmm

        initialSt <- getFullStorage cfmm
        (initialBalanceX, initialBalanceY) <- balancesOf balanceConsumers cfmm

        withSender liquidityProvider $ setPosition cfmm liquidityDelta (lowerTickIndex, upperTickIndex)
        checkAllInvariants cfmm

        st <- getFullStorage cfmm

        -- Ticks were initialized
        let initializedTickIndices = st & sTicks & bmMap & Map.keys
        checkCompares lowerTickIndex elem initializedTickIndices
        checkCompares upperTickIndex elem initializedTickIndices

        -- Ticks' states were correctly initialized.
        lowerTick <- st & sTicks & bmMap & Map.lookup lowerTickIndex & evalJust
        upperTick <- st & sTicks & bmMap & Map.lookup upperTickIndex & evalJust

        (lowerTick & tsSqrtPrice & adjustScale @30) @== sqrtPriceFor lowerTickIndex
        (upperTick & tsSqrtPrice & adjustScale @30) @== sqrtPriceFor upperTickIndex

        (lowerTick & tsLiquidityNet) @== toInteger liquidityDelta
        (upperTick & tsLiquidityNet) @== -(toInteger liquidityDelta)

        (lowerTick & tsNPositions) @== 1
        (upperTick & tsNPositions) @== 1

        do
          Accumulators expectedSecondsOutside expectedTickCumulativeOutside expectedFeeGrowthOutside expectedSecondsPerLiquidityOutside <- initTickAccumulators cfmm st lowerTickIndex
          (lowerTick & tsSecondsOutside & fromIntegral) @== expectedSecondsOutside
          (lowerTick & tsTickCumulativeOutside) @== expectedTickCumulativeOutside
          (lowerTick & tsFeeGrowthOutside <&> fmap toInteger) @== expectedFeeGrowthOutside
          (lowerTick & tsSecondsPerLiquidityOutside <&> toInteger) @== expectedSecondsPerLiquidityOutside
        do
          Accumulators expectedSecondsOutside expectedTickCumulativeOutside expectedFeeGrowthOutside expectedSecondsPerLiquidityOutside <- initTickAccumulators cfmm st upperTickIndex
          (upperTick & tsSecondsOutside & fromIntegral) @== expectedSecondsOutside
          (upperTick & tsTickCumulativeOutside) @== expectedTickCumulativeOutside
          (upperTick & tsFeeGrowthOutside <&> fmap toInteger) @== expectedFeeGrowthOutside
          (upperTick & tsSecondsPerLiquidityOutside <&> toInteger) @== expectedSecondsPerLiquidityOutside

        -- Check global state updates
        let positionIsActive = lowerTickIndex <= sCurTickIndex st && sCurTickIndex st < upperTickIndex

        if positionIsActive
          then sLiquidity st @== sLiquidity initialSt + liquidityDelta
          else sLiquidity st @== sLiquidity initialSt

        let positionId = sNewPositionId initialSt
        sNewPositionId st @== positionId + 1

        -- Check position's state
        position <- sPositions st & bmMap & Map.lookup positionId & evalJust

        psLiquidity position @== liquidityDelta
        psOwner position @== liquidityProvider
        (psLowerTickIndex &&& psUpperTickIndex) position @== (lowerTickIndex, upperTickIndex)

        expectedFeeGrowthInside <- tickAccumulatorsInside cfmm st lowerTickIndex upperTickIndex <&> aFeeGrowth
        psFeeGrowthInsideLast position @== expectedFeeGrowthInside

        -- Check FA2 transfers
        let PerToken xDelta yDelta = liquidityDeltaToTokensDelta (toInteger liquidityDelta) lowerTickIndex upperTickIndex (sCurTickIndex st) (sSqrtPrice st)
        (finalBalanceX, finalBalanceY) <- balancesOf balanceConsumers cfmm
        finalBalanceX @== initialBalanceX + fromIntegral @Integer @Natural xDelta
        finalBalanceY @== initialBalanceY + fromIntegral @Integer @Natural yDelta
  where
    -- | Attemps to execute a swap.
    -- If it fails with `tooBigPriceChangeErr`, try again with a smaller amount.
    safeSwap :: (HasCallStack, MonadNettest caps base m) => Natural -> (Natural -> m ()) -> m ()
    safeSwap amt doSwap = do
      attempt @TransferFailure (doSwap amt) >>= \case
        Right () -> pass
        Left (FailedWith _ errExpr)
          | errExpr == toExpression (toVal tooBigPriceChangeErr) -> do
            safeSwap (amt `div` 3) doSwap
        Left err -> runIO $ throwM err

test_updating_nonexisting_position :: TestTree
test_updating_nonexisting_position =
  forAllTokenTypeCombinations "attempt to update a non-existing position properly fails" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def

    withSender liquidityProvider do
      setPosition cfmm 1_e7 (-10, 10)
      updatePosition cfmm liquidityProvider 1_e7 3
        & expectCustomError_ #fA2_TOKEN_UNDEFINED

----------------------------------------------------------------------------
-- Hedgehog generators
----------------------------------------------------------------------------

data CreatePositionData = CreatePositionData
  { cpdLowerTickIndex :: TickIndex
  , cpdUpperTickIndex :: TickIndex
  , cpdLiquidityDelta :: Natural
  , cpdWaitTime :: Natural
  }
  deriving stock (Show)

genCreatePositionData :: Gen CreatePositionData
genCreatePositionData = do
  cpdLiquidityDelta <- Gen.integral (Range.linear 1 100000)
  cpdLowerTickIndex <- Gen.integral (Range.linearFrom 0 lowerBound upperBound)
  cpdUpperTickIndex <- Gen.integral (Range.linear (cpdLowerTickIndex + 1) (upperBound + 1))
  cpdWaitTime <- Gen.integral (Range.linear 0 10)
  pure CreatePositionData {..}
  where
    lowerBound = -10000
    upperBound = 10000

-- | Generate a series of positions whose boundaries are guaranteed to not overlap.
genNonOverlappingPositions :: Gen [CreatePositionData]
genNonOverlappingPositions = do
  cpds <- Gen.list (Range.linear 1 8) genCreatePositionData
  pure $ foldl'
    (\nonOverlapping cpd ->
      if boundsOverlap cpd nonOverlapping
        then nonOverlapping
        else cpd : nonOverlapping
    )
    []
    cpds
  where
    -- Checks if this position's bounds overlap with any of the other positions' bounds.
    boundsOverlap :: CreatePositionData -> [CreatePositionData] -> Bool
    boundsOverlap thisCpd otherCpds =
      let allTickIndices = thisCpd : otherCpds >>= \cpd -> [cpdLowerTickIndex cpd, cpdUpperTickIndex cpd]
      in List.nub allTickIndices /= allTickIndices

data SwapDirection = XToY | YToX
  deriving stock (Show, Enum, Bounded)

genSwapDirection :: Gen SwapDirection
genSwapDirection = Gen.enumBounded

data SwapData = SwapData
  { sdDirection :: SwapDirection
  , sdAmt :: Natural
  }
  deriving stock Show

genSwapData :: Gen SwapData
genSwapData =
  SwapData
    <$> genSwapDirection
    <*> Gen.integral (Range.linear 1 5000)
