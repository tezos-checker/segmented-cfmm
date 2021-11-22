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
import Test.Util

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
    (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def
    initialSt <- getFullStorage cfmm

    withSender liquidityProvider $ setPosition cfmm 0 (-100, 100)

    -- The storage shouldn't have changed (with few exceptions).
    now <- getNow
    getFullStorage cfmm @@== initialSt
      { sCumulativesBuffer = cumulativesBuffer1 now
      }
    balanceOf x cfmm @@== 0
    balanceOf y cfmm @@== 0

test_deposit_and_withdrawal_is_a_noop :: TestTree
test_deposit_and_withdrawal_is_a_noop =
  forAllTokenTypeCombinations "depositing and withdrawing the same amount of liquidity is a no-op" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def
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
    xBalance <- balanceOf x cfmm
    checkCompares xBalance elem [0, 1]
    yBalance <- balanceOf y cfmm
    checkCompares yBalance elem [0, 1]

test_adding_liquidity_twice :: TestTree
test_adding_liquidity_twice =
  forAllTokenTypeCombinations "adding liquidity twice is the same as adding it once" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) $ do
    let liquidityDelta = 1_e5
    liquidityProvider <- newAddress auto
    let accounts = [liquidityProvider]
    x <- originateTokenContract accounts (fst tokenTypes) (FA2.TokenId 0)
    y <- originateTokenContract accounts (snd tokenTypes) (FA2.TokenId 1)
    (cfmm1, _) <- prepareSomeSegCFMM accounts tokenTypes def { opTokens = Just (x, y) }
    (cfmm2, _) <- prepareSomeSegCFMM accounts tokenTypes def { opTokens = Just (x, y) }

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
    cfmm1XBalance <- balanceOf x cfmm1
    cfmm2XBalance <- balanceOf x cfmm2
    cfmm2XBalance `isInRangeNat` cfmm1XBalance $ (1, 1)

    cfmm1YBalance <- balanceOf y cfmm1
    cfmm2YBalance <- balanceOf y cfmm2
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
          & expectFailedWith pastDeadlineErr

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
          & expectFailedWith pastDeadlineErr

test_fails_if_its_not_multiple_tick_spacing :: TestTree
test_fails_if_its_not_multiple_tick_spacing =
  forAllTokenTypeCombinations "fails if a tick index is not a multiple of 'tick_spacing'" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let liquidityDelta = 10000000

    liquidityProvider <- newAddress auto
    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider] tokenTypes def { opModifyConstants = set cTickSpacingL 10 }

    withSender liquidityProvider do
      now <- getNow

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
          , sppDeadline = now
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
          , sppDeadline = now
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
          , sppDeadline = now
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
          & expectFailedWith highTokensErr

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
          & expectFailedWith highTokensErr

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
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let liquidityDelta = 10_000
    let lowerTickIndex = -10
    let upperTickIndex = 10

    liquidityProvider1 <- newAddress auto
    liquidityProvider2 <- newAddress auto
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
  forAllTokenTypeCombinations "Liquidity Providers earn fees from swaps" \tokenTypes ->
  testProperty (show tokenTypes) $ property do

    feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData

    clevelandProp do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      feeReceiver <- newAddress auto
      (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }

      withSender liquidityProvider $ setPosition cfmm 1_e7 (-10000, 10000)

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

      collectFees cfmm feeReceiver 0 liquidityProvider
      feeReceiverBalanceX <- balanceOf x feeReceiver
      feeReceiverBalanceY <- balanceOf y feeReceiver

      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      feeReceiverBalanceX `isInRangeNat` (sum xFees) $ (1, 0)
      feeReceiverBalanceY `isInRangeNat` (sum yFees) $ (1, 0)

      checkAllInvariants cfmm

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
      (cfmm, (x, y)) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }

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
      feeReceiver1BalanceX <- balanceOf x feeReceiver1
      feeReceiver1BalanceY <- balanceOf y feeReceiver1
      collectFees cfmm feeReceiver2 1 liquidityProvider2
      feeReceiver2BalanceX <- balanceOf x feeReceiver2
      feeReceiver2BalanceY <- balanceOf y feeReceiver2

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
      (cfmm, (x, y)) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }

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
      feeReceiver1BalanceX <- balanceOf x feeReceiver1
      feeReceiver1BalanceY <- balanceOf y feeReceiver1
      collectFees cfmm feeReceiver2 1 liquidityProvider2
      feeReceiver2BalanceX <- balanceOf x feeReceiver2
      feeReceiver2BalanceY <- balanceOf y feeReceiver2

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
  forAllTokenTypeCombinations "Accrued fees are discounted when adding liquidity to an existing position" \tokenTypes ->
  testProperty (show tokenTypes) $ property do

    feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData

    clevelandProp do
      liquidityProvider <- newAddress auto

      swapper <- newAddress auto
      feeReceiver <- newAddress auto
      let liquidityDelta = 1_e7
      let lowerTickIndex = -10_000
      let upperTickIndex = 10_000
      (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }

      withSender liquidityProvider $ setPosition cfmm liquidityDelta (lowerTickIndex, upperTickIndex)

      (xFees , yFees) <-
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

      initialBalanceX <- balanceOf x liquidityProvider
      initialBalanceY <- balanceOf y liquidityProvider

      withSender liquidityProvider $ updatePosition cfmm feeReceiver (toInteger liquidityDelta) 0

      -- The fees earned during the swaps should be discounted from the
      -- tokens needed to make the deposit.
      -- Due to rounding, it's possible the LP will receive 1 fewer tokens than expected.
      st <- getFullStorage cfmm
      let PerToken xDelta yDelta = liquidityDeltaToTokensDelta (fromIntegral liquidityDelta) lowerTickIndex upperTickIndex (sCurTickIndex st) (sSqrtPrice st)
      finalBalanceX <- balanceOf x liquidityProvider
      finalBalanceY <- balanceOf y liquidityProvider
      -- Note: Fees are rounded down when being distributed to LPs, so a margin of error of -1 is acceptable.
      -- Due to the floating-point math used in `liquidityDeltaToTokensDelta`, it's possible there
      -- will be an additional +/- 1 error.
      finalBalanceX `isInRangeNat` (initialBalanceX + xFees - fromIntegral @Integer @Natural xDelta) $ (2, 1)
      finalBalanceY `isInRangeNat` (initialBalanceY + yFees - fromIntegral @Integer @Natural yDelta) $ (2, 1)

      -- `feeReceiver` should not receive any fees.
      balanceOf x feeReceiver @@== 0
      balanceOf y feeReceiver @@== 0

      checkAllInvariants cfmm

test_ticks_are_updated :: TestTree
test_ticks_are_updated =
  forAllTokenTypeCombinations "Ticks' states are updated correctly when an overlapping position is created" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let liquidityDelta = 1_e5
    let feeBps = 50_00

    let ti1 = 0
    let ti2 = 50
    let ti3 = 100
    let ti4 = 150

    (cfmm, _) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
      { opModifyConstants = set cFeeBpsL feeBps }

    withSender liquidityProvider do
      setPosition cfmm liquidityDelta (ti1, ti3)
      setPosition cfmm liquidityDelta (ti2, ti4)

    -- Place a small swap to move the tick a little bit
    -- and make sure `tick_cumulative` is not 0.
    withSender swapper $ ytox cfmm 100 swapper

    -- Advance the time a few secs to make sure accumulators
    -- like `seconds_per_liquidity_cumulative` change to non-zero values.
    advanceSecs 3 [cfmm]

    -- Place a swap big enough to cross tick `ti2` and therefore
    -- change the value of the `*_outside` fields to something other than zero.
    withSender swapper $ ytox cfmm 1_000 swapper

    initialStorage <- getFullStorage cfmm
    initialState <- initialStorage & sTicks & bmMap & Map.lookup ti2 & evalJust

    -- Place a new position on `ti2` in order to update its state.
    withSender liquidityProvider $ setPosition cfmm liquidityDelta (ti2, ti3)

    -- Check that `ti2`'s state has been updated.
    finalStorage <- getFullStorage cfmm
    finalState <- finalStorage & sTicks & bmMap & Map.lookup ti2 & evalJust

    tsNPositions finalState @== tsNPositions initialState + 1
    tsLiquidityNet finalState @== tsLiquidityNet initialState + fromIntegral @Natural @Integer liquidityDelta
    tsSqrtPrice finalState @== tsSqrtPrice initialState

    -- Accumulators should stay unchanged.
    tsFeeGrowthOutside finalState @== tsFeeGrowthOutside initialState
    tsSecondsOutside finalState @== tsSecondsOutside initialState
    tsSecondsPerLiquidityOutside finalState @== tsSecondsPerLiquidityOutside initialState
    tsTickCumulativeOutside finalState @== tsTickCumulativeOutside initialState

    checkAllInvariants cfmm

test_many_small_liquidations :: TestTree
test_many_small_liquidations =
  forAllTokenTypeCombinations "Liquidating a position in small steps is (mostly) equivalent to doing it all at once" \tokenTypes ->
  testProperty (show tokenTypes) $ property do

    feeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    protoFeeBps <- forAll $ Gen.integral (Range.linear 0 100_00)
    swaps <- forAll $ Gen.list (Range.linear 1 5) genSwapData

    clevelandProp do
      liquidityProvider1 <- newAddress auto
      liquidityProvider2 <- newAddress auto
      swapper <- newAddress auto
      receiver1 <- newAddress auto
      receiver2 <- newAddress auto
      let liquidityDelta = 1_e7
      let accounts = [liquidityProvider1, liquidityProvider2, swapper]
      (cfmm, (x, y)) <- prepareSomeSegCFMM accounts tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }

      for_ [liquidityProvider1, liquidityProvider2] \liquidityProvider ->
        withSender liquidityProvider do
          setPosition cfmm liquidityDelta (-10_000, 10_000)

      for_ swaps \(SwapData swapDirection swapAmt) -> do
        withSender swapper do
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
      replicateM_ 10 do
        withSender liquidityProvider2 $ updatePosition cfmm receiver2 (- toInteger liquidityDelta `div` 10) 1

      balanceReceiver1X <- balanceOf x receiver1
      balanceReceiver1Y <- balanceOf y receiver1
      balanceReceiver2X <- balanceOf x receiver2
      balanceReceiver2Y <- balanceOf y receiver2

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
      (cfmm, (x, y)) <- prepareSomeSegCFMM [liquidityProvider, swapper] tokenTypes def
        { opModifyConstants = set cFeeBpsL feeBps . set cCtezBurnFeeBpsL protoFeeBps }
      checkAllInvariants cfmm

      for_ (createPositionData `zip` swapDirections) \(cpd, swapDirection) -> do
        let CreatePositionData lowerTickIndex upperTickIndex liquidityDelta waitTime = cpd

        -- Perform a swap to move the tick a bit.
        -- This ensures the global accumulators (like fee_growth) aren't always 0.
        withSender swapper do
          case swapDirection of
            XToY -> do
              initialBalanceX <- balanceOf x cfmm
              let amt = initialBalanceX `div` 2
              safeSwap amt \amt' -> xtoy cfmm amt' swapper
            YToX -> do
              initialBalanceY <- balanceOf y cfmm
              let amt = initialBalanceY `div` 2
              safeSwap amt \amt' -> ytox cfmm amt' swapper

        -- Advance the time a few secs to make sure the buffer is updated to reflect the swaps.
        advanceSecs waitTime [cfmm]
        checkAllInvariants cfmm

        initialSt <- getFullStorage cfmm
        initialBalanceX <- balanceOf x cfmm
        initialBalanceY <- balanceOf y cfmm

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
        balanceOf x cfmm @@== initialBalanceX + fromIntegral @Integer @Natural xDelta
        balanceOf y cfmm @@== initialBalanceY + fromIntegral @Integer @Natural yDelta
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
