-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.XToXPrime
  ( test_swapping_x_for_x_prime
  , test_fails_when_y_doesnt_match
  ) where

import Prelude

import Hedgehog hiding (assert, failure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lorentz hiding (assert, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)

import SegCFMM.Types
import Test.Math
import Test.SegCFMM.Contract
import Test.Util

test_swapping_x_for_x_prime :: TestTree
test_swapping_x_for_x_prime =
  propOnNetwork "allows swapping X for X'"
    do
      xType <- forAll $ Gen.element xTokenTypes
      yType <- forAll $ Gen.element yTokenTypes
      zType <- forAll $ Gen.element xTokenTypes

      dx <- forAll $ Gen.integral (Range.linear 0 300_000)
      feeBps1 <- forAll $ Gen.integral (Range.linear 0 10_000)
      feeBps2 <- forAll $ Gen.integral (Range.linear 0 10_000)
      protoFeeBps1 <- forAll $ Gen.integral (Range.linear 0 10_000)
      protoFeeBps2 <- forAll $ Gen.integral (Range.linear 0 10_000)

      pure ((xType, yType, zType), dx, feeBps1, feeBps2, protoFeeBps1, protoFeeBps2)
    ( (FA2, CTEZ, FA12)
    , 20_000
    , 7_00, 3_00
    , 3_00, 1_50
    )
    \((xType, yType, zType), dx, feeBps1, feeBps2, protoFeeBps1, protoFeeBps2) -> do
      -- When the Y token is not CTEZ, we expect the contract to behave as if the protocol fee had been set to zero.
      let effectiveProtoFeeBps1 = if yType == CTEZ then protoFeeBps1 else 0
      let effectiveProtoFeeBps2 = if yType == CTEZ then protoFeeBps2 else 0

      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      swapReceiver <- newAddress auto
      transferMoney liquidityProvider 10_e6
      let accounts = [liquidityProvider, swapper]

      (x, y, z) <- originateTokenContracts accounts
        ( (xType, FA2.TokenId 0)
        , (yType, FA2.TokenId 1)
        , (zType, FA2.TokenId 2)
        )
      (cfmm1, _) <- prepareSomeSegCFMM accounts (xType, yType) def
        { opTokens = Just (x, y), opModifyConstants = set cFeeBpsL feeBps1 . set cCtezBurnFeeBpsL protoFeeBps1 }
      (cfmm2, _) <- prepareSomeSegCFMM accounts (zType, yType) def
        { opTokens = Just (z, y), opModifyConstants = set cFeeBpsL feeBps2 . set cCtezBurnFeeBpsL protoFeeBps2 }

      for_ [cfmm1, cfmm2] \cfmm -> do
        withSender liquidityProvider $ setPosition cfmm 1_e7 (-1000, 1000)

      initialSt1 <- getStorage cfmm1
      initialSt2 <- getStorage cfmm2
      initialBalanceSwapperX <- balanceOf x swapper
      initialBalanceSwapperY <- balanceOf y swapper
      initialBalanceSwapperZ <- balanceOf z swapper
      initialBalanceSwapReceiverX <- balanceOf x swapReceiver
      initialBalanceSwapReceiverY <- balanceOf y swapReceiver
      initialBalanceSwapReceiverZ <- balanceOf z swapReceiver

      withSender swapper do
        call cfmm1 (Call @"X_to_x_prime") XToXPrimeParam
          { xppDx = dx
          , xppX_PrimeContract = toAddress cfmm2
          , xppDeadline = validDeadline
          , xppMinDxPrime = 0
          , xppToDxPrime = swapReceiver
          }

      finalBalanceSwapperX <- balanceOf x swapper
      finalBalanceSwapperY <- balanceOf y swapper
      finalBalanceSwapperZ <- balanceOf z swapper
      finalBalanceSwapReceiverX <- balanceOf x swapReceiver
      finalBalanceSwapReceiverY <- balanceOf y swapReceiver
      finalBalanceSwapReceiverZ <- balanceOf z swapReceiver

      let expectedFee1 = calcSwapFee feeBps1 dx
      let expectedNewPrice1 = calcNewPriceX (sSqrtPriceRPC initialSt1) (sLiquidityRPC initialSt1) (dx - expectedFee1)
      let expectedDy = fromIntegral @Integer @Natural $ receivedY (sSqrtPriceRPC initialSt1) (adjustScale @80 expectedNewPrice1) (sLiquidityRPC initialSt1) effectiveProtoFeeBps1
      let expectedFee2 = calcSwapFee feeBps2 expectedDy
      let expectedNewPrice2 = calcNewPriceY (sSqrtPriceRPC initialSt2) (sLiquidityRPC initialSt2) (expectedDy - expectedFee2) effectiveProtoFeeBps2
      let expectedDz = fromIntegral @Integer @Natural $ receivedX (sSqrtPriceRPC initialSt2) (adjustScale @80 expectedNewPrice2) (sLiquidityRPC initialSt2)

      finalBalanceSwapperX @== initialBalanceSwapperX - dx
      finalBalanceSwapperY @== initialBalanceSwapperY
      finalBalanceSwapperZ @== initialBalanceSwapperZ
      finalBalanceSwapReceiverX @== initialBalanceSwapReceiverX
      finalBalanceSwapReceiverY @== initialBalanceSwapReceiverY
      -- Allow a rounding error of 1 token
      finalBalanceSwapReceiverZ `isInRangeNat` (initialBalanceSwapReceiverZ + expectedDz) $ (1, 1)

test_fails_when_y_doesnt_match :: TestTree
test_fails_when_y_doesnt_match =
  forAllTokenTypeCombinations "fails when the 2 contracts have different Y tokens" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    swapReceiver <- newAddress auto
    let accounts = [liquidityProvider, swapper]

    (cfmm1, _) <- prepareSomeSegCFMM accounts tokenTypes def { opModifyConstants = set cFeeBpsL 0 . set cCtezBurnFeeBpsL 0 }
    (cfmm2, _) <- prepareSomeSegCFMM accounts tokenTypes def { opModifyConstants = set cFeeBpsL 0 . set cCtezBurnFeeBpsL 0 }

    for_ [cfmm1, cfmm2] \cfmm -> do
      withSender liquidityProvider $ setPosition cfmm liquidity (lowerTickIndex, upperTickIndex)

    let expectedErrors =
          ( failedWith (customError_ #fA2_NOT_OPERATOR) ||
            failedWith (constant @(MText, Natural, Natural) ("NotEnoughAllowance", 9, 0))
          )

    withSender swapper do
      call cfmm1 (Call @"X_to_x_prime") XToXPrimeParam
        { xppDx = 10
        , xppX_PrimeContract = toAddress cfmm2
        , xppDeadline = validDeadline
        , xppMinDxPrime = 0
        , xppToDxPrime = swapReceiver
        }
        & expectTransferFailure expectedErrors
