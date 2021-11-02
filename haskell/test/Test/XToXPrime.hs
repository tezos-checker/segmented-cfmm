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
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.SegCFMM.Contract (TokenType(..))
import Test.SegCFMM.Storage (defaultStorage)
import Test.Util

mkStorage
  :: ContractHandler FA2.FA2SampleParameter FA2.Storage -> FA2.TokenId
  -> ContractHandler FA2.FA2SampleParameter FA2.Storage -> FA2.TokenId
  -> Natural
  -> Storage
mkStorage xToken xTokenId yToken yTokenId feeBps =
  defaultStorage
    { sConstants = (sConstants defaultStorage)
      { cXTokenAddress = toAddress xToken
      , cXTokenId = xTokenId
      , cYTokenAddress = toAddress yToken
      , cYTokenId = yTokenId
      , cFeeBps = feeBps
      }
    }

mkFA2Storage :: FA2.TokenId -> [Address] -> Natural -> FA2.Storage
mkFA2Storage tokenId accounts accountBalance =
  FA2.Storage
    { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, tokenId), accountBalance)
    , sOperators = mempty
    , sTokenMetadata = mempty
    }

test_swapping_x_for_x_prime :: TestTree
test_swapping_x_for_x_prime =
  testProperty "allows swapping X for X'" $ property do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000
    let userFA2Balance = 1_e15

    dx <- forAll $ Gen.integral (Range.linear 0 300_000)
    feeBps1 <- forAll $ Gen.integral (Range.linear 0 10_000)
    feeBps2 <- forAll $ Gen.integral (Range.linear 0 10_000)

    clevelandProp do
      liquidityProvider <- newAddress auto
      swapper <- newAddress auto
      swapReceiver <- newAddress auto
      let accounts = [liquidityProvider, swapper]

      let xTokenId = FA2.TokenId 0
      let yTokenId = FA2.TokenId 1
      let zTokenId = FA2.TokenId 2
      let xFa2storage = FA2.Storage
            { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userFA2Balance)
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      let yFa2storage = FA2.Storage
            { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, yTokenId), userFA2Balance)
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      let zFa2storage = FA2.Storage
            { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, zTokenId), userFA2Balance)
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
      yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })
      zToken <- originateSimple "fa2" zFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [zTokenId] })

      cfmm1 <- originateSegCFMM FA2 FA2 $ mkStorage xToken xTokenId yToken yTokenId feeBps1
      cfmm2 <- originateSegCFMM FA2 FA2 $ mkStorage zToken zTokenId yToken yTokenId feeBps2

      deadline <- mkDeadline
      for_ accounts \account ->
        withSender account do
          call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm1) xTokenId]
          call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm1) yTokenId]

          call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm2) yTokenId]
          call zToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm2) zTokenId]

      for_ [cfmm1, cfmm2] \cfmm -> do
        withSender liquidityProvider do
          call cfmm (Call @"Set_position")
            SetPositionParam
              { sppLowerTickIndex = lowerTickIndex
              , sppUpperTickIndex = upperTickIndex
              , sppLowerTickWitness = minTickIndex
              , sppUpperTickWitness = minTickIndex
              , sppLiquidity = liquidity
              , sppDeadline = deadline
              , sppMaximumTokensContributed = PerToken userFA2Balance userFA2Balance
              }

      initialSt1 <- getFullStorage cfmm1
      initialSt2 <- getFullStorage cfmm2
      initialBalanceSwapperX <- balanceOf xToken xTokenId swapper
      initialBalanceSwapperY <- balanceOf yToken yTokenId swapper
      initialBalanceSwapperZ <- balanceOf zToken zTokenId swapper
      initialBalanceSwapReceiverX <- balanceOf xToken xTokenId swapReceiver
      initialBalanceSwapReceiverY <- balanceOf yToken yTokenId swapReceiver
      initialBalanceSwapReceiverZ <- balanceOf zToken zTokenId swapReceiver

      withSender swapper do
        call cfmm1 (Call @"X_to_x_prime") XToXPrimeParam
          { xppDx = dx
          , xppXPrimeContract = toAddress cfmm2
          , xppDeadline = deadline
          , xppMinDxPrime = 0
          , xppToDxPrime = swapReceiver
          }
      checkAllInvariants cfmm1
      checkAllInvariants cfmm2

      finalBalanceSwapperX <- balanceOf xToken xTokenId swapper
      finalBalanceSwapperY <- balanceOf yToken yTokenId swapper
      finalBalanceSwapperZ <- balanceOf zToken zTokenId swapper
      finalBalanceSwapReceiverX <- balanceOf xToken xTokenId swapReceiver
      finalBalanceSwapReceiverY <- balanceOf yToken yTokenId swapReceiver
      finalBalanceSwapReceiverZ <- balanceOf zToken zTokenId swapReceiver

      let expectedFee1 = calcSwapFee feeBps1 dx
      let expectedNewPrice1 = calcNewPriceX (sSqrtPrice initialSt1) (sLiquidity initialSt1) (dx - expectedFee1)
      let expectedDy = fromIntegral @Integer @Natural $ receivedY (sSqrtPrice initialSt1) (adjustScale @80 expectedNewPrice1) (sLiquidity initialSt1)
      let expectedFee2 = calcSwapFee feeBps2 expectedDy
      let expectedNewPrice2 = calcNewPriceY (sSqrtPrice initialSt2) (sLiquidity initialSt2) (expectedDy - expectedFee2)
      let expectedDz = fromIntegral @Integer @Natural $ receivedX (sSqrtPrice initialSt2) (adjustScale @80 expectedNewPrice2) (sLiquidity initialSt2)

      finalBalanceSwapperX @== initialBalanceSwapperX - dx
      finalBalanceSwapperY @== initialBalanceSwapperY
      finalBalanceSwapperZ @== initialBalanceSwapperZ
      finalBalanceSwapReceiverX @== initialBalanceSwapReceiverX
      finalBalanceSwapReceiverY @== initialBalanceSwapReceiverY
      -- Allow a rounding error of 1 token
      finalBalanceSwapReceiverZ `isInRangeNat` (initialBalanceSwapReceiverZ + expectedDz) $ (1, 1)


test_fails_when_y_doesnt_match :: TestTree
test_fails_when_y_doesnt_match =
  nettestScenarioOnEmulatorCaps "fails when the 2 contracts have different Y tokens" do
    let liquidity = 1_e7
    let lowerTickIndex = -1000
    let upperTickIndex = 1000
    let userFA2Balance = 1_e15

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    swapReceiver <- newAddress auto
    let accounts = [liquidityProvider, swapper]

    let xTokenId = FA2.TokenId 0
    let y1TokenId = FA2.TokenId 1
    let y2TokenId = FA2.TokenId 2
    let zTokenId = FA2.TokenId 3
    xToken <- originateSimple "fa2" (mkFA2Storage xTokenId accounts userFA2Balance) (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    y1Token <- originateSimple "fa2" (mkFA2Storage y1TokenId accounts userFA2Balance) (FA2.fa2Contract def { FA2.cAllowedTokenIds = [y1TokenId] })
    y2Token <- originateSimple "fa2" (mkFA2Storage y2TokenId accounts userFA2Balance) (FA2.fa2Contract def { FA2.cAllowedTokenIds = [y2TokenId] })
    zToken <- originateSimple "fa2" (mkFA2Storage zTokenId accounts userFA2Balance) (FA2.fa2Contract def { FA2.cAllowedTokenIds = [zTokenId] })

    cfmm1 <- originateSegCFMM FA2 FA2 $ mkStorage xToken xTokenId y1Token y1TokenId 0
    cfmm2 <- originateSegCFMM FA2 FA2 $ mkStorage zToken zTokenId y2Token y2TokenId 0

    deadline <- mkDeadline
    for_ accounts \account ->
      withSender account do
        for [cfmm1, cfmm2] \cfmm ->
          for_ [(xToken, xTokenId), (y1Token, y1TokenId), (y2Token, y2TokenId), (zToken, zTokenId)] \(token, tokenId) ->
            call token (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) tokenId]

    for_ [cfmm1, cfmm2] \cfmm -> do
      withSender liquidityProvider do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = deadline
            , sppMaximumTokensContributed = PerToken userFA2Balance userFA2Balance
            }

    withSender swapper do
      call cfmm1 (Call @"X_to_x_prime") XToXPrimeParam
        { xppDx = 10
        , xppXPrimeContract = toAddress cfmm2
        , xppDeadline = deadline
        , xppMinDxPrime = 0
        , xppToDxPrime = swapReceiver
        }
        & expectCustomError_ #fA2_NOT_OPERATOR
