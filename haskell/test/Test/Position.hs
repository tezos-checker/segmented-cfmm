-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Position where

import Prelude

import Data.Ix (inRange)
import qualified Data.Map as Map
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
import Tezos.Core (timestampPlusSeconds)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Math
import Test.SegCFMM.Contract (TokenType(..))
import Test.SegCFMM.Storage (defaultStorage)
import Test.Util

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-
TODO:
* update a position after some swaps have been made, check if we can receive fees
* attempt trades before positions are set
 -}

mkStorage
  :: ContractHandler FA2.FA2SampleParameter FA2.Storage -> FA2.TokenId
  -> ContractHandler FA2.FA2SampleParameter FA2.Storage -> FA2.TokenId
  -> Storage
mkStorage xToken xTokenId yToken yTokenId =
  defaultStorage
    { sConstants = (sConstants defaultStorage)
      { cXTokenAddress = toAddress xToken
      , cXTokenId = xTokenId
      , cYTokenAddress = toAddress yToken
      , cYTokenId = yTokenId
      }
    }

test_equal_ticks :: TestTree
test_equal_ticks =
  nettestScenarioOnEmulatorCaps "setting a position with lower_tick=upper_tick fails" do
    let lowerTickIndex = 100
    let upperTickIndex = 100
    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider $
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 1
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith tickOrderErr

test_wrong_tick_order :: TestTree
test_wrong_tick_order =
  nettestScenarioOnEmulatorCaps "setting a position with lower_tick>upper_tick fails" do
    let lowerTickIndex = 100
    let upperTickIndex = 99
    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider $
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 1
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith tickOrderErr

test_setting_a_position_with_zero_liquidity_is_a_noop :: TestTree
test_setting_a_position_with_zero_liquidity_is_a_noop =
  nettestScenarioOnEmulatorCaps "setting a position with zero liquidity is a no-op" do
    let lowerTickIndex = -100
    let upperTickIndex = 100
    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider $
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = 0
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }

    -- The storage shouldn't have changed
    getFullStorage cfmm @@== initialSt
    (balanceOf xToken xTokenId cfmm <&> fromIntegral @Natural @Integer) @@== 0
    (balanceOf yToken yTokenId cfmm <&> fromIntegral @Natural @Integer) @@== 0

test_deposit_and_withdrawal_is_a_noop :: TestTree
test_deposit_and_withdrawal_is_a_noop =
  nettestScenarioOnEmulatorCaps "depositing and withdrawing the same amount of liquidity is a no-op" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = -(toInteger liquidityDelta)
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1000000 1000000
          }
    -- The storage shouldn't have changed (with the exception that the 'new position id' counter has gone up).
    getFullStorage cfmm @@== initialSt { sNewPositionId = sNewPositionId initialSt + 1}
    -- The contract's balance should be 0.
    -- There is a margin of error, so the contract may end up with at most 1 token.
    xBalance <- balanceOf xToken xTokenId cfmm <&> fromIntegral @Natural @Integer
    checkCompares xBalance elem [0, 1]
    yBalance <- balanceOf yToken yTokenId cfmm <&> fromIntegral @Natural @Integer
    checkCompares yBalance elem [0, 1]

test_adding_liquidity_twice :: TestTree
test_adding_liquidity_twice =
  nettestScenarioOnEmulatorCaps "adding liquidity twice is the same as adding it once" $ do
    let liquidityDelta = 100000
    let lowerTickIndex = -25
    let upperTickIndex = 15

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm1 <- originateSegCFMM FA2 FA2 initialSt
    cfmm2 <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm1) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm1) yTokenId]
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm2) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm2) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider do
      -- Add liquidity twice to cfmm1
      call cfmm1 (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm1 (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = toInteger liquidityDelta
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1000000 1000000
          }
      -- Add twice the liquidity once to cfmm2
      call cfmm2 (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta * 2
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }

    -- The two contracts should have the same storage and the same balance.
    -- There may be a -/+1 margin of the error in the balance calculations.
    st1 <- getFullStorage cfmm1
    st2 <- getFullStorage cfmm2
    st1 @== st2
    cfmm1XBalance <- balanceOf xToken xTokenId cfmm1
    cfmm2XBalance <- balanceOf xToken xTokenId cfmm2
    checkCompares (cfmm1XBalance - 1, cfmm1XBalance + 1) inRange cfmm2XBalance
    cfmm1YBalance <- balanceOf yToken yTokenId cfmm1
    cfmm2YBalance <- balanceOf yToken yTokenId cfmm2
    checkCompares (cfmm1YBalance - 1, cfmm1YBalance + 1) inRange cfmm2YBalance

test_witnesses_must_be_valid :: TestTree
test_witnesses_must_be_valid =
  nettestScenarioOnEmulatorCaps "witnesses must be valid" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider do
      -- lower_tick_witness has not been initialized
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex + 1
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
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
          , sppDeadline = deadline
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
          , sppDeadline = deadline
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
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith invalidWitnessErr

test_fails_if_its_past_the_deadline :: TestTree
test_fails_if_its_past_the_deadline =
  nettestScenarioOnEmulatorCaps "fails if it's past the deadline" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

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

      let validDeadline = now
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

test_cannot_set_position_over_max_tick :: TestTree
test_cannot_set_position_over_max_tick =
  nettestScenarioOnEmulatorCaps "cannot set a position with upper_tick > max_tick" $ do
    let liquidityDelta = 10000
    let lowerTickIndex = -10

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = maxTickIndex + 1
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith tickNotExistErr

test_maximum_tokens_contributed :: TestTree
test_maximum_tokens_contributed =
  nettestScenarioOnEmulatorCaps "cannot transfer more than maximum_tokens_contributed" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 10

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider $ do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1 1
          }
          & expectFailedWith highTokensErr

      -- Also check "Update_position"
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = toInteger liquidityDelta
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1 1
          }
          & expectFailedWith highTokensErr

test_lowest_and_highest_ticks_cannot_be_garbage_collected :: TestTree
test_lowest_and_highest_ticks_cannot_be_garbage_collected =
  nettestScenarioOnEmulatorCaps "lowest and highest ticks cannot be garbage collected" $ do
    let liquidityDelta = 1
    let lowerTickIndex = minTickIndex
    let upperTickIndex = maxTickIndex

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 0
          , uppLiquidityDelta = -(toInteger liquidityDelta)
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1000000 1000000
          }
    -- The storage shouldn't have changed (with the exception that the 'new position id' counter has gone up).
    getFullStorage cfmm @@== initialSt { sNewPositionId = sNewPositionId initialSt + 1}

test_withdrawal_overflow :: TestTree
test_withdrawal_overflow =
  nettestScenarioOnEmulatorCaps "cannot withdraw more liquidity from a position than it currently has" $ do
    let liquidityDelta = 10000
    let lowerTickIndex = -10
    let upperTickIndex = 10

    liquidityProvider <- newAddress auto
    liquidityProvider2 <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap
              [ ((liquidityProvider, xTokenId), 100000)
              , ((liquidityProvider2, xTokenId), 100000)
              ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap
              [ ((liquidityProvider, yTokenId), 100000)
              , ((liquidityProvider2, yTokenId), 100000)
              ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]
    withSender liquidityProvider2 do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider2 (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider2 (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    -- Add some liquidity with `liquidityProvider`
    withSender liquidityProvider do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
    withSender liquidityProvider2 do
      -- Add some liquidity with `liquidityProvider2`,
      -- and then attempt to remove more than what was added.
      --
      -- There should still be some liquidity left in the contract (thanks to liquidityProvider),
      -- but the position's liquidity should drop below 0 (and fail).
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 1
          , uppLiquidityDelta = -(toInteger liquidityDelta) - 1
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1000000 1000000
          }
          & expectFailedWith positionLiquidityBelowZeroErr

test_position_initialization :: TestTree
test_position_initialization =
  testProperty "position is initialized correctly" $ property $ do
    let lowerBound = -10000
    let upperBound = 10000

    liquidityDelta <- forAll $ Gen.integral (Range.linear 1 100000)
    lowerTickIndex <- forAll $ Gen.integral (Range.linearFrom 0 lowerBound upperBound)
    upperTickIndex <- forAll $ Gen.integral (Range.linear (lowerTickIndex + 1) (upperBound + 1))
    waitTime <- forAll $ Gen.integral (Range.linear 0 10)
    clevelandProp do
      liquidityProvider <- newAddress auto
      let xTokenId = FA2.TokenId 0
      let yTokenId = FA2.TokenId 1
      let xFa2storage = FA2.Storage
            { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      let yFa2storage = FA2.Storage
            { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
            , sOperators = mempty
            , sTokenMetadata = mempty
            }
      xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
      yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

      let initialSt = mkStorage xToken xTokenId yToken yTokenId
      cfmm <- originateSegCFMM FA2 FA2 initialSt
      checkAllInvariants cfmm

      withSender liquidityProvider do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
        call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

      advanceSecs waitTime [cfmm]

      deadline <- mkDeadline
      withSender liquidityProvider $
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex
            , sppUpperTickIndex = upperTickIndex
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidityDelta
            , sppDeadline = deadline
            , sppMaximumTokensContributed = PerToken 1000000 1000000
            }

      checkAllInvariants cfmm

      st <- getFullStorage cfmm

      -- ticks were initialized
      (st & sTicks & mapToList <&> fmap fst) @@== [minTickIndex, lowerTickIndex, upperTickIndex, maxTickIndex]

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
        -- TODO: test creating multiple positions, otherwise the secondsPerLiquidityOutside value will always be 0
        Accumulators expectedSecondsOutside expectedTickCumulativeOutside expectedFeeGrowthOutside expectedSecondsPerLiquidityOutside <- initTickAccumulators cfmm st lowerTickIndex
        (lowerTick & tsSecondsOutside) @== expectedSecondsOutside
        (lowerTick & tsTickCumulativeOutside) @== expectedTickCumulativeOutside
        (lowerTick & tsFeeGrowthOutside) @== expectedFeeGrowthOutside
        (lowerTick & tsSecondsPerLiquidityOutside) @== expectedSecondsPerLiquidityOutside
      do
        Accumulators expectedSecondsOutside expectedTickCumulativeOutside expectedFeeGrowthOutside expectedSecondsPerLiquidityOutside <- initTickAccumulators cfmm st upperTickIndex
        (upperTick & tsSecondsOutside) @== expectedSecondsOutside
        (lowerTick & tsTickCumulativeOutside) @== expectedTickCumulativeOutside
        (upperTick & tsFeeGrowthOutside) @== expectedFeeGrowthOutside
        (upperTick & tsSecondsPerLiquidityOutside) @== expectedSecondsPerLiquidityOutside

      -- Check global state updates
      let positionIsActive = lowerTickIndex <= sCurTickIndex st && sCurTickIndex st < upperTickIndex

      if positionIsActive
        then sLiquidity st @== liquidityDelta
        else sLiquidity st @== 0

      let positionId = sNewPositionId initialSt
      sNewPositionId st @== positionId + 1

      -- Check position's state
      position <- sPositions st & bmMap & Map.lookup positionId & evalJust

      psLiquidity position @== liquidityDelta
      psOwner position @== liquidityProvider
      (psLowerTickIndex &&& psUpperTickIndex) position @== (lowerTickIndex, upperTickIndex)

      -- TODO: do some swaps, otherwise this will always be zero.
      -- The swaps must be within ranges with positive liquidity.
      expectedFeeGrowthInside <- tickAccumulatorsInside cfmm st lowerTickIndex upperTickIndex <&> aFeeGrowth
      psFeeGrowthInsideLast position @== expectedFeeGrowthInside

      -- Check FA2 transfers
      let PerToken x y = liquidityDeltaToTokensDelta (toInteger liquidityDelta) lowerTickIndex upperTickIndex (sCurTickIndex st) (sSqrtPrice st)
      (balanceOf xToken xTokenId cfmm <&> fromIntegral @Natural @Integer) @@== x
      (balanceOf yToken yTokenId cfmm <&> fromIntegral @Natural @Integer) @@== y

test_updating_nonexisting_position :: TestTree
test_updating_nonexisting_position =
  nettestScenarioOnEmulatorCaps "attempt to update a non-existing position properly fails" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 10

    liquidityProvider <- newAddress auto
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, xTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap [ ((liquidityProvider, yTokenId), 100000) ]
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    let initialSt = mkStorage xToken xTokenId yToken yTokenId
    cfmm <- originateSegCFMM FA2 FA2 initialSt

    withSender liquidityProvider do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

    deadline <- mkDeadline
    withSender liquidityProvider $ do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidity = liquidityDelta
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Update_position")
        UpdatePositionParam
          { uppPositionId = PositionId 3
          , uppLiquidityDelta = toInteger liquidityDelta
          , uppToX = liquidityProvider
          , uppToY = liquidityProvider
          , uppDeadline = deadline
          , uppMaximumTokensContributed = PerToken 1 1
          }
          & expectCustomError_ #fA2_TOKEN_UNDEFINED
