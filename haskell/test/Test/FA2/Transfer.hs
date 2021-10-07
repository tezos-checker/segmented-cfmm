-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.Transfer
  ( test_zero_transfers
  , test_unknown_position
  , test_removed_position
  , test_not_owner
  , test_not_operator
  , test_fungible_amount
  , test_owner_transfer
  , test_operator_transfer
  , test_self_transfer
  , test_multiple_transfers
  ) where
import Universum

import Test.Tasty (TestTree)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty
import Util.Named

import SegCFMM.Types
import Test.FA2.Common
import Test.Util

test_zero_transfers :: TestTree
test_zero_transfers =
  nettestScenarioOnEmulatorCaps "transfer is always accepted when the amount is 0" do
    owner <- newAddress auto
    operator <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    -- the token does not even exist
    withSender owner $ transferToken cfmm owner operator (FA2.TokenId 0) 0

    setSimplePosition cfmm owner -10 15
    withSender owner $ transferToken cfmm owner operator (FA2.TokenId 0) 0

    setSimplePosition cfmm owner -20 -15
    withSender owner $ updateOperator cfmm owner operator (FA2.TokenId 1) True
    withSender operator $ transferToken cfmm owner operator (FA2.TokenId 1) 0

test_unknown_position :: TestTree
test_unknown_position =
  nettestScenarioOnEmulatorCaps "transfer does not accept unknown positions" do
    owner <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    expectCustomError_ #fA2_TOKEN_UNDEFINED $
      withSender owner $ transferToken' cfmm owner receiver (FA2.TokenId 0)

test_removed_position :: TestTree
test_removed_position =
  nettestScenarioOnEmulatorCaps "depositing and withdrawing the same amount of liquidity is a no-op" $ do
    let liquidityDelta = 10000000
    let lowerTickIndex = -10
    let upperTickIndex = 15

    owner <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    deadline <- mkDeadline
    withSender owner do
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidityDelta = liquidityDelta
          , sppToX = owner
          , sppToY = owner
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }
      call cfmm (Call @"Set_position")
        SetPositionParam
          { sppLowerTickIndex = lowerTickIndex
          , sppUpperTickIndex = upperTickIndex
          , sppLowerTickWitness = minTickIndex
          , sppUpperTickWitness = minTickIndex
          , sppLiquidityDelta = -liquidityDelta
          , sppToX = owner
          , sppToY = owner
          , sppDeadline = deadline
          , sppMaximumTokensContributed = PerToken 1000000 1000000
          }

    -- the token is once again undefined because the position was removed
    expectCustomError_ #fA2_TOKEN_UNDEFINED $
      withSender owner $ transferToken' cfmm owner receiver (FA2.TokenId 0)


test_not_owner :: TestTree
test_not_owner =
  nettestScenarioOnEmulatorCaps "transfer rejects non-owned/operated positions" do
    owner <- newAddress auto
    notOwner <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM' [owner, notOwner, receiver]

    setSimplePosition cfmm owner -10 15
    expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0) $
      withSender notOwner $ transferToken' cfmm notOwner receiver (FA2.TokenId 0)

test_not_operator :: TestTree
test_not_operator =
  nettestScenarioOnEmulatorCaps "transfer rejects invalid operators" do
    owner <- newAddress auto
    notOper <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM' [owner, notOper]

    setSimplePosition cfmm owner -10 15
    expectCustomError_ #fA2_NOT_OPERATOR $
      withSender notOper $ transferToken' cfmm owner notOper (FA2.TokenId 0)

test_fungible_amount :: TestTree
test_fungible_amount =
  nettestScenarioOnEmulatorCaps "transfer rejects amounts higher than 1" do
    owner <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    setSimplePosition cfmm owner -10 15
    expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 2, #present .! 1) $
      withSender owner $ transferToken cfmm owner receiver (FA2.TokenId 0) 2

test_owner_transfer :: TestTree
test_owner_transfer =
  nettestScenarioOnEmulatorCaps "transfer moves positions when called by owner" do
    owner <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    setSimplePosition cfmm owner -10 15
    balanceOf cfmm (FA2.TokenId 0) owner @@== 1
    balanceOf cfmm (FA2.TokenId 0) receiver @@== 0
    withSender owner $ transferToken' cfmm owner receiver (FA2.TokenId 0)
    balanceOf cfmm (FA2.TokenId 0) owner @@== 0
    balanceOf cfmm (FA2.TokenId 0) receiver @@== 1
    -- check that previous owner can no longer manage the position ...
    expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 1, #present .! 0) $
      withSender owner $ transferToken' cfmm owner receiver (FA2.TokenId 0)
    -- ... but the new one can
    withSender receiver $ transferToken' cfmm receiver owner (FA2.TokenId 0)

test_operator_transfer :: TestTree
test_operator_transfer =
  nettestScenarioOnEmulatorCaps "transfer moves positions when called by operator" do
    owner <- newAddress auto
    operator <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    setSimplePosition cfmm owner -10 15
    withSender owner $ updateOperator cfmm owner operator (FA2.TokenId 0) True

    balanceOf cfmm (FA2.TokenId 0) owner @@== 1
    balanceOf cfmm (FA2.TokenId 0) operator @@== 0
    balanceOf cfmm (FA2.TokenId 0) receiver @@== 0
    withSender operator $ transferToken' cfmm owner receiver (FA2.TokenId 0)
    balanceOf cfmm (FA2.TokenId 0) owner @@== 0
    balanceOf cfmm (FA2.TokenId 0) operator @@== 0
    balanceOf cfmm (FA2.TokenId 0) receiver @@== 1

test_self_transfer :: TestTree
test_self_transfer =
  nettestScenarioOnEmulatorCaps "transfer accepts self-transfer of an existing position" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    setSimplePosition cfmm owner -10 15
    withSender owner $ transferToken' cfmm owner owner (FA2.TokenId 0)

test_multiple_transfers :: TestTree
test_multiple_transfers =
  nettestScenarioOnEmulatorCaps "transfer can handle multiple updates, in order" do
    owner1 <- newAddress auto
    owner2 <- newAddress auto
    receiver <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM' [owner1, owner2, receiver]

    setSimplePosition cfmm owner1 -10 15
    setSimplePosition cfmm owner1 -20 -15
    setSimplePosition cfmm owner2 5 12
    withSender owner2 $ updateOperator cfmm owner2 owner1 (FA2.TokenId 2) True
    let tokenIds = map FA2.TokenId [0..2]

    balancesOf cfmm tokenIds owner1    @@== [1, 1, 0]
    balancesOf cfmm tokenIds owner2    @@== [0, 0, 1]
    balancesOf cfmm tokenIds receiver  @@== [0, 0, 0]

    withSender owner1 $ transferTokens cfmm
      [ FA2.TransferItem owner2
          [ FA2.TransferDestination owner1 (FA2.TokenId 2) 1 ]
      , FA2.TransferItem owner1 $
          [ FA2.TransferDestination receiver (FA2.TokenId 0) 1
          , FA2.TransferDestination owner2 (FA2.TokenId 1) 1
          , FA2.TransferDestination receiver (FA2.TokenId 2) 1
          ]
      ]

    balancesOf cfmm tokenIds owner1    @@== [0, 0, 0]
    balancesOf cfmm tokenIds owner2    @@== [0, 1, 0]
    balancesOf cfmm tokenIds receiver  @@== [1, 0, 1]
