-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.BalanceOf
  ( test_unknown_position
  , test_empty_requests
  , test_owned_position
  , test_unowned_position
  , test_exisiting_position
  , test_multiple_positions
  ) where

import Universum

import Test.Tasty (TestTree)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty

import Test.SegCFMM.Contract
import Test.Util

liquidity :: Natural
liquidity = 1_e7

test_unknown_position :: TestTree
test_unknown_position =
  nettestScenarioOnEmulatorCaps "balance_of requires exising positions" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def

    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 0) cfmm) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 1) cfmm) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 8) cfmm) owner

test_empty_requests :: TestTree
test_empty_requests =
  nettestScenarioOnEmulatorCaps "balance_of accepts empty requests" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    balancesOf cfmm [] owner @@== []

test_owned_position :: TestTree
test_owned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 1 for an owned position" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (-10, 10)
    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) owner @@== 1

test_unowned_position :: TestTree
test_unowned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 for someone else's position" do
    owner <- newAddress auto
    nonOwner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, nonOwner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (-10, 15)

    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) nonOwner @@== 0

test_exisiting_position :: TestTree
test_exisiting_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 if caller is unknown" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (10, 15)

    nonOwner <- newAddress auto
    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) nonOwner @@== 0

test_multiple_positions :: TestTree
test_multiple_positions =
  nettestScenarioCaps "balance_of can handle multiple owners and positions" do
    owner1 <- newAddress auto
    owner2 <- newAddress auto
    owner3 <- newAddress auto
    nonOwner1 <- newAddress auto
    nonOwner2 <- newAddress auto
    transferMoney owner1 10_e6
    cfmm <- fst <$> prepareSomeSegCFMM [owner1, owner2, owner3, nonOwner1] defaultTokenTypes def
    withSender owner1 $ inBatch do
      setPosition cfmm liquidity (-20, -15)   -- TokenId 0
      setPosition cfmm liquidity (-10, 1)     -- TokenId 1
      setPosition cfmm liquidity (6, 17)      -- TokenId 2
      pure ()
    withSender owner2 do
      setPosition cfmm liquidity (-10, 15)    -- TokenId 3
    withSender owner3 $ inBatch do
      setPosition cfmm liquidity (-10, 15)    -- TokenId 4
      setPosition cfmm liquidity (-1, 8)      -- TokenId 5
      pure ()

    let tokenIds = map FA2.TokenId [0..5]

    balancesOf cfmm tokenIds owner1    @@== [1, 1, 1, 0, 0, 0]
    balancesOf cfmm tokenIds owner2    @@== [0, 0, 0, 1, 0, 0]
    balancesOf cfmm tokenIds owner3    @@== [0, 0, 0, 0, 1, 1]
    balancesOf cfmm tokenIds nonOwner1 @@== [0, 0, 0, 0, 0, 0]
    balancesOf cfmm tokenIds nonOwner2 @@== [0, 0, 0, 0, 0, 0]
