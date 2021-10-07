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

import Test.FA2.Common
import Test.Util (balanceOf, balancesOf, prepareSomeSegCFMM, prepareSomeSegCFMM')

test_unknown_position :: TestTree
test_unknown_position =
  nettestScenarioOnEmulatorCaps "balance_of requires exising positions" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner

    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf cfmm (FA2.TokenId 0) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf cfmm (FA2.TokenId 1) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf cfmm (FA2.TokenId 8) owner

test_empty_requests :: TestTree
test_empty_requests =
  nettestScenarioOnEmulatorCaps "balance_of accepts empty requests" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner
    balancesOf cfmm [] owner @@== []

test_owned_position :: TestTree
test_owned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 1 for an owned position" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner
    setSimplePosition cfmm owner -10 10
    balanceOf cfmm (FA2.TokenId 0) owner @@== 1

test_unowned_position :: TestTree
test_unowned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 for someone else's position" do
    owner <- newAddress auto
    nonOwner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM' [owner, nonOwner]
    setSimplePosition cfmm owner -10 15

    balanceOf cfmm (FA2.TokenId 0) nonOwner @@== 0

test_exisiting_position :: TestTree
test_exisiting_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 if caller is unknown" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM owner
    setSimplePosition cfmm owner 10 15

    nonOwner <- newAddress auto
    balanceOf cfmm (FA2.TokenId 0) nonOwner @@== 0

test_multiple_positions :: TestTree
test_multiple_positions =
  nettestScenarioOnEmulatorCaps "balance_of can handle multiple owners and positions" do
    owner1 <- newAddress auto
    owner2 <- newAddress auto
    owner3 <- newAddress auto
    nonOwner1 <- newAddress auto
    nonOwner2 <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM' [owner1, owner2, owner3, nonOwner1]
    setSimplePosition cfmm owner1 -20 -15   -- TokenId 0
    setSimplePosition cfmm owner1 -10 1     -- TokenId 1
    setSimplePosition cfmm owner1 6 17      -- TokenId 2
    setSimplePosition cfmm owner2 -10 15    -- TokenId 3
    setSimplePosition cfmm owner3 -10 15    -- TokenId 4
    setSimplePosition cfmm owner3 -1 8      -- TokenId 5

    let tokenIds = map FA2.TokenId [0..5]

    balancesOf cfmm tokenIds owner1    @@== [1, 1, 1, 0, 0, 0]
    balancesOf cfmm tokenIds owner2    @@== [0, 0, 0, 1, 0, 0]
    balancesOf cfmm tokenIds owner3    @@== [0, 0, 0, 0, 1, 1]
    balancesOf cfmm tokenIds nonOwner1 @@== [0, 0, 0, 0, 0, 0]
    balancesOf cfmm tokenIds nonOwner2 @@== [0, 0, 0, 0, 0, 0]
