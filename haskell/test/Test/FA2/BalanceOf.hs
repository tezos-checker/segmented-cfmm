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

import Test.Util

liquidity :: Natural
liquidity = 1_e7

test_unknown_position :: TestTree
test_unknown_position =
  forAllTokenTypeCombinations "balance_of requires exising positions" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] tokenTypes

    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 0) cfmm) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 1) cfmm) owner
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf (TokenInfo (FA2.TokenId 8) cfmm) owner

test_empty_requests :: TestTree
test_empty_requests =
  forAllTokenTypeCombinations "balance_of accepts empty requests" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] tokenTypes
    balancesOf cfmm [] owner @@== []

test_owned_position :: TestTree
test_owned_position =
  forAllTokenTypeCombinations "balance_of is 1 for an owned position" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] tokenTypes
    withSender owner $ setPosition cfmm liquidity (-10, 10)
    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) owner @@== 1

test_unowned_position :: TestTree
test_unowned_position =
  forAllTokenTypeCombinations "balance_of is 0 for someone else's position" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    nonOwner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, nonOwner] tokenTypes
    withSender owner $ setPosition cfmm liquidity (-10, 15)

    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) nonOwner @@== 0

test_exisiting_position :: TestTree
test_exisiting_position =
  forAllTokenTypeCombinations "balance_of is 0 if caller is unknown" \tokenTypes ->
    nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] tokenTypes
    withSender owner $ setPosition cfmm liquidity (10, 15)

    nonOwner <- newAddress auto
    balanceOf (TokenInfo (FA2.TokenId 0) cfmm) nonOwner @@== 0

test_multiple_positions :: TestTree
test_multiple_positions =
  forAllTokenTypeCombinations "balance_of can handle multiple owners and positions" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner1 <- newAddress auto
    owner2 <- newAddress auto
    owner3 <- newAddress auto
    nonOwner1 <- newAddress auto
    nonOwner2 <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner1, owner2, owner3, nonOwner1] tokenTypes
    withSender owner1 $ setPosition cfmm liquidity (-20, -15)   -- TokenId 0
    withSender owner1 $ setPosition cfmm liquidity (-10, 1)     -- TokenId 1
    withSender owner1 $ setPosition cfmm liquidity (6, 17)      -- TokenId 2
    withSender owner2 $ setPosition cfmm liquidity (-10, 15)    -- TokenId 3
    withSender owner3 $ setPosition cfmm liquidity (-10, 15)    -- TokenId 4
    withSender owner3 $ setPosition cfmm liquidity (-1, 8)      -- TokenId 5

    let tokenIds = map FA2.TokenId [0..5]

    balancesOf cfmm tokenIds owner1    @@== [1, 1, 1, 0, 0, 0]
    balancesOf cfmm tokenIds owner2    @@== [0, 0, 0, 1, 0, 0]
    balancesOf cfmm tokenIds owner3    @@== [0, 0, 0, 0, 1, 1]
    balancesOf cfmm tokenIds nonOwner1 @@== [0, 0, 0, 0, 0, 0]
    balancesOf cfmm tokenIds nonOwner2 @@== [0, 0, 0, 0, 0, 0]
