-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
{-# OPTIONS_GHC -Wno-orphans #-}

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

import Fmt (Buildable, build, tupleF)
import Lorentz (Address)
import Test.SegCFMM.Contract
import Test.Util

liquidity :: Natural
liquidity = 1_e7

test_unknown_position :: TestTree
test_unknown_position =
  nettestScenarioOnEmulatorCaps "balance_of requires exising positions" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def

    let balanceOf' tokenId = do
          balanceConsumer <- originateBalanceConsumer (TokenInfo tokenId cfmm)
          balanceOf balanceConsumer owner

    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf' $ FA2.TokenId 0
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf' $ FA2.TokenId 1
    expectCustomError_ #fA2_TOKEN_UNDEFINED $ balanceOf' $ FA2.TokenId 8

test_empty_requests :: TestTree
test_empty_requests =
  nettestScenarioOnEmulatorCaps "balance_of accepts empty requests" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    balanceConsumer <- originateBalanceConsumer (TokenInfo (FA2.TokenId 0) cfmm)
    balancesOfMany [balanceConsumer] ([] :: [Address]) @@== one []

test_owned_position :: TestTree
test_owned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 1 for an owned position" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (-10, 10)

    balanceConsumer <- originateBalanceConsumer (TokenInfo (FA2.TokenId 0) cfmm)
    balanceOf balanceConsumer owner @@== 1

test_unowned_position :: TestTree
test_unowned_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 for someone else's position" do
    owner <- newAddress auto
    nonOwner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, nonOwner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (-10, 15)

    balanceConsumer <- originateBalanceConsumer (TokenInfo (FA2.TokenId 0) cfmm)
    balanceOf balanceConsumer nonOwner @@== 0

test_exisiting_position :: TestTree
test_exisiting_position =
  nettestScenarioOnEmulatorCaps "balance_of is 0 if caller is unknown" do
    owner <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner] defaultTokenTypes def
    withSender owner $ setPosition cfmm liquidity (10, 15)

    nonOwner <- newAddress auto
    balanceConsumer <- originateBalanceConsumer (TokenInfo (FA2.TokenId 0) cfmm)
    balanceOf balanceConsumer nonOwner @@== 0

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

    -- Originating all 6 contracts in one batch may go over the gas limit,
    -- so we do it in 2 batches of 3 instead.
    (bc0, bc1, bc2) <- originateBalanceConsumers
      ( TokenInfo (FA2.TokenId 0) cfmm
      , TokenInfo (FA2.TokenId 1) cfmm
      , TokenInfo (FA2.TokenId 2) cfmm
      )
    (bc3, bc4, bc5) <- originateBalanceConsumers
      ( TokenInfo (FA2.TokenId 3) cfmm
      , TokenInfo (FA2.TokenId 4) cfmm
      , TokenInfo (FA2.TokenId 5) cfmm
      )

    (balancesPosition0, balancesPosition1, balancesPosition2)
      <- balancesOfMany (bc0, bc1, bc2) (owner1, owner2, owner3, nonOwner1, nonOwner2)
    (balancesPosition3, balancesPosition4, balancesPosition5)
      <- balancesOfMany (bc3, bc4, bc5) (owner1, owner2, owner3, nonOwner1, nonOwner2)

    balancesPosition0 @== (1, 0, 0, 0, 0)
    balancesPosition1 @== (1, 0, 0, 0, 0)
    balancesPosition2 @== (1, 0, 0, 0, 0)
    balancesPosition3 @== (0, 1, 0, 0, 0)
    balancesPosition4 @== (0, 0, 1, 0, 0)
    balancesPosition5 @== (0, 0, 1, 0, 0)

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e) => Buildable (a, b, c, d, e) where build = tupleF
