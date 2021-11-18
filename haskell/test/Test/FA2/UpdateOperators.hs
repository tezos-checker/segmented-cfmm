-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.UpdateOperators
  ( test_sender_owner
  , test_unknown_position
  , test_unknown_operator
  , test_empty_updates
  , test_remove_unset
  , test_overriding_updates
  , test_multiple_updates
  ) where

import Universum

import Test.Tasty (TestTree)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty

import Test.Util

test_sender_owner :: TestTree
test_sender_owner =
  forAllTokenTypeCombinations "update_operators require the owner to be the sender" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    operator <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, operator] tokenTypes

    expectCustomError_ #fA2_NOT_OWNER $
      updateOperator cfmm owner operator (FA2.TokenId 0) True
    expectCustomError_ #fA2_NOT_OWNER $
      withSender owner $ do
        let sameOwner = FA2.AddOperator $ FA2.OperatorParam owner operator (FA2.TokenId 0)
            notOwner = FA2.AddOperator $ FA2.OperatorParam operator owner (FA2.TokenId 2)
        updateOperators cfmm [sameOwner, notOwner]

test_unknown_position :: TestTree
test_unknown_position =
  forAllTokenTypeCombinations "update_operators allows unknown positions" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    operator <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, operator] tokenTypes

    withSender owner $ updateOperator cfmm owner operator (FA2.TokenId 0) True
    -- The transfer can be performed even if the token_id is added later
    withSender owner $ setPosition cfmm 1_e7 (-10, 15)
    receiver <- newAddress auto
    withSender operator $ transferToken' cfmm owner receiver (FA2.TokenId 0)

test_unknown_operator :: TestTree
test_unknown_operator =
  forAllTokenTypeCombinations "update_operators allows unknown operators and owners" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    cfmm <- fst <$> prepareSomeSegCFMM [] tokenTypes

    owner <- newAddress auto
    operator <- newAddress auto
    withSender owner $ updateOperator cfmm owner operator (FA2.TokenId 4) True

test_empty_updates :: TestTree
test_empty_updates =
  forAllTokenTypeCombinations "update_operators accepts empty updates" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    cfmm <- fst <$> prepareSomeSegCFMM [] tokenTypes
    updateOperators cfmm []

test_remove_unset :: TestTree
test_remove_unset =
  forAllTokenTypeCombinations "update_operators accepts removing an unset operator" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    operator <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, operator] tokenTypes

    withSender owner $ updateOperator cfmm owner operator (FA2.TokenId 4) False

test_overriding_updates :: TestTree
test_overriding_updates =
  forAllTokenTypeCombinations "update_operators overrides opposite updates" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    operator <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, operator] tokenTypes

    let addTheOperator = FA2.AddOperator $ FA2.OperatorParam owner operator (FA2.TokenId 0)
        removeTheOperator = FA2.RemoveOperator $ FA2.OperatorParam owner operator (FA2.TokenId 0)
    withSender owner $ updateOperators cfmm [addTheOperator, removeTheOperator]
    expectCustomError_ #fA2_NOT_OPERATOR $
      withSender operator $ transferToken' cfmm owner operator (FA2.TokenId 0)

test_multiple_updates :: TestTree
test_multiple_updates =
  forAllTokenTypeCombinations "update_operators can handle multiple updates" \tokenTypes ->
  nettestScenarioOnEmulatorCaps (show tokenTypes) do
    owner <- newAddress auto
    operator1 <- newAddress auto
    operator2 <- newAddress auto
    cfmm <- fst <$> prepareSomeSegCFMM [owner, operator1, operator2] tokenTypes
    withSender owner do
      setPosition cfmm 1_e7 (-10, 15)
      setPosition cfmm 1_e7 (-20, -15)

      let addOperator1 = FA2.AddOperator $ FA2.OperatorParam owner operator1 (FA2.TokenId 0)
          removeOperator1 = FA2.RemoveOperator $ FA2.OperatorParam owner operator1 (FA2.TokenId 1)
          addOperator2 = FA2.AddOperator $ FA2.OperatorParam owner operator2 (FA2.TokenId 1)
      updateOperators cfmm [addOperator1, removeOperator1, addOperator2]

    withSender operator1 $ transferToken' cfmm owner operator2 (FA2.TokenId 0)
    withSender operator2 $ transferToken' cfmm owner operator1 (FA2.TokenId 1)
