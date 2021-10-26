-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.OffChainView
  ( test_OffChainViews
  ) where

import Universum hiding (view)

import Fmt (pretty)

import Lorentz.Value
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface (View)
import Michelson.Interpret (MichelsonFailed)
import Michelson.Test.Dummy
import Morley.Metadata
import Test.HUnit ((@?=), Assertion, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Tezos.Address

import SegCFMM.Types
import SegCFMM.TZIP16Metadata
import Test.SegCFMM.Storage (defaultStorage)

xToken :: Address
xToken = unsafeParseAddress "KT1XMaGzMc7iGVkbnCC9gZyjs2b85A3NxJTz"

yToken :: Address
yToken = unsafeParseAddress "KT1N3SQzqEb5vU7HmtqSqnuSTgrxs8hwkqVU"

xTokenId :: FA2.TokenId
xTokenId = FA2.TokenId 0

yTokenId :: FA2.TokenId
yTokenId = FA2.TokenId 1

offChainViewStorage :: Storage
offChainViewStorage =
  defaultStorage
    { sConstants = (sConstants defaultStorage)
      { cXTokenAddress = xToken
      , cXTokenId = xTokenId
      , cYTokenAddress = yToken
      , cYTokenId = yTokenId
      }
    }

test_OffChainViews :: TestTree
test_OffChainViews =
  let
    mc = mkMetadataSettings defaultMetadataConfig
  in
    testGroup "Off-chain views"
    [ testCase "Get the address of tokens X" $
        runView @Address (getTokenXAddressView mc) offChainViewStorage NoParam
          (Right $ xToken)
    , testCase "Get the address of tokens Y" $
        runView @Address (getTokenYAddressView mc) offChainViewStorage NoParam
          (Right $ yToken)
    , testCase "Get the id of tokens X" $
        runView @FA2.TokenId (getTokenXIdView mc) offChainViewStorage NoParam
          (Right $ xTokenId)
    , testCase "Get the id of tokens Y" $
        runView @FA2.TokenId (getTokenYIdView mc) offChainViewStorage NoParam
          (Right $ yTokenId)
    ]

runView
  :: forall ret. (HasCallStack, IsoValue ret, Eq ret, Show ret)
  => View $ ToT Storage
  -> Storage
  -> ViewParam
  -> Either MichelsonFailed ret
  -> Assertion
runView view storage param expected =
  case interpretView dummyContractEnv view param storage of
    Right x -> Right x @?= expected
    Left (VIEMichelson _ (MSVIEMichelsonFailed e)) -> Left e @?= expected
    Left err -> assertFailure (pretty err)
