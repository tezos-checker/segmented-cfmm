-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Metadata
  ( spec_Metadata
  ) where

import Universum

import Lorentz.Base (toMichelsonContract)
import Lorentz.Value
import Lorentz.Test
import Michelson.Test.Integrational (tOriginate)
import Morley.Metadata (ViewParam(..))
import Test.Hspec (Spec, describe, it)
import Test.Morley.Metadata (checkView)
import Tezos.Address (unsafeParseAddress)

import SegCFMM.Types
import Test.SegCFMM.Contract (TokenType(..), segCFMMContract)
import Test.SegCFMM.Storage (storageWithMetadata)

-- | Ensure that metadata is encoded properly
spec_Metadata
  :: Spec
spec_Metadata = do
  describe "OEnsure that metadata is encoded properly" $ do
    it "should contain `get_token_x_address` view in the metadata" $
      integrationalTestExpectation shouldContainCorrectViews


shouldContainCorrectViews :: IntegrationalScenarioM ()
shouldContainCorrectViews = do
  cfmm <- tOriginate (toMichelsonContract $ segCFMMContract FA2 FA2) "Segmented CFMM"
              (toVal storageWithMetadata) (toMutez 0)
  checkView sMetadata
    (TAddress @Parameter cfmm)
    "get_token_x_address"
    (NoParam)
    (unsafeParseAddress "KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" :: Address)
