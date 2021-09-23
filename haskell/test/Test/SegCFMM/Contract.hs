-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Module that contains the import of segmented-cfmm contract to be used
-- in Haskell tests.
module Test.SegCFMM.Contract
  ( TokenType (..)
  , segCFMMContract
  ) where

import Universum (error)

import Lorentz (Contract)
import Lorentz.Test.Import (embedContract)

import SegCFMM.Types

-- | Helper datatype used to select an implementation in 'segCFMMContract'.
data TokenType = FA12 | FA2 | CTEZ

segCFMMContract :: TokenType -> TokenType -> Contract (Parameter) (Storage)
segCFMMContract xTokenType yTokenType = case (xTokenType, yTokenType) of
  (FA12, CTEZ) -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12_CTEZ.tz")
  (FA2,  CTEZ) -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2_CTEZ.tz")
  (FA12, FA2)  -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12_FA2.tz")
  (FA2,  FA2)  -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2.tz")
  (FA12, FA12) -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12.tz")
  (FA2,  FA12) -> $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2_FA12.tz")
  _            -> error "invalid combination of 'TokenType's"
