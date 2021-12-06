-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Module that contains the import of segmented-cfmm contract to be used
-- in Haskell tests.
module Test.SegCFMM.Contract
  ( TokenType (..)
  , xTokenTypes
  , yTokenTypes
  , allTokenTypeCombinations
  , defaultTokenTypes
  , segCFMMContract
  ) where

import Prelude

import Lorentz (Contract)
import Lorentz.Test.Import (embedContract)

import SegCFMM.Types

-- | Helper datatype used to select an implementation in 'segCFMMContract'.
data TokenType = FA12 | FA2 | CTEZ
  deriving stock (Show, Eq)

xTokenTypes, yTokenTypes :: [TokenType]
xTokenTypes = [FA2, FA12]
yTokenTypes = [FA2, FA12, CTEZ]

allTokenTypeCombinations :: [(TokenType, TokenType)]
allTokenTypeCombinations = (,) <$> xTokenTypes <*> yTokenTypes

-- | An arbitrary pair of token types.
-- Tests whose outcome does not depend on the token types, can use this default pair.
defaultTokenTypes :: (TokenType, TokenType)
defaultTokenTypes = (FA2, CTEZ)

segCFMMContract :: TokenType -> TokenType -> Contract Parameter Storage
segCFMMContract xTokenType yTokenType = case (xTokenType, yTokenType) of
  (FA12, CTEZ) -> fa12_ctez
  (FA2,  CTEZ) -> fa2_ctez
  (FA12, FA2)  -> fa12_fa2
  (FA2,  FA2)  -> fa2_fa2
  (FA12, FA12) -> fa12_fa12
  (FA2,  FA12) -> fa2_fa12
  _            -> error "invalid combination of 'TokenType's"

fa12_ctez :: Contract Parameter Storage
fa12_ctez = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12_CTEZ.tz")

fa2_ctez :: Contract Parameter Storage
fa2_ctez = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2_CTEZ.tz")

fa12_fa2 :: Contract Parameter Storage
fa12_fa2 = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12_FA2.tz")

fa2_fa2 :: Contract Parameter Storage
fa2_fa2 = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2.tz")

fa12_fa12 :: Contract Parameter Storage
fa12_fa12 = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA12.tz")

fa2_fa12 :: Contract Parameter Storage
fa2_fa12 = $$(embedContract @Parameter @Storage "test/segmented_cfmm_FA2_FA12.tz")
