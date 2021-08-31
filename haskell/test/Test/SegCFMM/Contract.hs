-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Module that contains the import of segmented-cfmm contract to be used
-- in Haskell tests.
module Test.SegCFMM.Contract
  ( segCFMMContract
  ) where

import Michelson.Typed

import SegCFMM.Types
import Util (fetchContract)

segCFMMContract :: Contract (ToT Parameter) (ToT Storage)
segCFMMContract =
 $(fetchContract @(ToT Parameter) @(ToT Storage) "SEGMENTED_CFMM_PATH")
