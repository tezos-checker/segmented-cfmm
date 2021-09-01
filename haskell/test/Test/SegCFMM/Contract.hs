-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Module that contains the import of segmented-cfmm contract to be used
-- in Haskell tests.
module Test.SegCFMM.Contract
  ( segCFMMContract
  ) where

import Lorentz (Contract)
import Lorentz.Test.Import (embedContract)

import SegCFMM.Types

segCFMMContract :: Contract (Parameter) (Storage)
segCFMMContract =
  $$(embedContract @Parameter @Storage "test/segmented_cfmm_default.tz")
