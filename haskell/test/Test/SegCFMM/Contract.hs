-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | LIGO version of the contract.
module Test.SegCFMM.Contract
  ( segCFMMContractLigo
  ) where

import Michelson.Typed

import SegCFMM.Types
import Util (fetchContract)

segCFMMContractLigo :: Contract (ToT Parameter) (ToT Storage)
segCFMMContractLigo =
 $(fetchContract @(ToT Parameter) @(ToT Storage) "SEGMENTED_CFMM_PATH")
