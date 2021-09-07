-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Types
  ( unit_ContractTypesMatch
  , unit_StorageTypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Michelson.Typed (Contract)

import SegCFMM.Types (Storage)
import Test.SegCFMM.Contract
import Test.SegCFMM.Storage

unit_ContractTypesMatch :: Assertion
unit_ContractTypesMatch = evaluateNF_ @(Contract _ _) segCFMMContract

-- TODO: Removed when `defaultStorage` is used in the tests.
unit_StorageTypesMatch :: Assertion
unit_StorageTypesMatch = void $ pure (defaultStorage :: Storage)
