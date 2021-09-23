-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Types
  ( unit_ContractTypesMatch
  , unit_StorageTypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Lorentz (Contract)

import SegCFMM.Types (Storage)
import Test.SegCFMM.Contract
import Test.SegCFMM.Storage

unit_ContractTypesMatch :: Assertion
unit_ContractTypesMatch = do
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA12 CTEZ
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA2 CTEZ
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA12 FA2
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA2 FA2
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA12 FA12
  evaluateNF_ @(Contract _ _) $ segCFMMContract FA2 FA12

-- TODO: Removed when `defaultStorage` is used in the tests.
unit_StorageTypesMatch :: Assertion
unit_StorageTypesMatch = void $ pure (defaultStorage :: Storage)
