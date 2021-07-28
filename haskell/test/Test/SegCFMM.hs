-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM
  ( test_SegCFMM
  ) where

import Universum

import Test.Tasty (TestTree, testGroup)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_SegCFMM :: TestTree
test_SegCFMM = testGroup "Segmented CFMM Tests"
  []
