-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.General where

import Prelude

import Lorentz hiding (assert, not, now, (>>))
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)

import Lorentz.Test (contractConsumer)
import SegCFMM.Errors
import SegCFMM.Types
import Test.SegCFMM.Contract
import Test.Util

test_non_zero_transfers :: TestTree
test_non_zero_transfers =
  nettestScenarioCaps "Non zero transfers are not allowed" do
    alice <- newAddress "alice"
    (cfmm, _) <- prepareSomeSegCFMM [alice] defaultTokenTypes def

    consumer <- originateSimple "consumer" [] contractConsumer

    transfer TransferData
      { tdTo = cfmm
      , tdAmount = 1
      , tdEntrypoint = ep "observe"
      , tdParameter = mkView [] consumer :: ObserveParam
      }
      & expectFailedWith nonZeroTransferErr
