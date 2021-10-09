-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Storage
  ( defaultStorage
  , storageWithIncreasedBuffer10
  ) where

import Michelson.Typed

import SegCFMM.Types
import Util (fetchValue)

defaultStorage :: Storage
defaultStorage =
  fromVal ($(fetchValue @Storage "test/storage_default.tz"))

storageWithIncreasedBuffer10 :: Storage
storageWithIncreasedBuffer10 =
  fromVal ($(fetchValue @Storage "test/storage_increased_buffer_10.tz"))
