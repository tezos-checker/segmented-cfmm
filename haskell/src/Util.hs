-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Read a contract at compile time.
module Util
  ( fetchValue
  , resolveSourcePath
  ) where

import Prelude

import qualified Data.Text.IO.Utf8 as Utf8
import Fmt (pretty)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Language.Haskell.TH.Syntax as TH
import System.Environment (lookupEnv)

import Michelson.Runtime.Import (readValue)
import Michelson.Typed

readDependentSource
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> m Text
readDependentSource path = do
  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  qAddDependentFile path
  Utf8.readFile path

resolveSourcePath
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> String
  -> m FilePath
resolveSourcePath defaultPath envKey =
  fromMaybe defaultPath <$> liftIO (lookupEnv envKey)

-- | Reads a Michelson expression into a known typed value at compile type.
fetchValue :: forall st. KnownIsoT st => FilePath -> TH.ExpQ
fetchValue path = do
  valueLiteral <- readDependentSource path
  verifiedFetchedValue @st valueLiteral

verifiedFetchedValue :: forall st. KnownIsoT st => Text -> TH.ExpQ
verifiedFetchedValue valueLiteral =
  case readValue @(ToT st) "" valueLiteral of
    Left e ->
      -- Emit a compiler error if the value cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the value.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the
        -- value can be parsed+typechecked.
        either (error . pretty) fromVal $ readValue "" valueLiteral
      |]
