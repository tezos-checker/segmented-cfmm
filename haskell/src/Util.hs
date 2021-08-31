-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Read a contract at compile time.
module Util
  ( fetchContract
  , fetchValue
  ) where

import Universum

import Fmt (pretty)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Language.Haskell.TH.Syntax as TH
import System.Environment (lookupEnv)

import Michelson.Runtime.Import (readContract, readValue)
import Michelson.Typed

-- | Read a contract at compile time assuming its expected type is known.
--
-- This is not an ideal implementation, e.g. it does not pretty-print
-- types in error messages on types mismatch.
fetchContract :: forall cp st. (KnownT cp, KnownT st) => String -> TH.ExpQ
fetchContract envKey = do
  path <- resolveSourcePath "test/segmented_cfmm_default.tz" envKey
                          -- ↑ This default path works on CI.
                          -- There it's relative to the repo root, apparently.
  contract <- readDependentSource path

  case readContract @cp @st path contract of
    Left e ->
      -- Emit a compiler error if the contract cannot be read.
      fail (pretty e)
    Right _ ->
      -- Emit a haskell expression that reads the contract.
      [|
        -- Note: it's ok to use `error` here, because we just proved that the contract
        -- can be parsed+typechecked.
        either (error . pretty) id $
          readContract path contract
      |]

readDependentSource
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> m Text
readDependentSource path = do
  -- We cannot use 'embedFile' directly because it returns Exp,
  -- doing the following should be equivalent
  qAddDependentFile path
  readFile path

resolveSourcePath
  :: forall m. (MonadIO m, TH.Quasi m)
  => FilePath
  -> String
  -> m FilePath
resolveSourcePath defaultPath envKey =
  fromMaybe defaultPath <$> liftIO (lookupEnv envKey)

-- | Reads a Michelson expression into a known typed value at compile type.
fetchValue :: forall st. KnownIsoT st => FilePath -> String -> TH.ExpQ
fetchValue defaultPath envKey = do
  path <- resolveSourcePath defaultPath envKey
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
