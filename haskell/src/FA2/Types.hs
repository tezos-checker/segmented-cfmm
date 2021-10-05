-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Auxiliary FA2 types.
module FA2.Types
  ( module FA2.Types
  ) where

import Universum

import Fmt (Buildable, GenericBuildable(..))

import Lorentz.Annotation
import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Value

data FA2Token = FA2Token
  { fa2Address :: Address
  , fa2TokenId :: TokenId
  } deriving stock (Eq, Ord)

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "FA2Token" ligoLayout
deriving via (GenericBuildable FA2Token) instance Buildable FA2Token
deriving anyclass instance IsoValue FA2Token
instance HasAnnotation FA2Token
