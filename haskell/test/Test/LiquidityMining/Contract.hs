-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | Module that contains the import of the liquidity mining contract.
module Test.LiquidityMining.Contract
  ( liquidityMiningContract
  ) where

import Lorentz (Contract)
import Lorentz.Test.Import (embedContract)

import LiquidityMining.Types

liquidityMiningContract :: Contract Parameter Storage
liquidityMiningContract = $$(embedContract "test/liquidity_mining.tz")
