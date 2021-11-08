-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.Common
  ( setSimplePosition
  ) where

import Lorentz hiding (assert, map, not, now, (>>))
import Morley.Nettest

import SegCFMM.Types
import Test.Util

-- | Utility function to make a simple call to @Set_position@ between the two
-- given 'TickIndex'.
--
-- It should succeed and a position be created if the given address was also
-- given to 'prepareSomeSegCFMM'.
setSimplePosition
  :: MonadNettest caps base m
  => ContractHandler Parameter Storage
  -> Address
  -> TickIndex
  -> TickIndex
  -> m ()
setSimplePosition cfmm liquidityProvider lowerTickIndex upperTickIndex = do
  let liquidity = 10000000

  withSender liquidityProvider do
    call cfmm (Call @"Set_position")
      SetPositionParam
        { sppLowerTickIndex = lowerTickIndex
        , sppUpperTickIndex = upperTickIndex
        , sppLowerTickWitness = minTickIndex
        , sppUpperTickWitness = minTickIndex
        , sppLiquidity = liquidity
        , sppDeadline = validDeadline
        , sppMaximumTokensContributed = PerToken 1000000 1000000
        }
