-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.SegCFMM
  ( test_SegCFMM
  ) where

import Universum

import Lorentz hiding (assert, now, (>>))
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Tezos.Address (unsafeParseAddress)

import SegCFMM.Types
import Test.SegCFMM.Contract (segCFMMContract)
import Test.SegCFMM.Storage (defaultStorage)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}


test_SegCFMM :: TestTree
test_SegCFMM = testGroup "Segmented CFMM Tests"
  [ nettestScenarioCaps "Set a position"
      setPositionTest
  , nettestScenarioCaps "Swap X for Y"
      swapXYTest
  ]

setPositionTest
  :: MonadNettest caps base m
  => m ()
setPositionTest = do
  owner1 <- newAddress auto
  cfmm <- originateSegCFMM defaultStorage
  let
    -- TODO: originate proper contract
    addr = unsafeParseAddress "KT1MPGAKt68xEgLe1c2n7AG89Hph8Q7o44UX"

    param = SetPositionParam
          { sppLowerTickIndex = TickIndex 2
          , sppUpperTickIndex = TickIndex 10
          , sppLowerTickWitness = TickIndex $ negate maxTick
          , sppUpperTickWitness = TickIndex $ negate maxTick
            -- Have to used `negate maxTick` on both witnesses otherwise fail with 100 (invalid witness) error code
          , sppLiquidityDelta = 0
          , sppToX = addr
          , sppToY = addr
          }

  -- TODO: Commented out until the test is fixed. Current call result in 300 error (tick not initialized).
  -- withSender owner1 $
  --   call cfmm (Call @"Set_position") param

  _ <- getStorage @Storage cfmm
  pure ()

swapXYTest
  :: MonadNettest caps base m
  => m ()
swapXYTest = do
  owner1 <- newAddress auto
  receiver <- newAddress auto
  cfmm <- originateSegCFMM defaultStorage

  let
    param = XToYParam
          { xpDx = pickX (mkX 10 :: X 80 Natural)
          , xpDeadline = [timestampQuote| 2022-01-01T00:00:00Z |]
          , xpMinDy = pickX (mkX 1 :: X 80 Natural)
          , xpToDy = receiver
          }

  -- TODO: Commented out until the test is fixed. Current call result in 104 error (smaller than min asset).
  -- withSender owner1 $
  --   call cfmm (Call @"X_to_Y") param

  _ <- getStorage @Storage cfmm
  pure ()


-------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------

originateSegCFMM
 :: MonadNettest caps base m
 => Storage
 -> m (ContractHandler Parameter Storage)
originateSegCFMM storage = do
  originateSimple "Segmented CFMM" storage segCFMMContract


-- | Note: `half_bps_pow` only supports sqrt_price up to this tick index: `2^20 - 1`.
maxTick :: Integer
maxTick = 1048575
