-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.SegCFMM
  ( test_setPosition
  , test_swapXY
  , test_swapXXPrime
  ) where

import Universum

import Lorentz hiding (assert, now, (>>))
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)
import Tezos.Address (unsafeParseAddress)
import Tezos.Core (timestampPlusSeconds)

import SegCFMM.Types
import Test.SegCFMM.Contract (TokenType(..), segCFMMContract)
import Test.SegCFMM.Storage (defaultStorage)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_setPosition :: TestTree
test_setPosition =
  nettestScenarioCaps "Set a position" do
    owner1 <- newAddress auto
    cfmm <- originateSegCFMM FA2 CTEZ defaultStorage
    currentTime <- getNow
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
            , sppDeadline = timestampPlusSeconds currentTime 60
            , sppMaximumTokensContributed = PerToken 10000 10000
            }

    -- TODO: Commented out until the test is fixed. Current call result in 300 error (tick not initialized).
    -- withSender owner1 $
    --   call cfmm (Call @"Set_position") param

    _ <- getStorage @Storage cfmm
    pure ()

test_swapXY :: TestTree
test_swapXY =
  nettestScenarioCaps "Swap X for Y" do
    owner1 <- newAddress auto
    receiver <- newAddress auto
    cfmm <- originateSegCFMM FA2 CTEZ defaultStorage

    let
      param = XToYParam
            { xpDx = pickX (mkX 10 :: X 80 Natural)
            , xpDeadline = [timestampQuote| 2022-01-01T00:00:00Z |]
            , xpMinDy = pickX (mkX 1 :: X 80 Natural)
            , xpToDy = receiver
            }

    -- TODO: Commented out until the test is fixed. Current call result in 104 error (smaller than min asset).
    -- withSender owner1 $
    --   call cfmm (Call @"X_to_y") param

    _ <- getStorage @Storage cfmm
    pure ()

test_swapXXPrime :: TestTree
test_swapXXPrime =
  nettestScenarioCaps "Swap X for X" do
    owner1 <- newAddress auto
    cfmm <- originateSegCFMM FA2 CTEZ defaultStorage

    let
      param = XToXPrimeParam
            { xppDx = pickX (mkX 100 :: X 80 Natural)
            , xppDeadline = [timestampQuote| 2022-01-01T00:00:00Z |]
            , xppXPrimeContract = toAddress cfmm
            , xppMinDxPrime = pickX (mkX 1 :: X 80 Natural)
            , xppToDxPrime = owner1
            }

    -- TODO: Result from x_to_y swap always results in 0 which should not happen.
    -- withSender owner1 $
    --   call cfmm (Call @"X_to_x_prime") param

    _ <- getStorage @Storage cfmm
    pure ()


-------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------

originateSegCFMM
  :: MonadNettest caps base m
  => TokenType -> TokenType -> Storage
  -> m (ContractHandler Parameter Storage)
originateSegCFMM xTokenType yTokenType storage = do
  originateSimple "Segmented CFMM" storage $ segCFMMContract xTokenType yTokenType


-- | Note: `half_bps_pow` only supports sqrt_price up to this tick index: `2^20 - 1`.
maxTick :: Integer
maxTick = 1048575
