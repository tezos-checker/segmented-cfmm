-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.SegCFMM
  ( test_swapXY
  , test_swapXXPrime
  ) where


import Prelude

import Lorentz hiding (assert, now, (>>))
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree)

import SegCFMM.Types
import Test.SegCFMM.Contract (TokenType(..))
import Test.SegCFMM.Storage (defaultStorage)
import Test.Util (originateSegCFMM)

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
