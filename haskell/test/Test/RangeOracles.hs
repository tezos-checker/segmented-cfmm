-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.RangeOracles
  ( module Test.RangeOracles
  ) where

import Prelude

import Lorentz hiding (assert, not, now, (>>))
import Lorentz.Test (contractConsumer, sec)
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Util

test_CornerCases :: TestTree
test_CornerCases = testGroup "Corner cases"
  [ nettestScenarioOnEmulatorCaps "Asking at uninitialized tick causes an error" do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      withSender alice do
        call cfmm (Call @"Set_position") $
          setPositionParamSimple (TickIndex (-100), TickIndex 100) 1

      consumer <- originateSimple "consumer" [] contractConsumer
      expectFailedWith tickNotExistErr $
        call cfmm (Call @"Snapshot_cumulatives_inside") $
          SnapshotCumulativesInsideParam (TickIndex (-10)) (TickIndex 100) (toContractRef consumer)

  , nettestScenarioOnEmulatorCaps "Asking at empty range works as expected" do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      withSender alice do
        call cfmm (Call @"Set_position") $
          setPositionParamSimple (TickIndex 0, TickIndex 10) 1

      advanceTime (sec 1)

      consumer <- originateSimple "consumer" [] contractConsumer
      call cfmm (Call @"Snapshot_cumulatives_inside") $
        SnapshotCumulativesInsideParam (TickIndex 0) (TickIndex 0) (toContractRef consumer)

      getStorage consumer @@== [CumulativesInsideSnapshot 0 (X 0) 0]

  , nettestScenarioOnEmulatorCaps "Reversed ranges cause an error" do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      withSender alice do
        call cfmm (Call @"Set_position") $
          setPositionParamSimple (TickIndex 0, TickIndex 10) 1

      advanceTime (sec 1)

      consumer <- originateSimple "consumer" [] contractConsumer
      expectFailedWith tickOrderErr $
        call cfmm (Call @"Snapshot_cumulatives_inside") $
          SnapshotCumulativesInsideParam (TickIndex 10) (TickIndex 0) (toContractRef consumer)

  ]

test_ValuesSanity :: TestTree
test_ValuesSanity = testGroup "Values are sane"
  [ nettestScenarioOnEmulatorCaps "One position, jumping right" do
      let tickIndicesRange@(lowerTickIndex, upperTickIndex) =
            (TickIndex -100, TickIndex 100)

      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      withSender alice do
        call cfmm (Call @"Set_position") $
          setPositionParamSimple (lowerTickIndex, upperTickIndex) 1000

      gettingCumulativesInsideDiff cfmm tickIndicesRange
        do advanceTime (sec 2000)
        @@== CumulativesInsideSnapshot
          { cisTickCumulativeInside = 0
          , cisSecondsInside = 2000
          , cisSecondsPerLiquidityInside = 2
          }

      -- additionally checking a period where contract changes
      sums <- gettingCumulativesInsideDiff cfmm tickIndicesRange $ do
        advanceTime (sec 2000)

        withSender alice $ convertTokens cfmm 2

        curTickIndex <- sCurTickIndex <$> getFullStorage cfmm
        gettingCumulativesInsideDiff cfmm tickIndicesRange
          do advanceTime (sec 1000)
          @@== CumulativesInsideSnapshot
            { cisTickCumulativeInside = unTickIndex curTickIndex * 1000
            , cisSecondsInside = 1000
            , cisSecondsPerLiquidityInside = 1
            }

      curTickIndex <- sCurTickIndex <$> getFullStorage cfmm
      sums @== CumulativesInsideSnapshot
        { cisTickCumulativeInside = 0 + unTickIndex curTickIndex * 1000
        , cisSecondsInside = 2000 + 1000
        , cisSecondsPerLiquidityInside = 2 + 1
        }

  ]
