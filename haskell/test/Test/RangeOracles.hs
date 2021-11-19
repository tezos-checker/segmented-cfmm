-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.RangeOracles
  ( module Test.RangeOracles
  ) where

import Prelude

import Control.Lens (each)
import Data.Ratio ((%))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lorentz hiding (assert, not, now, (>>))
import Lorentz.Test (contractConsumer, sec)
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.Util

test_CornerCases :: TestTree
test_CornerCases = testGroup "Corner cases"
  [ forAllTokenTypeCombinations "Asking at uninitialized tick causes an error" \tokenTypes ->
    nettestScenarioOnEmulatorCaps (show tokenTypes) do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice] tokenTypes

      withSender alice $ setPosition cfmm 1 (-100, 100)

      consumer <- originateSimple "consumer" [] contractConsumer
      expectFailedWith tickNotExistErr $
        call cfmm (Call @"Snapshot_cumulatives_inside") $
          SnapshotCumulativesInsideParam (TickIndex (-10)) (TickIndex 100) (toContractRef consumer)

  , forAllTokenTypeCombinations "Asking at empty range works as expected" \tokenTypes ->
    nettestScenarioOnEmulatorCaps (show tokenTypes) do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice] tokenTypes

      withSender alice $ setPosition cfmm 1 (0, 10)

      advanceTime (sec 1)

      consumer <- originateSimple "consumer" [] contractConsumer
      call cfmm (Call @"Snapshot_cumulatives_inside") $
        SnapshotCumulativesInsideParam (TickIndex 0) (TickIndex 0) (toContractRef consumer)

      getStorage consumer @@== [CumulativesInsideSnapshot 0 (X 0) 0]

  , forAllTokenTypeCombinations "Reversed ranges cause an error" \tokenTypes ->
    nettestScenarioOnEmulatorCaps (show tokenTypes) do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice] tokenTypes

      withSender alice $ setPosition cfmm 1 (0, 10)

      advanceTime (sec 1)

      consumer <- originateSimple "consumer" [] contractConsumer
      expectFailedWith tickOrderErr $
        call cfmm (Call @"Snapshot_cumulatives_inside") $
          SnapshotCumulativesInsideParam (TickIndex 10) (TickIndex 0) (toContractRef consumer)

  ]

test_ValuesSanity :: TestTree
test_ValuesSanity = testGroup "Values are sane"
  [ forAllTokenTypeCombinations "One position, jumping right" \tokenTypes ->
    nettestScenarioOnEmulatorCaps (show tokenTypes) do
      let tickIndicesRange@(lowerTickIndex, upperTickIndex) =
            (TickIndex -100, TickIndex 100)

      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice] tokenTypes

      withSender alice $ setPosition cfmm 1000 (lowerTickIndex, upperTickIndex)

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

        withSender alice $ ytox cfmm 2 alice

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


  , forAllTokenTypeCombinations "Interleaving positions creation, jumps over them and tracking" \tokenTypes ->
    testProperty (show tokenTypes) $ H.property do

      -- Further go test constants - they were tuned up manually using
      -- @runIO . fmt@ printer. Some of them are left as comments.
      -- Note that for succeeding tests, Tasty tends to remove most of the
      -- stdout/stderr output.
      --
      -- This means that the test is fragile - if something changes in math
      -- part of the contract, then the test will silently stop being significant,
      -- either because we will rarely cross any position boundary, or because
      -- we will leave all the positions too quickly.
      --
      -- Unfortunatelly, this issue is difficult to handle now because within
      -- Cleveland scenario (where we can see how the cur tick index changes)
      -- we cannot use 'H.classify' family of functions, nor there can we adopt
      -- the generator.

      let mkTimePeriod = (*1000)
      -- Liquidity of positions must be relatively high because we want to make
      -- small jumps on token conversions (checking cur tick index by 1-5
      -- points would be perfect)
      let mkLiquidity = (*500)
      -- Small bounds help to check various edge cases
      -- (like ones where positions share the same boundary tick)
      let tickBoundary = 10
      let mkTokensJump = (*1)
      let maxStepsNum = 5

      -- Need to provide non-zero-liquidity range
      let baseTickRange =
              ( TickIndex $ -tickBoundary * fromIntegral maxStepsNum
              , TickIndex $  tickBoundary * fromIntegral maxStepsNum
              )

      -- Generate scenario
      -- State contains positions created so far
      commands <- H.forAll $ Gen.list (Range.linear 1 maxStepsNum) do
        -- Positions to create
        positions <- Gen.list (Range.linear 1 3) do
          lowerTickIndex <- TickIndex <$> Gen.integral
            (Range.linearFrom 0 (-tickBoundary) (tickBoundary - 1))
          upperTickIndex <- TickIndex <$> Gen.integral
            (Range.linear (unTickIndex lowerTickIndex + 1) tickBoundary)

          liquidity <- mkLiquidity <$> Gen.integral (Range.linear 1 5)
          return $ ((lowerTickIndex, upperTickIndex), liquidity)

        -- How many tokens to convert during tick index jumps.
        -- We aim at cur tick index change by 1-5 points
        jump <- mkTokensJump <$> Gen.integral (Range.linearFrom @Integer 0 -3 3)
        -- Time at which we will measure cumulative values diff
        -- (each separate value does not make much sense on itself)
        checkedTimePeriod <- mkTimePeriod <$> Gen.integral (Range.linear 0 10)
        return (positions, jump, checkedTimePeriod)

      let startingTicks = mconcat
            [ [minTickIndex, maxTickIndex]
            , baseTickRange ^.. each
            ]
      -- For each command, evaluate all ticks initialized so far
      -- (including the one created by the current command)
      let commands' = evaluatingState startingTicks $
            for commands \cmd@(poss, _, _) ->
            state \ticks -> do
              let newTicks = poss >>= \((lo, up), _) -> [lo, up]
                  allTicks = ordNub . sort $ newTicks <> ticks
              ((cmd, allTicks), allTicks)

      clevelandProp do
        -- runIO . fmt $ "\n\n---------\n\n"

        alice <- newAddress "alice"
        receiver <- newAddress auto
        (cfmm, _) <- prepareSomeSegCFMM [alice] tokenTypes

        withSender alice do
          setPosition cfmm (mkLiquidity 10) baseTickRange

          for_ commands' $ \((positions, jump, timePeriod), allTicks) -> do

            -- Do changes in the contract

            for_ positions $ \(boundaries, liquidity) ->
              setPosition cfmm liquidity boundaries

            case jump `Prelude.compare` 0 of
              EQ -> pass
              GT -> ytox cfmm (fromIntegral @Integer @Natural jump) receiver
              LT -> xtoy cfmm (fromIntegral @Integer @Natural $ Prelude.abs jump) receiver

            checkAllInvariants cfmm

            -- Check cumulatives inside snapshot

            let allTickRanges = do
                  tick : otherTicks <- tails allTicks
                  tick2 <- otherTicks
                  return (tick, tick2)

            s <- getFullStorage cfmm
            let curTickIndex = sCurTickIndex s
            let curLiquidity = sLiquidity s

            -- runIO . fmt $ "-- Did changes -- \n"
            -- runIO . fmt $ "Cur tick index: " +| curTickIndex |+ "\n"
            -- runIO . fmt $ "Cur tick witness: " +| sCurTickWitness s |+ "\n"

            for_ allTickRanges $ \tickRange@(lowerTickIndex, upperTickIndex) -> do
              -- Taking a probe of cumulative values
              result <- gettingCumulativesInsideDiff cfmm tickRange
                do advanceTime (sec (timePeriod % 1))

              -- If this range was "active", then the values have increased
              if lowerTickIndex <= curTickIndex && curTickIndex < upperTickIndex
                then do
                  cisTickCumulativeInside result @==
                    unTickIndex curTickIndex * toInteger timePeriod
                  cisSecondsInside result @==
                    toInteger timePeriod
                  cisSecondsPerLiquidityInside result @~=
                    mkX' @Rational (toInteger timePeriod % toInteger curLiquidity)
                else do
                  cisTickCumulativeInside result @== 0
                  cisSecondsInside result @== 0
                  cisSecondsPerLiquidityInside result @== 0

  ]
