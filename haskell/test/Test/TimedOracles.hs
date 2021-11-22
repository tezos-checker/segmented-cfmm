-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.TimedOracles
  ( module Test.TimedOracles
  ) where

import Prelude

import Fmt ((+|), (|+))
import Lorentz.Macro hiding (assert)
import Lorentz.Test (contractConsumer, sec)
import Morley.Nettest
import Morley.Nettest.Tasty
import Test.Tasty (TestTree, testGroup)
import Tezos.Core (timestampPlusSeconds)

import SegCFMM.Errors
import SegCFMM.Types
import Test.Invariants
import Test.SegCFMM.Storage
import Test.Util

test_BufferInitialization :: TestTree
test_BufferInitialization =
  testGroup "Our initial buffer matches the ligo's one"
  [ nettestScenarioOnEmulatorCaps "Default buffer" do
      initCumulativesBuffer 0
        @== sCumulativesBuffer defaultStorage

  , nettestScenarioOnEmulatorCaps "Extra buffer slots = 10" do
      initCumulativesBuffer 10
        @== sCumulativesBuffer storageWithIncreasedBuffer10

  ]

test_Continuity :: TestTree
test_Continuity =
  nettestScenarioCaps "Returned cumulative values continuously grow over time" do
    alice <- newAddress "alice"
    (cfmm, _) <- prepareSomeSegCFMM' [alice] Nothing
      (Just defaultStorage { sCumulativesBuffer = initCumulativesBuffer 100 })
      id

    advanceTime (sec 3)

    withSender alice $ setPosition cfmm 100 (-100, 100)

    advanceTime (sec 3)

    checkedTimes <- do
      cumulatives <- getStorage cfmm >>= lastRecordedCumulatives
      let time = tcTime cumulatives
      return $ timestampPlusSeconds time <$> [-3 .. 0]

    -- Our property of interest here:
    -- lim{t -> record_time} cumulative(t) = cumulative(record_time)
    -- We will also check places of regular growth at the same time.
    consumer <- originateSimple "consumer" [] contractConsumer
    call cfmm (Call @"Observe") $ mkView checkedTimes consumer
    cumulatives <- getStorage consumer >>= \case
      [v] -> pure v
      _ -> failure "Expected to get exactly 1 cumulatives value"

    let adjacents = groupAdjacent cumulatives
    let diffs = uncurry (-) <$> adjacents
    assert (length (group diffs) == 1) $
      "Got irregular cumulatives growth:\n\
      \  " +| group diffs |+ "\n\
      \Cumulative values:\n" +| "\n\
      \  " +| cumulatives |+ ""

test_TimeOutOfBounds :: TestTree
test_TimeOutOfBounds =
  nettestScenarioCaps "Observing time out of bounds" do
    alice <- newAddress "alice"
    (cfmm, _) <- prepareSomeSegCFMM' [alice] Nothing
      (Just defaultStorage { sCumulativesBuffer = initCumulativesBuffer 100 })
      id

    now <- getNow
    consumer <- originateSimple "consumer" [] contractConsumer

    expectFailedWith observeFutureTimestampErr $
      call cfmm (Call @"Observe") $ mkView [now `timestampPlusSeconds` 1000] consumer

    expectFailedWith observeOutdatedTimestampErr $
      call cfmm (Call @"Observe") (mkView [now `timestampPlusSeconds` (-100000)] consumer)

test_IncreaseObservationCount :: TestTree
test_IncreaseObservationCount =
  nettestScenarioOnEmulatorCaps "Increasing observation count works as expected" do
    alice <- newAddress "alice"
    (cfmm, _) <- prepareSomeSegCFMM [alice]

    -- This helps to distinguish dummy and true values in the buffer
    -- Note: this also triggers the contract to record a value in the buffer
    withSender alice $ setPosition cfmm 100 (-100, 100)

    -- Run invariants that can be checked immediately,
    -- and return info (current storage) for performing later mass checks.
    let
      runInvariantsChecks :: (MonadEmulated caps base m, HasCallStack) => m Storage
      runInvariantsChecks = do
        s <- getFullStorage cfmm
        checkCumulativesBufferInvariants s
        return s

    storageSnapshotInit <- do
      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1
      cbFirstRPC @== 1
      cbLastRPC @== 1

      runInvariantsChecks

    let incr = 5
    -- Note: this does /not/ trigger new record because we are at the same time moment
    call cfmm (Call @"Increase_observation_count") incr

    consumer <- originateSimple "consumer" [] contractConsumer

    storageSnapshot0 <- do
      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 1
      cbLastRPC @== 1

      runInvariantsChecks

    -- No dummy slots were consumed till this moment, checking how they are
    -- getting filled now.
    -- We had to do only one step ahead in the buffer till this point.
    storageSnapshots1 <- for [1 .. incr] \i -> do
      -- Arbitrary call to put a new record in the buffer
      advanceTime (sec 1)
      call cfmm (Call @"Observe") $ mkView [] consumer

      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 1
      cbLastRPC @== i + 1

      runInvariantsChecks

    -- No more increase is expected

    storageSnapshots2 <- for [1..3] \i -> do
      -- Arbitrary call to put a new record in the buffer
      advanceTime (sec 1)
      call cfmm (Call @"Observe") $ mkView [] consumer

      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 1 + i
      cbLastRPC @== 1 + incr + i

      runInvariantsChecks

    let allStorageSnapshots =
            storageSnapshotInit
          : storageSnapshot0
          : storageSnapshots1
         <> storageSnapshots2

    mapM_ checkCumulativesBufferTimeInvariants $
      groupAdjacent allStorageSnapshots

    let vals = elems $ foldMap (cbEntries . sCumulativesBuffer) allStorageSnapshots
    when (and [v == v0 | v0 : _ <- pure vals, v <- vals]) $
      failure $
        "All values in the buffer were eventually equal, the test is not significant\n"
        +| vals |+ ""

test_LargeInitialBuffer :: TestTree
test_LargeInitialBuffer =
  nettestScenarioOnEmulatorCaps "Setting large initial buffer works properly" do
    alice <- newAddress "alice"
    let incr = 10
    (cfmm, _) <- prepareSomeSegCFMM' [alice] Nothing
      (Just defaultStorage { sCumulativesBuffer = initCumulativesBuffer incr })
      id

    -- Note: this also triggers the contract to record a value in the buffer
    withSender alice $ setPosition cfmm 100 (-100, 100)

    -- Run invariants that can be checked immediately,
    -- and return info (current storage) for performing later mass checks.
    let
      runInvariantsChecks :: (MonadEmulated caps base m, HasCallStack) => m Storage
      runInvariantsChecks = do
        s <- getFullStorage cfmm
        checkCumulativesBufferInvariants s
        return s

    consumer <- originateSimple "consumer" [] contractConsumer

    storageSnapshot0 <- do
      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 0
      cbLastRPC @== 1

      runInvariantsChecks

    storageSnapshot1 <- do
      replicateM_ 2 do
        advanceTime (sec 1)
        call cfmm (Call @"Observe") $ mkView [] consumer

      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 0
      cbLastRPC @== 3

      runInvariantsChecks

    storageSnapshot2 <- do
      replicateM_ 10 do
        advanceTime (sec 1)
        call cfmm (Call @"Observe") $ mkView [] consumer

      CumulativesBufferRPC{..} <- sCumulativesBufferRPC <$> getStorage cfmm
      cbReservedLengthRPC @== 1 + incr
      cbFirstRPC @== 3
      cbLastRPC @== 13

      runInvariantsChecks

    let allStorageSnapshots =
          [ storageSnapshot0
          , storageSnapshot1
          , storageSnapshot2
          ]

    mapM_ checkCumulativesBufferTimeInvariants $
      groupAdjacent allStorageSnapshots

    let vals = elems $ foldMap (cbEntries . sCumulativesBuffer) allStorageSnapshots
    when (and [v == v0 | v0 : _ <- pure vals, v <- vals]) $
      failure $
        "All values in the buffer were eventually equal, the test is not significant\n"
        +| vals |+ ""


test_ObservedValues :: TestTree
test_ObservedValues = testGroup "Observed values are sane"
  [ nettestScenarioOnEmulatorCaps "Seconds per liquidity cumulative" do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      consumer <- originateSimple "consumer" [] contractConsumer

      do
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      do
        withSender alice $ setPosition cfmm 10 (-100, 100)
        advanceTime (sec 10)
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      do
        withSender alice do
          setPosition cfmm 40 (-10, 30)
          setPosition cfmm 10000 (30, 50) -- not active position

        advanceTime (sec 100)
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      do
        withSender alice $ updatePosition cfmm alice -10 0
        advanceTime (sec 10)
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      viewedResults <- reverse <$> getStorage consumer
      splCums <- forM viewedResults $ \case
        [x] -> pure (cvSecondsPerLiquidityCumulative x)
        _ -> failure "Expected exactly one entry"

      safeHead splCums @== Just (X 0)

      [ adjustScale @30 $ nextSplCum - prevSplCum | (prevSplCum, nextSplCum) <- groupAdjacent splCums ]
        @== mkX' @Double <$> [1, 2, 0.25]

  , nettestScenarioOnEmulatorCaps "Tick cumulative" do
      alice <- newAddress "alice"
      (cfmm, _) <- prepareSomeSegCFMM [alice]

      consumer <- originateSimple "consumer" [] contractConsumer

      do
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      do
        withSender alice do
          setPosition cfmm 10 (-10, 10)
          -- Just to the end of position
          ytox cfmm 2 alice
        sCurTickIndex <$> getFullStorage cfmm @@== TickIndex 10
        advanceTime (sec 10)
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      do
        withSender alice do
          setPosition cfmm 10 (-20, 50)
          -- Just to the start of position
          xtoy cfmm 5 alice
        sCurTickIndex <$> getFullStorage cfmm @@== TickIndex (-21)
        advanceTime (sec 100)
        now <- getNow
        call cfmm (Call @"Observe") $ mkView [now] consumer

      viewedResults <- reverse <$> getStorage consumer
      tickCums <- forM viewedResults $ \case
        [x] -> pure (cvTickCumulative x)
        _ -> failure "Expected exactly one entry"

      safeHead tickCums @== Just 0

      [ nextTickCum - prevTickCum | (prevTickCum, nextTickCum) <- groupAdjacent tickCums ]
        @== [10 * 10, -21 * 100]

    ]
