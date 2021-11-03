-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.Invariants
  ( checkAllInvariants

    -- * Individual invariants checks
  , checkCumulativesBufferInvariants
  , checkCumulativesBufferTimeInvariants
  ) where

import Prelude
import qualified Unsafe

import Data.Ix (inRange)
import qualified Data.List as List
import qualified Data.Map.Merge.Strict as Map.Merge
import Fmt
import Lorentz hiding (assert, map, not, now, or, (>>))
import Morley.Nettest
import Tezos.Core (timestampToSeconds)

import SegCFMM.Types
import Test.Math
import Test.Util

checkAllInvariants :: (HasCallStack, MonadEmulated caps base m) => ContractHandler Parameter Storage -> m ()
checkAllInvariants cfmm = do
  st <- getFullStorage cfmm
  checkTickMapInvariants st
  checkTickInvariants st
  checkStorageInvariants st
  checkBalanceInvariants cfmm st
  checkAccumulatorsInvariants cfmm st
  checkCumulativesBufferInvariants st

-- | Invariant:
-- For all accumulators: the sum of all the values accumulated between any two consecutive ticks
-- must equal the global accumulated value.
-- E.g.: if the ticks [i1, i2, i3, i4] have been initialized, then:
--   fee_growth == fee_growth_inside(i1, i2) + fee_growth_inside(i2, i3) + fee_growth_inside(i3, i4)
{-# ANN checkAccumulatorsInvariants ("HLint: ignore Use uncurry" :: Text) #-}
checkAccumulatorsInvariants
  :: (HasCallStack, MonadEmulated caps base m)
  => ContractHandler Parameter Storage
  -> Storage
  -> m ()
checkAccumulatorsInvariants cfmm st = do
  tickIndices <- mapToList (sTicks st) <&> fmap fst
  let tickIndicesPaired = tickIndices `zip` Unsafe.tail tickIndices
  insideAccumulators <- for tickIndicesPaired \(idx1, idx2) -> tickAccumulatorsInside cfmm st idx1 idx2
  let sumInsideAccumulators = foldl1 addTickAccumulators insideAccumulators

  CumulativesValue {cvTickCumulative, cvSecondsPerLiquidityCumulative} <- observe cfmm
  now <- getNow
  let globalAccumulators =
        Accumulators
          { aSeconds = timestampToSeconds now
          , aTickCumulative = cvTickCumulative
          , aFeeGrowth = fmap toInteger <$> sFeeGrowth st
          , aSecondsPerLiquidity = toInteger <$> cvSecondsPerLiquidityCumulative
          }

  globalAccumulators @== sumInsideAccumulators
  where
    addTickAccumulators :: Accumulators -> Accumulators -> Accumulators
    addTickAccumulators tc1 tc2 =
      Accumulators
        { aSeconds = aSeconds tc1 + aSeconds tc2
        , aTickCumulative = aTickCumulative tc1 + aTickCumulative tc2
        , aFeeGrowth = aFeeGrowth tc1 + aFeeGrowth tc2
        , aSecondsPerLiquidity = aSecondsPerLiquidity tc1 + aSecondsPerLiquidity tc2
        }

-- | Invariant:
-- The contract always has enough balance to liquidite all positions (and pay any fees due).
checkBalanceInvariants :: (HasCallStack, MonadEmulated caps base m) => ContractHandler Parameter Storage -> Storage -> m ()
checkBalanceInvariants cfmm st = do
  let positions = toPairs $ bmMap $ sPositions st

  -- Liquidate all positions and rollback.
  offshoot "contract should have enough liquidity to liquidate all positions" do
    for_ positions \(idx, pstate) -> do
      let liquidityProvider = psOwner pstate
      deadline <- mkDeadline
      withSender liquidityProvider do
        call cfmm (Call @"Update_position")
          UpdatePositionParam
            { uppPositionId = idx
            , uppLiquidityDelta = - fromIntegral (psLiquidity pstate)
            , uppToX = liquidityProvider
            , uppToY = liquidityProvider
            , uppDeadline = deadline
            , uppMaximumTokensContributed = PerToken 0 0
            }

-- | Invariants:
-- 1. @cur_tick_witness@ is the highest initialized tick lower than or equal to @cur_tick_index@.
-- 2.1. Current liquidity is equal to the sum of all the tick's @liquidity_net@
--      from the lowest tick up to the current tick.
-- 2.2. Current liquidity is also equal to the sum of liquidities of positions
--      that cover the current tick.
-- 3. @sqrt_price@ is the correct price for @cur_tick_index@.
checkStorageInvariants :: (HasCallStack, MonadNettest caps base m) => Storage -> m ()
checkStorageInvariants st = do
  -- Invariant 1.
  ticks <- mapToList (sTicks st)
  let curTickIndex = sCurTickIndex st
  let expectedCurTickWitness = ticks <&> fst & filter (<= curTickIndex) & maximum
  sCurTickWitness st @== expectedCurTickWitness

  -- Invariant 2.1.
  let liquiditiyAfterPriorTicks =
        ticks
        & filter (\t -> fst t <= curTickIndex)
        <&> (\t -> snd t & tsLiquidityNet)
        & sum
  sLiquidity st @== fromIntegral @Integer @Natural liquiditiyAfterPriorTicks

  -- Invariant 2.2.
  let liquidityOfActivePositions = sum do
        PositionState{..} <- elems (bmMap $ sPositions st)
        guard (curTickIndex `inTicksRange` (psLowerTickIndex, psUpperTickIndex))
        return psLiquidity
  sLiquidity st @== liquidityOfActivePositions

  -- Invariant 3.
  -- Note that the global @cur_tick_index@ does not always match the global @sqrt_price@ _exactly_.
  -- A small swap may cause the @sqrt_price@ to move a tiny bit,
  -- but it may not be enough to make the @cur_tick_index@ jump a whole unit (+1 or -1).
  checkCompares
    (sCurTickIndex st & sqrtPriceFor, sCurTickIndex st + 1 & sqrtPriceFor)
    inRange
    (sSqrtPrice st & adjustScale)

-- | Invariants:
-- 1. The sum of all the tick's liquidity_net must be 0
-- 2. Scanning the ticks from left-to-right, the running sum of their liquidity_net must never drop below 0
--      (otherwise we'd have a tick range with negative liquidity)
-- 3. All ticks must have n_positions > 0
{-# ANN checkTickInvariants ("HLint: ignore Avoid lambda using `infix`" :: Text) #-}
checkTickInvariants :: (HasCallStack, MonadNettest caps base m) => Storage -> m ()
checkTickInvariants st = do
  ticks <- mapToList (sTicks st)

  let tickLiquidities = tsLiquidityNet . snd <$> ticks

  -- Invariant 1
  sum tickLiquidities @== 0

  -- Invariant 2
  for_ (List.scanl' (+) 0 tickLiquidities) \runningSum ->
    checkCompares runningSum (>=) 0

  -- Invariant 3
  for_ (ticks <&> tsNPositions . snd) \nPositions ->
    checkCompares nPositions (>) 0

-- | Invariants:
-- 1. The bigmap always contains at least 2 entries.
-- 2. The linked-list is acyclical.
-- 3. Tick indices are in strictly increasing order.
-- 4. All bigmap indices are reachable by traversing the linked-list front to back or back to front.
-- 5. All @prev@ and @next@ pointers are valid,
--      except for the first tick's @prev@ pointer
--      and the last tick tick's @next@ pointer.
checkTickMapInvariants :: forall caps base m. (HasCallStack, MonadNettest caps base m) => Storage -> m ()
checkTickMapInvariants (sTicks -> tickMap) = do
  -- Invariant 1
  checkCompares (length tickMap) (>=) 2

  -- Invariant 5
  mapToList tickMap <&> fmap fst >>= checkIndices
  mapToListReverse tickMap <&> fmap fst >>= checkIndices
  where
    checkIndices :: [TickIndex] -> m ()
    checkIndices indices = do
      -- Invariant 2
      assert (indices == List.nub indices) $ unlinesF
        [ "The ticks linked-list is cyclical."
        , "Indices traversed: "
        , build $ indices
        ]

      -- Invariant 3
      assert (indices == sort indices) $ unlinesF @_ @Builder
        [ "Indices are not in increasing order."
        , "Indices traversed: "
        , build $ indices
        ]

      -- Invariant 4
      assert (indices == sort (keys (bmMap tickMap))) $ unlinesF @_ @Builder
        [ "Expected to have traversed all the nodes in the linked-list."
        , "Some nodes may not be reachable."
        , "Linked-list indices: " <> build (sort (keys (bmMap tickMap)))
        , "Reachable indices:   " <> build indices
        ]

-- | Invariants:
-- 1. Non-map fields in the buffer are sensible.
--    1.1. The last index is greater or equal than the first index.
--    1.2. The reserved map size is not smaller than the actual number of records.
-- 2. The map contains values under the appropriate keys.
-- 3. Timestamps increase strictly monotonically.
-- 4. Cumulative values increase strictly monotonically.
--
-- We have no way to check that values outside of [first, last] range are dummy
-- values and only they are.
checkCumulativesBufferInvariants :: forall caps base m. (HasCallStack, MonadNettest caps base m) => Storage -> m ()
checkCumulativesBufferInvariants (sCumulativesBuffer -> buffer@CumulativesBuffer{..}) = do
  -- Invariant 1.1
  checkCompares cbLast (>=) cbFirst

  -- Invariant 1.2
  checkCompares cbReservedLength (>=) (cbActualLength buffer)

  -- Invariants 2
  let bufferMap = cbAllEntries buffer
  keys bufferMap @== [cbFirst .. cbFirst + cbReservedLength - 1]

  -- Only actual records
  let bufferRecordsMap = cbEntries buffer

  -- Invariant 3
  isMonothonic $ elems bufferRecordsMap <&> tcTime

  -- Invariant 4
  isMonothonic $ elems bufferRecordsMap <&> scSum . tcSpl
  -- Tick index can be negative, we don't check it

-- | Invariants on storages separated in time.
--
-- 1. Recorded values to not change.
checkCumulativesBufferTimeInvariants
  :: forall caps base m. (HasCallStack, MonadNettest caps base m)
  => (Storage, Storage) -> m ()
checkCumulativesBufferTimeInvariants storages = do
  let mapBoth f (a, b) = (f a, f b)

  let buffers = mapBoth sCumulativesBuffer storages
  let bufferMaps = mapBoth cbEntries buffers

  -- Invariant 1
  let mergeEq k v1 v2 = assert (v1 == v2) $
        "Value for key " +| k |+ " has changed:\n\
        \  Was:\n    " +| v1 |+ "\n\
        \  After:\n    " +| v2 |+ "\n"
  _ <- uncurry
    (Map.Merge.mergeA
      Map.Merge.dropMissing
      Map.Merge.dropMissing
      (Map.Merge.zipWithAMatched mergeEq)
    ) bufferMaps

  pass
