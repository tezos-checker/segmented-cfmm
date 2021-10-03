-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.Invariants
  ( checkAllInvariants
  ) where

import Prelude
import qualified Unsafe

import qualified Data.List as List
import qualified Data.Map as Map
import Fmt
import Lorentz hiding (assert, not, now, (>>))
import Morley.Nettest

import SegCFMM.Types
import Test.Math
import Test.Util

checkAllInvariants :: (HasCallStack, MonadEmulated caps base m) => ContractHandler cp Storage -> m ()
checkAllInvariants ch = do
  st <- getFullStorage ch
  checkTickMapInvariants st
  checkTickInvariants st
  checkStorageInvariants st

-- | Invariant:
-- 1. @cur_tick_witness@ is the highest initialized tick lower than or equal to @cur_tick_index@.
-- 2. Current liquidity is equal to the sum of all the tick's @liquidity_net@
--    from the lowest tick up to the current tick.
-- 3. @sqrt_price@ is the correct price for @cur_tick_index@.
checkStorageInvariants :: (HasCallStack, MonadNettest caps base m) => Storage -> m ()
checkStorageInvariants st = do
  -- Invariant 1.
  ticks <- mapToList (sTicks st)
  let curTickIndex = sCurTickIndex st
  let expectedCurTickWitness = ticks <&> fst & filter (<= curTickIndex) & maximum
  sCurTickWitness st @== expectedCurTickWitness

  -- Invariant 2.
  let expectedLiquidity =
        ticks
        & filter (\t -> fst t <= curTickIndex)
        <&> (\t -> snd t & tsLiquidityNet)
        & sum
  sLiquidity st @== fromIntegral @Integer @Natural expectedLiquidity

  -- Invariant 3.
  (sSqrtPrice st & adjustScale) @== (sCurTickIndex st & sqrtPriceFor)

-- | Invariants:
-- 1. The sum of all the tick's liquidity_net must be 0
-- 2. Scanning the ticks from left-to-right, the running sum of their liquidity_net must never drop below 0
--      (otherwise we'd have a tick range with negative liquidity)
-- 3. All ticks (except the first and the last) must have liquidity =/= 0
-- 4. All ticks must have n_positions > 0
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
  for_ (tickLiquidities & Unsafe.tail & Unsafe.init) \tickLiquidity ->
    tickLiquidity @/= 0

  -- Invariant 4
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
      assert (indices == sort (Map.keys (bmMap tickMap))) $ unlinesF @_ @Builder
        [ "Expected to have traversed all the nodes in the linked-list."
        , "Some nodes may not be reachable."
        , "Linked-list indices: " <> build (sort (Map.keys (bmMap tickMap)))
        , "Reachable indices:   " <> build indices
        ]
