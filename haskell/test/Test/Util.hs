-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Util
  (
  -- * Cleveland helpers
    clevelandProp
  , evalJust
  -- * Segmented CFMM helpers
  , originateSegCFMM
  , mkDeadline
  , advanceSecs
  , mapToList
  , mapToListReverse
  -- * FA2 helpers
  , balanceOf
  -- * Other utils
  , divUp
  ) where

import Prelude

import qualified Data.Map as Map
import Fmt (Buildable(build), GenericBuildable(..), indentF, unlinesF, (+|), (|+))
import Hedgehog hiding (assert, failure)
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Pure (PureM, runEmulated)
import Tezos.Core (timestampPlusSeconds)
import Time (sec)

import SegCFMM.Types
import Test.SegCFMM.Contract (TokenType(..), segCFMMContract)

deriving stock instance Eq CumulativesBuffer
deriving stock instance Eq Storage

----------------------------------------------------------------------------
-- Cleveland helpers
----------------------------------------------------------------------------

-- | Create a hedgehog property-based test from a cleveland scenario.
clevelandProp :: (MonadIO m, MonadTest m) => EmulatedT PureM () -> m ()
clevelandProp = nettestTestProp . runEmulated . uncapsNettestEmulated

evalJust :: (HasCallStack, MonadNettest caps base m) => Maybe a -> m a
evalJust = \case
  Nothing -> failure "Expected 'Just', got 'Nothing'"
  Just a -> pure a

----------------------------------------------------------------------------
-- Segmented CFMM helpers
----------------------------------------------------------------------------

originateSegCFMM
  :: MonadNettest caps base m
  => TokenType -> TokenType -> Storage
  -> m (ContractHandler Parameter Storage)
originateSegCFMM xTokenType yTokenType storage = do
  originateSimple "Segmented CFMM" storage $ segCFMMContract xTokenType yTokenType

-- | Create a valid deadline
mkDeadline :: MonadNettest caps base m => m Timestamp
mkDeadline = do
  currentTime <- getNow
  pure $ currentTime `timestampPlusSeconds` 1000

-- | Advance time by @n@ seconds, while calling some view entrypoint to make sure
-- the cumulative buffers are filled every second.
advanceSecs :: MonadNettest caps base m => Int -> ContractHandler Parameter st -> m ()
advanceSecs n cfmm = do
  consumer <- originateSimple @[CumulativesValue] "consumer" [] contractConsumer
  for_ [1..n] \_ -> do
    call cfmm (Call @"Observe") $ mkView [] consumer
    advanceTime (sec 1)

-- | Converts a michelson doubly linked list (encoded as a big_map) to a list.
-- The linked list is traversed starting from `minTickIndex` and
-- following @next@ pointers.
mapToList :: forall caps base m. MonadNettest caps base m => TickMap -> m [(TickIndex, TickState)]
mapToList tickMap =
  go minTickIndex [] (bmMap tickMap)
  where
    go :: TickIndex -> [(TickIndex, TickState)] -> Map TickIndex TickState -> m [(TickIndex, TickState)]
    go currentIndex acc ll = do
      if currentIndex == maxTickIndex + 1
        -- we've reached the end of the list
        then do
          assert (Map.notMember currentIndex ll) $
            "Expected linked-list not to contain a tick index greater than `maxTickIndex`, but it did: " <> build currentIndex
          pure $ reverse acc
        -- we haven't reached the end of the list
        else case Map.lookup currentIndex ll of
          Just current -> go (current & tsNext) ((currentIndex, current) : acc) ll
          Nothing -> failure $ "Index " +| currentIndex |+ " does not exist in the linked-list."

-- | Converts a michelson doubly linked list (encoded as a big_map) to a list.
-- The linked list is traversed starting from `maxTickIndex` and
-- following @prev@ pointers.
--
-- The indices are returned in ascending order, e.g.
-- > mapToList tickMap == mapToListReverse tickMap
mapToListReverse :: forall caps base m. MonadNettest caps base m => TickMap -> m [(TickIndex, TickState)]
mapToListReverse tickMap =
  go maxTickIndex [] (bmMap tickMap)
  where
    go :: TickIndex -> [(TickIndex, TickState)] -> Map TickIndex TickState -> m [(TickIndex, TickState)]
    go currentIndex acc ll = do
      if currentIndex == minTickIndex - 1
        -- we've reached the end of the list
        then do
          assert (Map.notMember currentIndex ll) $
            "Expected linked-list not to contain a tick index lower than `minTickIndex`, but it did: " <> build currentIndex
          pure acc
        -- we haven't reached the end of the list
        else case Map.lookup currentIndex ll of
          Just current -> go (current & tsPrev) ((currentIndex, current) : acc) ll
          Nothing -> failure $ "Index " +| currentIndex |+ " does not exist in the linked-list."


----------------------------------------------------------------------------
-- FA2 helpers
----------------------------------------------------------------------------

deriveManyRPC "FA2.BalanceResponseItem" []
deriving via (GenericBuildable BalanceRequestItemRPC) instance Buildable BalanceRequestItemRPC
deriving via (GenericBuildable BalanceResponseItemRPC) instance Buildable BalanceResponseItemRPC

-- | Retrieve the FA2 balance for a given account.
balanceOf
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr)
  => ContractHandler FA2.FA2SampleParameter storage -> FA2.TokenId -> addr -> m Natural
balanceOf fa2 tokenId account = do
  consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
  call fa2 (Call @"Balance_of") (FA2.mkFA2View [FA2.BalanceRequestItem (toAddress account) tokenId] consumer)
  consumerStorage <- getStorage consumer

  case consumerStorage of
    [[BalanceResponseItemRPC _ bal]] -> pure bal
    _ -> failure $ unlinesF
          [ "Expected consumer storage to have exactly 1 balance response, with exactly 1 item."
          , "Consumer storage:"
          , indentF 2 $ build consumerStorage
          ]

----------------------------------------------------------------------------
-- Other utils
----------------------------------------------------------------------------

-- | Integer division, like `div`, but rounds up instead of truncating towards negative infinity.
divUp :: Integer -> Integer -> Integer
divUp x y = ceiling $ fromIntegral @Integer @Double x / fromIntegral @Integer @Double y
infixl 7 `divUp`
