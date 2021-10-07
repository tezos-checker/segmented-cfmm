-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Util
  (
  -- * Cleveland helpers
    clevelandProp
  , evalJust
  -- * FA2 helpers
  , simpleFA2Storage
  -- * Segmented CFMM helpers
  , originateSegCFMM
  , prepareSomeSegCFMM
  , prepareSomeSegCFMM'
  , mkDeadline
  , advanceSecs
  , mapToList
  , mapToListReverse
  -- * FA2 helpers
  , balanceOf
  , balancesOf
  , updateOperator
  , updateOperators
  , transferToken
  , transferToken'
  , transferTokens
  -- * Other utils
  , divUp
  ) where

import Prelude

import qualified Data.Map as Map
import Fmt (Buildable(build), GenericBuildable(..), indentF, unlinesF, (+|), (|+))
import Hedgehog hiding (assert, failure)
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert, map, transferTokens)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Pure (PureM, runEmulated)
import Tezos.Core (timestampPlusSeconds)
import Time (sec)

import FA2.Types
import SegCFMM.Types as CFMM
import Test.SegCFMM.Contract (TokenType(..), segCFMMContract)
import Test.SegCFMM.Storage as CFMM

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
-- FA2 helpers
----------------------------------------------------------------------------

simpleFA2Storage :: [Address] -> FA2.TokenId -> FA2.Storage
simpleFA2Storage addresses tokenId = FA2.Storage
  { sLedger = mkBigMap $ map (\addr -> ((addr,tokenId), 100000)) addresses
  , sOperators = mempty
  , sTokenMetadata = mempty
  }

----------------------------------------------------------------------------
-- Segmented CFMM helpers
----------------------------------------------------------------------------

originateSegCFMM
  :: MonadNettest caps base m
  => TokenType -> TokenType -> Storage
  -> m (ContractHandler Parameter Storage)
originateSegCFMM xTokenType yTokenType storage = do
  originateSimple "Segmented CFMM" storage $ segCFMMContract xTokenType yTokenType

-- | Originate some CFMM contract.
--
-- This will originate the necessary FA2 tokens and the CFMM contract itself
-- to operate on them.
prepareSomeSegCFMM
  :: MonadNettest caps base m
  => Address
  -> m ( ContractHandler CFMM.Parameter CFMM.Storage
       , (FA2Token, FA2Token)
       )
prepareSomeSegCFMM liquidityProvider = prepareSomeSegCFMM' [liquidityProvider]

-- | Like 'prepareSomeSegCFMM' but accepts multiple (or no) liquidity providers.
prepareSomeSegCFMM'
  :: MonadNettest caps base m
  => [Address]
  -> m ( ContractHandler CFMM.Parameter CFMM.Storage
       , (FA2Token, FA2Token)
       )
prepareSomeSegCFMM' liquidityProviders = do
  let xTokenId = FA2.TokenId 0
  let yTokenId = FA2.TokenId 1
  let xFa2storage = simpleFA2Storage liquidityProviders xTokenId
  let yFa2storage = simpleFA2Storage liquidityProviders yTokenId
  xToken <- originateSimple "fa2-X" xFa2storage
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
  yToken <- originateSimple "fa2-Y" yFa2storage
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

  let initialSt = CFMM.defaultStorage
        { CFMM.sConstants = (CFMM.sConstants CFMM.defaultStorage)
          { CFMM.cXTokenAddress = toAddress xToken
          , CFMM.cXTokenId = xTokenId
          , CFMM.cYTokenAddress = toAddress yToken
          , CFMM.cYTokenId = yTokenId
          }
        }
  cfmm <- originateSegCFMM FA2 FA2 initialSt

  forM_ liquidityProviders $ \lp ->
    withSender lp do
      call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam lp (toAddress cfmm) xTokenId]
      call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam lp (toAddress cfmm) yTokenId]

  return
    ( cfmm
    , ( FA2Token (toAddress xToken) xTokenId
      , FA2Token (toAddress yToken) yTokenId
      )
    )

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

-- | Retrieve the FA2 balance for a given account and token.
balanceOf
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr, FA2.ParameterC param)
  => ContractHandler param storage -> FA2.TokenId -> addr -> m Natural
balanceOf fa2 tokenId account = balancesOf fa2 [tokenId] account >>= \case
  [bal] -> return bal
  bals  -> failure $ unlinesF
    [ "Expected consumer storage to have exactly 1 item in its balance response."
    , "Balance response items:"
    , indentF 2 $ build bals
    ]

-- | Retrieve the FA2 balances for a given account and tokens.
balancesOf
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr, FA2.ParameterC param)
  => ContractHandler param storage -> [FA2.TokenId] -> addr -> m [Natural]
balancesOf fa2 tokenIds account = do
  consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
  call fa2 (Call @"Balance_of") (FA2.mkFA2View (map (FA2.BalanceRequestItem (toAddress account)) tokenIds) consumer)
  getStorage consumer >>= \case
    [bals] -> pure $ map (\(BalanceResponseItemRPC _ bal) -> bal) bals
    consumerStorage -> failure $ unlinesF
      [ "Expected consumer storage to have exactly 1 balance response."
      , "Consumer storage:"
      , indentF 2 $ build consumerStorage
      ]

-- | Update a single operator for an FA2 contract.
updateOperator
  :: (HasCallStack, MonadNettest caps base m, FA2.ParameterC param)
  => ContractHandler param storage
  -> Address     -- ^ owner
  -> Address     -- ^ operator
  -> FA2.TokenId -- ^ token id
  -> Bool        -- ^ operation: 'True' to add, 'False' to remove
  -> m ()
updateOperator fa2 opOwner opOperator opTokenId doAdd = do
  let opParam = FA2.OperatorParam{..}
      updOperation = if doAdd then FA2.AddOperator else FA2.RemoveOperator
  updateOperators fa2 [updOperation opParam]

-- | Update operators for an FA2 contract.
updateOperators
  :: (HasCallStack, MonadNettest caps base m, FA2.ParameterC param)
  => ContractHandler param storage
  -> FA2.UpdateOperatorsParam
  -> m ()
updateOperators fa2 operatorUpdates =
  call fa2 (Call @"Update_operators") operatorUpdates

-- | Transfer one token, in any amount, in an FA2 contract.
transferToken
  :: (HasCallStack, MonadNettest caps base m, FA2.ParameterC param)
  => ContractHandler param storage
  -> Address     -- ^ source
  -> Address     -- ^ destination
  -> FA2.TokenId -- ^ token id
  -> Natural     -- ^ amount
  -> m ()
transferToken fa2 tiFrom tdTo tdTokenId tdAmount = do
  let tiTxs = [FA2.TransferDestination{..}]
  transferTokens fa2 [FA2.TransferItem{..}]

-- | Transfer a single token, also in amount, in an FA2 contract.
transferToken'
  :: (HasCallStack, MonadNettest caps base m, FA2.ParameterC param)
  => ContractHandler param storage
  -> Address     -- ^ source
  -> Address     -- ^ destination
  -> FA2.TokenId -- ^ token id
  -> m ()
transferToken' fa2 tiFrom tdTo tdTokenId = transferToken fa2 tiFrom tdTo tdTokenId 1

-- | Transfer tokens in an FA2 contract.
transferTokens
  :: (HasCallStack, MonadNettest caps base m, FA2.ParameterC param)
  => ContractHandler param storage
  -> FA2.TransferParams
  -> m ()
transferTokens fa2 transferParams = call fa2 (Call @"Transfer") transferParams

----------------------------------------------------------------------------
-- Other utils
----------------------------------------------------------------------------

-- | Integer division, like `div`, but rounds up instead of truncating towards negative infinity.
divUp :: Integer -> Integer -> Integer
divUp x y = ceiling $ fromIntegral @Integer @Double x / fromIntegral @Integer @Double y
infixl 7 `divUp`
