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
  , TokenInfo(..)
  , simpleFA2Storage
  , originateFA2
  , balanceOf
  , balancesOf
  , updateOperator
  , updateOperators
  , transferToken
  , transferToken'
  , transferTokens
  -- * Segmented CFMM helpers
  , originateSegCFMM
  , prepareSomeSegCFMM
  , prepareSomeSegCFMM'
  , observe
  , setPositionParamSimple
  , updatePositionParamSimple
  , gettingCumulativesInsideDiff
  , convertTokens
  , mkDeadline
  , advanceSecs
  , mapToList
  , mapToListReverse
  , collectAllFees
  , collectFees
  , lastRecordedCumulatives
  , (@~=)
  , inTicksRange
  -- * Other utils
  , divUp
  , isInRange
  , isInRangeNat
  , groupAdjacent
  , isMonothonic
  ) where

import Prelude

import Data.Coerce (coerce)
import Data.Ix (Ix, inRange)
import qualified Data.List as List
import qualified Data.Map as Map
import Fmt (Buildable(build), GenericBuildable(..), indentF, listF, unlinesF, (+|), (|+))
import Hedgehog hiding (assert, failure)
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert, map, transferTokens)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Pure (PureM, runEmulated)
import Tezos.Address (ta)
import Tezos.Core (timestampPlusSeconds)
import Time (sec)

import SegCFMM.Types as CFMM
import Test.SegCFMM.Contract (TokenType(..), segCFMMContract)
import Test.SegCFMM.Storage as CFMM

deriving stock instance Eq CumulativesBuffer
deriving stock instance Eq Storage

data TokenInfo = TokenInfo
  { tiTokenId :: FA2.TokenId
  , tiToken :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  }

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

originateFA2
  :: MonadNettest caps base m
  => [Address]
  -> FA2.TokenId
  -> m TokenInfo
originateFA2 accounts tokenId@(FA2.TokenId tid) = do
  let st = FA2.Storage
        { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, tokenId), 1_e15)
        , sOperators = mempty
        , sTokenMetadata = mempty
        }
  let name = "fa2-" <> show tid
  tokenAddr <- originateSimple name st (FA2.fa2Contract def { FA2.cAllowedTokenIds = [tokenId] })
  pure $ TokenInfo tokenId tokenAddr

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
  => [Address]
  -> m ( ContractHandler CFMM.Parameter CFMM.Storage
       , (TokenInfo, TokenInfo)
       )
prepareSomeSegCFMM accounts =
  prepareSomeSegCFMM' accounts Nothing Nothing id

-- | Like 'prepareSomeSegCFMM' but allows overriding some defaults.
prepareSomeSegCFMM'
  :: MonadNettest caps base m
  => [Address]
  -> Maybe (TokenInfo, TokenInfo)
  -> Maybe CFMM.Storage
  -> (Constants -> Constants)
  -> m ( ContractHandler CFMM.Parameter CFMM.Storage
       , (TokenInfo, TokenInfo)
       )
prepareSomeSegCFMM' accounts tokensInfoMb initialStorageMb modifyConstants = do

  tokensInfo@(TokenInfo xTokenId xToken, TokenInfo yTokenId yToken) <-
    case tokensInfoMb of
      Just tokensInfo -> pure tokensInfo
      Nothing -> forEach (FA2.TokenId 0, FA2.TokenId 1) $ originateFA2 accounts

  let initialStorage = fromMaybe defaultStorage initialStorageMb
  let initialStorage' = initialStorage
        { sConstants = modifyConstants $ (sConstants initialStorage)
          { cXTokenAddress = toAddress xToken
          , cXTokenId = xTokenId
          , cYTokenAddress = toAddress yToken
          , cYTokenId = yTokenId
          }
        }
  cfmm <- originateSegCFMM FA2 FA2 initialStorage'

  forM_ accounts $ \account ->
    withSender account do
      updateOperator xToken account (toAddress cfmm) xTokenId True
      updateOperator yToken account (toAddress cfmm) yTokenId True

  return
    ( cfmm
    , tokensInfo
    )

observe :: (HasCallStack, MonadEmulated caps base m) => ContractHandler Parameter st -> m CumulativesValue
observe cfmm = do
  currentTime <- getNow
  consumer <- originateSimple @[CumulativesValue] "consumer" [] contractConsumer
  call cfmm (Call @"Observe") $ mkView [currentTime] consumer
  getFullStorage consumer >>= \case
    [[cv]] -> pure cv
    _ -> failure "Expected to get exactly 1 CumulativeValue"

setPositionParamSimple :: (TickIndex, TickIndex) -> Natural -> SetPositionParam
setPositionParamSimple (sppLowerTickIndex, sppUpperTickIndex) sppLiquidity =
  SetPositionParam
  { sppLowerTickIndex
  , sppUpperTickIndex
  , sppLowerTickWitness = minTickIndex
  , sppUpperTickWitness = minTickIndex
  , sppLiquidity
  , sppDeadline = [timestampQuote| 20021-01-01T00:00:00Z |]
  , sppMaximumTokensContributed = 1e100
  }

updatePositionParamSimple :: PositionId -> Integer -> UpdatePositionParam
updatePositionParamSimple uppPositionId uppLiquidityDelta =
  UpdatePositionParam
  { uppPositionId
  , uppLiquidityDelta
  , uppToX = receiver
  , uppToY = receiver
  , uppDeadline = [timestampQuote| 20021-01-01T00:00:00Z |]
  , uppMaximumTokensContributed = 1e100
  }
  where
    receiver = [ta|tz1QCtwyKA4S8USgYRJRghDNYLHkkQ3S1yAU|]

-- | Get the diff of cumulatives_inside at given ticks range between two given
-- timestamps.
gettingCumulativesInsideDiff
  :: (MonadEmulated caps base m, HasCallStack)
  => ContractHandler Parameter Storage
  -> (TickIndex, TickIndex)
  -> m ()
  -> m CumulativesInsideSnapshot
gettingCumulativesInsideDiff cfmm (loTick, hiTick) action = do
  consumer <- originateSimple "consumer" [] contractConsumer

  call cfmm (Call @"Snapshot_cumulatives_inside") $
    SnapshotCumulativesInsideParam loTick hiTick (toContractRef consumer)
  action
  call cfmm (Call @"Snapshot_cumulatives_inside") $
    SnapshotCumulativesInsideParam loTick hiTick (toContractRef consumer)

  getFullStorage consumer >>= \case
    [s2, s1] -> return (subCumulativesInsideSnapshot s2 s1)
    _ -> failure "Expected exactly 2 elements"

-- | Convert given amount of X or Y tokens
--
-- Positive value will increase the current tick index, and negative value will
-- decrease it.
convertTokens
  :: (MonadEmulated caps base m, HasCallStack)
  => ContractHandler Parameter Storage
  -> Integer
  -> m ()
convertTokens cfmm tokens =
  case tokens `Prelude.compare` 0 of
    EQ -> pass
    GT -> call cfmm (Call @"Y_to_x") YToXParam
      { ypDy = 2
      , ypDeadline = [timestampQuote| 20021-01-01T00:00:00Z |]
      , ypMinDx = 0
      , ypToDx = receiver
      }
    LT -> call cfmm (Call @"X_to_y") XToYParam
      { xpDx = 2
      , xpDeadline = [timestampQuote| 20021-01-01T00:00:00Z |]
      , xpMinDy = 0
      , xpToDy = receiver
      }
  where
    receiver = [ta|tz1QCtwyKA4S8USgYRJRghDNYLHkkQ3S1yAU|]

-- | Create a valid deadline
mkDeadline :: MonadNettest caps base m => m Timestamp
mkDeadline = do
  currentTime <- getNow
  pure $ currentTime `timestampPlusSeconds` 1000

-- | Advance time by @n@ seconds, while calling some view entrypoint to make sure
-- the cumulative buffers are filled every second.
advanceSecs :: MonadNettest caps base m => Natural -> [ContractHandler Parameter st] -> m ()
advanceSecs n cfmms = do
  consumer <- originateSimple @[CumulativesValue] "consumer" [] contractConsumer
  for_ [1..n] \_ -> do
    advanceTime (sec 1)
    for_ cfmms \cfmm -> call cfmm (Call @"Observe") $ mkView [] consumer

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

-- | Collect fees from all positions.
collectAllFees :: (HasCallStack, MonadEmulated caps base m) => ContractHandler Parameter Storage -> Address -> m ()
collectAllFees cfmm receiver = do
  st <- getFullStorage cfmm
  for_ (toPairs $ bmMap $ sPositions st) \(posId, pos) -> do
    collectFees cfmm receiver posId (psOwner pos)

-- | Collect fees from a single position.
collectFees
  :: (HasCallStack, MonadEmulated caps base m)
  => ContractHandler Parameter Storage
  -> Address
  -> PositionId
  -> Address
  -> m ()
collectFees cfmm receiver posId posOwner = do
  deadline <- mkDeadline
  withSender posOwner do
    call cfmm (Call @"Update_position")
      UpdatePositionParam
        { uppPositionId = posId
        , uppLiquidityDelta = 0
        , uppToX = receiver
        , uppToY = receiver
        , uppDeadline = deadline
        , uppMaximumTokensContributed = PerToken 0 0
        }

-- | Get last recorded cumulative values from storage.
lastRecordedCumulatives
  :: forall caps base m. MonadNettest caps base m
  => AsRPC Storage -> m TimedCumulatives
lastRecordedCumulatives s = do
  let buffer = sCumulativesBufferRPC s
  getBigMapValue (cbMapRPC buffer) (cbLastRPC buffer)

-- | Check two values with sufficiently large precision are approximately equal.
infix 1 @~=
(@~=)
    :: (HasCallStack, Integral a, Buildable a, KnownNat n, MonadNettest caps base m)
    => X n a -> X n a -> m ()
actual @~= expected = do
  let X a = adjustScale @30 actual
  let X b = adjustScale @30 expected
  assert (a - 1 <= b && b <= a + 1) $
    unlinesF
      [ "Failed approximate comparison"
      , "━━ Expected (rhs) ━━"
      , build expected
      , "━━ Got (lhs) ━━"
      , build actual
      ]

-- | Check whether given tick index is within the given range.
--
-- CFMM contract treats tick ranges as half-open @[lo, hi)@ ranges.
inTicksRange :: TickIndex -> (TickIndex, TickIndex) -> Bool
inTicksRange (TickIndex x) (TickIndex l, TickIndex r) =
  l <= x && x < r

----------------------------------------------------------------------------
-- Other utils
----------------------------------------------------------------------------

-- | Integer division, like `div`, but rounds up instead of truncating towards negative infinity.
divUp :: Integer -> Integer -> Integer
divUp x y = ceiling $ fromIntegral @Integer @Double x / fromIntegral @Integer @Double y
infixl 7 `divUp`

-- | @x `isInRange` y $ (down, up)@ checks that @x@ is in the range @[y - down, y + up]@.
isInRange
  :: (HasCallStack, MonadNettest caps base m, Ix a, Num a, Buildable a)
  => a -> a -> (a, a) -> m ()
isInRange x y (marginDown, marginUp) =
  checkCompares (y - marginDown, y + marginUp) inRange x

-- | Similar to `isInRange`, but checks that the lower bound cannot be less than 0.
isInRangeNat :: (HasCallStack, MonadNettest caps base m, Coercible nat Natural) => nat -> nat -> (Natural, Natural) -> m ()
isInRangeNat (coerce -> x) (coerce -> y) (marginDown, marginUp) = do
  let upperBound = y + marginUp
  let lowerBound =
        -- check for underflows.
        if marginDown <= y
          then y - marginDown
          else 0
  checkCompares (lowerBound, upperBound) inRange x

groupAdjacent :: [a] -> [(a, a)]
groupAdjacent l = [ (a1, a2) | a1 : a2 : _ <- List.tails l ]

-- | Check that values grow monothonically (non-strictly).
isMonothonic
  :: (HasCallStack, MonadNettest caps base m, Ord a, Buildable a)
  => [a] -> m ()
isMonothonic l =
  assert (l == sort l) ("Values do not grow monothonically: " +| listF l |+ "")
