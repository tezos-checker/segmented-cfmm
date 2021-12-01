-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Util
  (
  -- * Cleveland helpers
    clevelandProp
  , evalJust
  , forAllTokenTypeCombinations
  , forAllTokenTypeCombinationsOnNetwork
  , propOnNetwork
  -- * FA2 helpers
  , TokenInfo(TokenInfo)
  , originateFA2
  , originateTokenContracts
  , originateBalanceConsumer
  , originateBalanceConsumers
  , balancesOfMany
  , balancesOf
  , balanceOf
  , updateOperator
  , updateOperators
  , transferToken
  , transferToken'
  , transferTokens
  -- * Segmented CFMM helpers
  , OriginationParams(..)
  , prepareSomeSegCFMM
  , observe
  , setPosition
  , updatePosition
  , xtoy
  , ytox
  , gettingCumulativesInsideDiff
  , validDeadline
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
  , Lorentz.def
  ) where

import Prelude

import Control.Lens as Lens
import Data.Coerce (coerce)
import Data.Ix (Ix, inRange)
import qualified Data.List as List
import qualified Data.Map as Map
import Fmt (Buildable(build), GenericBuildable(..), indentF, listF, unlinesF, (+|), (|+))
import Hedgehog hiding (assert, failure)
import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Indigo.Contracts.ManagedLedger as FA12
import Lorentz hiding (assert, map, take, transferTokens)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Morley.Nettest.Caps (MonadOps)
import Morley.Nettest.Pure (PureM, runEmulated)
import Morley.Nettest.Tasty
  (nettestScenarioCaps, nettestScenarioOnEmulator, nettestScenarioOnNetworkCaps)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Time (sec)
import Util.Named ((.!))

import SegCFMM.Types as CFMM
import Test.SegCFMM.Contract
import Test.SegCFMM.Storage as CFMM

deriving stock instance Eq CumulativesBuffer
deriving stock instance Eq Storage

data TokenInfo where
  TokenInfo :: FA2.ParameterC param => FA2.TokenId -> ContractHandler param st -> TokenInfo
  TokenInfo_12 :: ContractHandler FA12.Parameter FA12.Storage -> TokenInfo

-- | Stores all the information needed to perform a balance request.
data BalanceConsumer where
  BalanceConsumerFA2
    :: FA2.ParameterC param
    => FA2.TokenId
    -> ContractHandler param st
    -> ContractHandler [FA2.BalanceResponseItem] [[FA2.BalanceResponseItem]]
    -> BalanceConsumer
  BalanceConsumerFA12
    :: ContractHandler FA12.Parameter FA12.Storage
    -> ContractHandler Natural [Natural]
    -> BalanceConsumer

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

-- | Runs a test, for each possible token type combination, on the emulator.
forAllTokenTypeCombinations :: TestName -> ((TokenType, TokenType) -> TestTree) -> TestTree
forAllTokenTypeCombinations testName mkTest =
  testGroup testName $ do
    x <- xTokenTypes
    y <- yTokenTypes
    pure $ mkTest (x, y)

-- | Runs a test, for each possible token type combination, on the emulator.
-- For the (FA, CTEZ) combination, the test will also be run on a network.
forAllTokenTypeCombinationsOnNetwork
  :: TestName
  -> (forall m. Monad m => (TokenType, TokenType) -> NettestT m ())
  -> TestTree
forAllTokenTypeCombinationsOnNetwork testName mkTest =
  testGroup testName do
    x <- xTokenTypes
    y <- yTokenTypes
    let tokenTypes = (x, y)
    if tokenTypes == defaultTokenTypes
      then one $ nettestScenarioCaps (show tokenTypes) $ mkTest tokenTypes
      else one $ nettestScenarioOnEmulator (show tokenTypes) \_ -> uncapsNettest (mkTest tokenTypes)

-- | Runs a property test:
--   * 100 times on the emulator with arbitrary data
--   * Once on a network with fixed data
propOnNetwork
  :: TestName
  -> PropertyT IO a
  -- ^ Generate arbitrary data to run the test on an emulator
  -> a
  -- ^ Fixed data to run the test on a network
  -> (forall m. Monad m => a -> NettestT m ())
  -> TestTree
propOnNetwork testName mkData constantData mkTest =
  testGroup testName
    [ networkTest
    , propTest
    ]
  where
    networkTest =
      nettestScenarioOnNetworkCaps "Unit" $ mkTest constantData
    propTest =
      testProperty "Property" $ property $ do
        testData <- mkData
        nettestTestProp . uncapsNettest $ mkTest testData

----------------------------------------------------------------------------
-- FA2 helpers
----------------------------------------------------------------------------

defaultBalance :: Natural
defaultBalance = 1_e15

originateFA2
  :: (HasCallStack, MonadOps m)
  => [Address]
  -> FA2.TokenId
  -> m (ContractHandler FA2.FA2SampleParameter FA2.Storage)
originateFA2 accounts tokenId@(FA2.TokenId tid) = do
  let st = FA2.Storage
        { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, tokenId), defaultBalance)
        , sOperators = mempty
        , sTokenMetadata = mempty
        }
  let name = "fa2-" <> show tid
  originateSimple name st (FA2.fa2Contract def { FA2.cAllowedTokenIds = [tokenId] })

{-# ANN originateFA12 ("HLint: ignore Use tuple-section" :: Text) #-}
originateFA12
  :: (HasCallStack, MonadOps m)
  => [Address]
  -> Address
  -> m (ContractHandler FA12.Parameter FA12.Storage)
originateFA12 accounts admin = do
  let yCtezStorage = FA12.mkStorage admin $
        Map.fromList $ accounts <&> \acct -> (acct, defaultBalance)
  originateSimple "ctez" yCtezStorage FA12.managedLedgerContract

originateTokenContracts
  :: (HasCallStack, MonadNettest caps base m
     , Lens.Each tokenTypesAndIds tokenInfos (TokenType, FA2.TokenId) TokenInfo
     , Lens.Each tokenTypesAndIds tokenTypesAndIds (TokenType, FA2.TokenId) (TokenType, FA2.TokenId)
     )
  => [Address]
  -> tokenTypesAndIds
  -> m tokenInfos
originateTokenContracts accounts tokenTypesAndIds = do
  if allOf (each . _1) (== FA2) tokenTypesAndIds
    then
      -- If we only need FA2 tokens, then we don't need to create an `admin` account
      inBatch do
        forEach tokenTypesAndIds \(_, tokenId) -> TokenInfo tokenId <$> originateFA2 accounts tokenId
    else do
      -- If we need any FA1.2 tokens, then we need an admin.
      admin <- newAddress "admin"
      inBatch do
        forEach tokenTypesAndIds \(tokenType, tokenId) ->
          case tokenType of
            FA2 -> TokenInfo tokenId <$> originateFA2 accounts tokenId
            _ -> TokenInfo_12 <$> originateFA12 accounts admin

deriveManyRPC "FA2.BalanceResponseItem" []
deriving via (GenericBuildable BalanceRequestItemRPC) instance Buildable BalanceRequestItemRPC
deriving via (GenericBuildable BalanceResponseItemRPC) instance Buildable BalanceResponseItemRPC

-- | Originate a contract capable of storing the responses of FA2 / FA1.2 balance requests.
originateBalanceConsumer :: (HasCallStack, MonadOps m) => TokenInfo -> m BalanceConsumer
originateBalanceConsumer = \case
  TokenInfo tokenId tokenAddr -> do
    consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
    pure $ BalanceConsumerFA2 tokenId tokenAddr consumer
  TokenInfo_12 tokenAddr -> do
    consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @Natural)
    pure $ BalanceConsumerFA12 tokenAddr consumer

-- | Originate many contracts capable of storing the responses of FA2 / FA1.2 balance requests in a batch.
originateBalanceConsumers
  :: ( HasCallStack
     , MonadNettest caps base m
     , Lens.Each tokens consumers TokenInfo BalanceConsumer
     )
  => tokens -> m consumers
originateBalanceConsumers tokenInfos =
  inBatch $ forEach tokenInfos originateBalanceConsumer

-- | Retrieve the FA2 / FA1.2 balances for the given accounts.
balancesOfMany
  :: ( HasCallStack
     , MonadNettest caps base m
     , ToAddress addr
     , Lens.Each addresses addresses addr addr
     , Lens.Each addresses balances addr Natural
     , Lens.Each balanceConsumers balanceConsumers BalanceConsumer BalanceConsumer
     , Lens.Each balanceConsumers allBalances BalanceConsumer balances
     )
  => balanceConsumers
  -> addresses
  -> m allBalances
balancesOfMany balanceConsumers accounts = do
  let accountsList = toListOf each accounts
  -- Perform all balance requests in a batch.
  inBatch do
    forOf_ each balanceConsumers \case
      BalanceConsumerFA2 tokenId tokenAddr consumer -> do
        let param = accountsList <&> \acc -> FA2.BalanceRequestItem (toAddress acc) tokenId
        call tokenAddr (Call @"Balance_of") (FA2.mkFA2View param consumer)
      BalanceConsumerFA12 tokenAddr consumer -> do
        for_ accountsList \acc ->
          call tokenAddr (Call @"GetBalance") (mkView (#owner .! toAddress acc) consumer)

  -- Check the consumers' storages.
  forEach balanceConsumers \balanceConsumer -> do
    balances <-
      case balanceConsumer of
        BalanceConsumerFA2 _ _ consumer -> do
          getStorage consumer >>= \case
            (response : _) -> pure $
              response <&> (\(BalanceResponseItemRPC _ bal) -> bal) & take (length accountsList)
            consumerStorage -> failure $ unlinesF
              [ "Expected consumer storage to have at least 1 balance response."
              , "Consumer storage:"
              , indentF 2 $ build consumerStorage
              ]
        BalanceConsumerFA12 _ consumer ->
          reverse . take (length accountsList) <$> getStorage consumer

    -- Replace each account in the input with its balance.
    pure $ accounts & unsafePartsOf each .~ balances

-- | Retrieve the balances of many FA2 / FA1.2 tokens for a single account.
balancesOf
  :: ( HasCallStack
     , MonadNettest caps base m
     , ToAddress addr
     , Lens.Each balanceConsumers balanceConsumers BalanceConsumer BalanceConsumer
     , Lens.Each balanceConsumers balances' BalanceConsumer (Identity Natural)
     , Lens.Each balances' balances (Identity Natural) Natural
     )
  => balanceConsumers
  -> addr -> m balances
balancesOf balanceConsumers account = do
  balances <- balancesOfMany balanceConsumers (Identity account)
  pure $ balances & each %~ runIdentity

-- | Retrieve the balance of a single FA2 / FA1.2 for a single account.
balanceOf
  :: ( HasCallStack
     , MonadNettest caps base m
     , ToAddress addr
     )
  => BalanceConsumer
  -> addr -> m Natural
balanceOf balanceConsumer account =
  runIdentity <$> balancesOf (Identity balanceConsumer) account

-- | Update a single operator for an FA2 contract.
updateOperator
  :: (HasCallStack, MonadOps m, FA2.ParameterC param)
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
  :: (HasCallStack, MonadOps m, FA2.ParameterC param)
  => ContractHandler param storage
  -> FA2.UpdateOperatorsParam
  -> m ()
updateOperators fa2 operatorUpdates =
  call fa2 (Call @"Update_operators") operatorUpdates

-- | Transfer one token, in any amount, in an FA2 contract.
transferToken
  :: (HasCallStack, MonadOps m, FA2.ParameterC param)
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
  :: (HasCallStack, MonadOps m, FA2.ParameterC param)
  => ContractHandler param storage
  -> Address     -- ^ source
  -> Address     -- ^ destination
  -> FA2.TokenId -- ^ token id
  -> m ()
transferToken' fa2 tiFrom tdTo tdTokenId = transferToken fa2 tiFrom tdTo tdTokenId 1

-- | Transfer tokens in an FA2 contract.
transferTokens
  :: (HasCallStack, MonadOps m, FA2.ParameterC param)
  => ContractHandler param storage
  -> FA2.TransferParams
  -> m ()
transferTokens fa2 transferParams = call fa2 (Call @"Transfer") transferParams

----------------------------------------------------------------------------
-- Segmented CFMM helpers
----------------------------------------------------------------------------

data OriginationParams = OriginationParams
  { opTokens :: Maybe (TokenInfo, TokenInfo)
    -- ^ Id and address of the X and Y tokens. If unspecified, new tokens will be created.
  , opModifyStorage :: CFMM.Storage -> CFMM.Storage
  , opModifyConstants :: Constants -> Constants
  }

instance Default OriginationParams where
  def = OriginationParams
    { opTokens = Nothing
    , opModifyStorage = id
    , opModifyConstants = id
    }

-- | Originate some CFMM contract.
--
-- This will originate the necessary FA2 tokens and the CFMM contract itself
-- to operate on them.
prepareSomeSegCFMM
  :: (HasCallStack, MonadNettest caps base m)
  => [Address]
  -> (TokenType, TokenType)
  -> OriginationParams
  -> m ( ContractHandler CFMM.Parameter CFMM.Storage
       , (TokenInfo, TokenInfo)
       )
prepareSomeSegCFMM accounts (xTokenType, yTokenType) (OriginationParams tokensInfoMb modifyStorage modifyConstants) = do

  tokensInfo@(x, y) :: (TokenInfo, TokenInfo) <-
    case tokensInfoMb of
      Just tokensInfo -> pure tokensInfo
      Nothing -> originateTokenContracts accounts ((xTokenType, FA2.TokenId 0), (yTokenType, FA2.TokenId 1))

  let initialStorage = modifyStorage defaultStorage
  let initialStorage' = initialStorage
        { sConstants = modifyConstants $ (sConstants initialStorage)
          { cXTokenAddress = case x of TokenInfo _ addr -> toAddress addr; TokenInfo_12 addr -> toAddress addr
          , cXTokenId = case x of TokenInfo tokenId _ -> tokenId; TokenInfo_12 _ -> FA2.TokenId 0
          , cYTokenAddress = toAddress case y of TokenInfo _ addr -> toAddress addr; TokenInfo_12 addr -> toAddress addr
          , cYTokenId = case y of TokenInfo tokenId _ -> tokenId; TokenInfo_12 _ -> FA2.TokenId 0
          }
        }

  cfmm <- originateSimple "cfmm" initialStorage' $ segCFMMContract xTokenType yTokenType

  for_ accounts $ \account ->
    withSender account $ inBatch do
      for_ [x, y] \case
        TokenInfo_12 tokenAddr -> call tokenAddr (Call @"Approve") (#spender .! toAddress cfmm, #value .! defaultBalance)
        TokenInfo tokenId tokenAddr -> updateOperator tokenAddr account (toAddress cfmm) tokenId True

  return (cfmm, tokensInfo)

observe :: (HasCallStack, MonadNettest caps base m) => ContractHandler Parameter st -> m CumulativesValue
observe cfmm = do
  currentTime <- getNow
  consumer <- originateSimple @[CumulativesValue] "observe-consumer" [] contractConsumer
  call cfmm (Call @"Observe") $ mkView [currentTime] consumer
  getStorage consumer >>= \case
    [[cv]] -> pure cv
    _ -> failure "Expected to get exactly 1 CumulativeValue"

-- | Utility function to make a simple call to @Set_position@ between the two
-- given 'TickIndex'.
--
-- It should succeed and a position be created if the given address was also
-- given to 'prepareSomeSegCFMM'.
setPosition
  :: (MonadOps m, HasCallStack)
  => ContractHandler Parameter Storage
  -> Natural
  -> (TickIndex, TickIndex)
  -> m ()
setPosition cfmm liquidity (lowerTickIndex, upperTickIndex) = do
  call cfmm (Call @"Set_position")
    SetPositionParam
      { sppLowerTickIndex = lowerTickIndex
      , sppUpperTickIndex = upperTickIndex
      , sppLowerTickWitness = minTickIndex
      , sppUpperTickWitness = minTickIndex
      , sppLiquidity = liquidity
      , sppDeadline = validDeadline
      , sppMaximumTokensContributed = PerToken defaultBalance defaultBalance
      }

updatePosition
  :: (MonadOps m, HasCallStack)
  => ContractHandler Parameter Storage
  -> Address
  -> Integer
  -> PositionId
  -> m ()
updatePosition cfmm receiver liquidityDelta positionId = do
  call cfmm (Call @"Update_position")
    UpdatePositionParam
      { uppPositionId = positionId
      , uppLiquidityDelta = liquidityDelta
      , uppToX = receiver
      , uppToY = receiver
      , uppDeadline = validDeadline
      , uppMaximumTokensContributed = PerToken defaultBalance defaultBalance
      }

xtoy
  :: (MonadOps m, HasCallStack)
  => ContractHandler Parameter Storage -> Natural -> Address -> m ()
xtoy cfmm dx receiver =
  call cfmm (Call @"X_to_y") XToYParam
    { xpDx = dx
    , xpDeadline = validDeadline
    , xpMinDy = 0
    , xpToDy = receiver
    }

ytox
  :: (MonadOps m, HasCallStack)
  => ContractHandler Parameter Storage -> Natural -> Address -> m ()
ytox cfmm dy receiver =
  call cfmm (Call @"Y_to_x") YToXParam
    { ypDy = dy
    , ypDeadline = validDeadline
    , ypMinDx = 0
    , ypToDx = receiver
    }

-- | Get the diff of cumulatives_inside at given ticks range between two given
-- timestamps.
gettingCumulativesInsideDiff
  :: (MonadNettest caps base m, HasCallStack)
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

  getStorage consumer >>= \case
    [s2, s1] -> return (subCumulativesInsideSnapshot s2 s1)
    _ -> failure "Expected exactly 2 elements"

validDeadline :: Timestamp
validDeadline = [timestampQuote| 20021-01-01T00:00:00Z |]

-- | Advance time by @n@ seconds, while calling some view entrypoint to make sure
-- the cumulative buffers are filled every second.
advanceSecs :: MonadNettest caps base m => Natural -> [ContractHandler Parameter st] -> m ()
advanceSecs n cfmms = do
  consumer <- originateSimple @[CumulativesValue] "advance-secs-consumer" [] contractConsumer
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
  :: (HasCallStack, MonadNettest caps base m)
  => ContractHandler Parameter Storage
  -> Address
  -> PositionId
  -> Address
  -> m ()
collectFees cfmm receiver posId posOwner = do
  withSender posOwner do
    call cfmm (Call @"Update_position")
      UpdatePositionParam
        { uppPositionId = posId
        , uppLiquidityDelta = 0
        , uppToX = receiver
        , uppToY = receiver
        , uppDeadline = validDeadline
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
