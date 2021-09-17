-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Types mirrored from LIGO implementation.
module SegCFMM.Types
  ( X (..)
  , mkX

  , Storage (..)
  , Parameter (..)
  , PerToken (..)
  , TickIndex (..)
  , TickState (..)
  , SetPositionParam (..)
  , XToYParam (..)
  , YToXParam (..)
  , XToXPrimeParam (..)
  ) where

import Universum

import Fmt (Buildable, GenericBuildable(..), build)

import Lorentz hiding (now)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface.ParameterInstances ()
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Morley.Client (AsRPC, deriveRPCWithStrategy)

-- | A value with @2^-n@ precision.
newtype X (n :: Nat) a = X
  { pickX :: a
    -- ^ Get the value multiplied by @2^n@.
  } deriving stock (Show, Eq, Generic)
    deriving newtype (IsoValue, HasAnnotation)

instance (Buildable a, KnownNat n) => Buildable (X n a) where
  build x = build (pickX x) <> " X 2^" <> build (powerOfX x)

type instance AsRPC (X n a) = X n a

powerOfX :: KnownNat n => X n a -> Natural
powerOfX (X{} :: X n a) = natVal (Proxy @n)

-- | Convert a fraction to 'X'.
mkX :: forall a n b. (KnownNat n, RealFrac a, Integral b) => a -> X n b
mkX = X . round . (* 2 ^ natVal (Proxy @n))

data Parameter
  = X_to_y XToYParam
    -- ^ Trade up to a quantity dx of asset x, receives dy
  | Y_to_x YToXParam
    -- ^ Trade up to a quantity dy of asset y, receives dx
  | X_to_x_prime XToXPrimeParam
    -- ^ Equivalent to token_to_token
  | Set_position SetPositionParam
    -- ^ Updates or creates a new position in the given range.
  | Get_position_info GetPositionInfoParam
    -- ^ Get information about position
  | Call_fa2 FA2.Parameter
    -- ^ Call FA2 interface
  | Snapshot_cumulatives_inside SnapshotCumulativesInsideParam
    -- ^ Get oracle values at given range.
  | Observe ObserveParam
    -- ^ Get oracle values at certain points of time.
  | Increase_observation_count Natural
    -- ^ Set the number of stored accumulators for geometric mean price oracle.

-- | Parameter of @X_to_Y@ entrypoints
data XToYParam = XToYParam
  { xpDx :: Natural
    -- ^ Sold tokens amount.
  , xpDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , xpMinDy :: Natural
    -- ^ Minimal expected number of tokens bought.
  , xpToDy :: Address
    -- ^ Recipient of Y tokens.
  }

-- | Parameter of @Y_to_X@ entrypoints
data YToXParam = YToXParam
  { ypDy :: Natural
    -- ^ Sold tokens amount.
  , ypDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , ypMinDx :: Natural
    -- ^ Minimal expected number of tokens bought.
  , ypToDx :: Address
    -- ^ Recipient of X tokens.
  }

-- | Parameter of @X_to_X_prime@ entrypoints
data XToXPrimeParam = XToXPrimeParam
  { xppDx :: Natural
    -- ^ Sold tokens amount.
  , xppXPrimeContract :: Address
    -- ^ Address of another segmented-cfmm contract.
  , xppDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , xppMinDxPrime :: Natural
    -- ^ Minimal expected number of tokens bought.
  , xppToDxPrime :: Address
    -- ^ Recipient of X tokens.
  }

data SetPositionParam = SetPositionParam
  { sppLowerTickIndex :: TickIndex
    -- ^ Lower tick
  , sppUpperTickIndex :: TickIndex
    -- ^ Upper tick
  , sppLowerTickWitness :: TickIndex
    -- ^ Index of an initialized lower tick lower than `sppIL` (to find it easily in the linked list).
  , sppUpperTickWitness :: TickIndex
    -- ^ Index of an initialized upper tick lower than `sppIU` (to find it easily in the linked list).
  , sppLiquidityDelta :: Integer
    -- ^ How to change liquidity of the position (if not yet exists, assumed to have 0 liquidity).
  , sppToX :: Address
    -- ^ Where to send freed X tokens, if any.
  , sppToY :: Address
    -- ^ Where to send freed Y tokens, if any.
  , sppDeadline :: Timestamp
    -- ^ The deadline for the request to be executed.
  , sppMaximumTokensContributed :: PerToken Natural
    -- ^ The maximum number of tokens to contribute.
    -- If a higher amount is required, the entrypoint fails.
  }

type ObserveParam = View [Timestamp] [CumulativesValue]

data PositionInfo = PositionInfo
  { piLiquidity :: Natural
  , piIndex :: PositionIndex
  }

data CumulativesInsideSnapshot = CumulativesInsideSnapshot
  { cisTickCumulativeInside :: Integer
  , cisSecondsPerLiquidityInside :: X 128 Natural
  , cisSecondsInside :: Natural
  }

data SnapshotCumulativesInsideParam = SnapshotCumulativesInsideParam
  { sciLowerTickIndex :: TickIndex
  , sciUpperTickIndex :: TickIndex
  , sciCallback :: ContractRef CumulativesInsideSnapshot
  }

type GetPositionInfoParam = View PositionId PositionInfo

-----------------------------------------------------------------
-- Storage
-----------------------------------------------------------------

data Storage = Storage
  { sLiquidity :: Natural
    -- ^ Virtual liquidity, the value L for which the curve locally looks like x * y = L^2
  , sSqrtPrice :: X 80 Natural
    -- ^ Square root of the virtual price, the value P for which P = x / y
  , sCurTickIndex :: TickIndex
    -- ^ Current tick index: The highest tick corresponding to a price less than or
    -- equal to sqrt_price^2, does not necessarily corresponds to a boundary.
  , sCurTickWitness :: TickIndex
    -- ^ The highest initialized tick lower than or equal to cur_tick_index
  , sFeeGrowth :: PerToken (X 128 Natural)
    -- ^ Represent the total amount of fees that have been earned per unit of
    -- virtual liquidity, over the entire history of the contract.
  , sTicks :: TickMap
    -- ^ Ticks' states.
  , sPositions :: PositionMap
    -- ^ Positions' states.
  , sPositionIndexes :: PositionIndexMap
  , sCumulativesBuffer :: CumulativesBuffer
    -- ^ Stored cumulative time-weighted values.

  , sMetadata :: TZIP16.MetadataMap BigMap
    -- ^ TZIP-16 metadata.
  , sNewPositionId :: PositionId
    -- ^ Incremental position id to be assigned to new position.
  , sOperators :: Operators
    -- ^ FA2 operators
  , sConstants :: Constants
  }

-- Needed by `sMetadata`
instance Buildable (ByteString) where
  build = build . show @Text

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT


data PerToken a = PerToken
  { ptX :: a
  , ptY :: a
  }

type instance AsRPC (PerToken a) = PerToken a

-- | Tick types, representing pieces of the curve offered between different tick segments.
newtype TickIndex = TickIndex Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable TickIndex


instance HasAnnotation TickIndex where
  annOptions = segCfmmAnnOptions

type instance AsRPC TickIndex = TickIndex


-- | Information stored for every initialized tick.
data TickState = TickState
  { tsPrev :: TickIndex
    -- ^ Index of the previous initialized tick.
  , tsNext :: TickIndex
    -- ^ Index of the next initialized tick.
  , tsLiquidityNet :: Integer
    -- ^ Track total amount of liquidity that is added/removed when
    -- this tick is crossed.
  , tsNPosition :: Natural
    -- ^ Number of positions that cover this tick.
  , tsSecondsOutside :: Natural
    -- ^ Overall number of seconds spent below or above this tick
    --   (below or above - depends on whether the current tick
    --    is below or above this tick).
  , tsTickCumulativeOutside :: Integer
    -- ^ Track tick index accumulated below or above this tick.
  , tsFeeGrowthOutside :: PerToken (X 128 Natural)
    -- ^ Track fees accumulated below or above this tick.
  , tsSecondsPerLiquidityOutside :: X 128 Natural
    -- ^ Track seconds-weighted 1/L value below or above this tick.
  , tsSqrtPrice :: X 80 Natural
    -- ^ Square root of the price associated with this tick.
  }

type TickMap = BigMap TickIndex TickState

-- | Position types, representing LP positions.
data PositionIndex = PositionIndex
  { piOwner :: Address
  , piLowerTickIndex :: TickIndex
    -- ^ Lower bound.
  , piUpperTickIndex :: TickIndex
    -- ^ Upper bound.
  } deriving stock (Ord, Eq)

data PositionState = PositionState
  { psLiquidity :: Natural
    -- ^ Amount of virtual liquidity that the position represented the last
    -- time it was touched. This amount does not reflect the fees that have
    -- been accumulated since the contract was last touched.
  , psFeeGrowthInsideLast :: PerToken (X 128 Natural)
    -- ^ Used to calculate uncollected fees.
  , psPositionId :: PositionId
    -- ^ When deleting a position_state, we also need to delete `position_index`
    -- in `store.position_indexes`. Storing `position_id` here allows us to delete that.
  }

newtype PositionId = PositionId Natural
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable PositionId

instance HasAnnotation PositionId where
  annOptions = segCfmmAnnOptions

type instance AsRPC PositionId = PositionId


-- | Map containing Liquidity providers.
type PositionMap = BigMap PositionIndex PositionState

-- | One-to-one relation from `postion_id` to `position_index`.
-- Used for querying `position_state` with just a `position_id`.
type PositionIndexMap = BigMap PositionId PositionIndex


-- | Return value of observation entrypoint.
data CumulativesValue = CumulativesValue
  { cvTickCumulative :: Integer
  , cvSecondsPerLiquidityCumulative :: X 128 Natural
  }

data TickCumulative = TickCumulative
  { tcSum :: Integer
  , tcBlockStartValue :: TickIndex
  }

data LpsCumulative = LpsCumulative
  { lcSum :: X 128 Natural
  , lcBlockStartLiquidityValue :: Natural
  }

data TimedCumulatives = TimedCumulatives
  { tcTime :: Timestamp
  , tcTick :: TickCumulative
  , tcLps :: LpsCumulative
  }

initTimedCumulatives :: TimedCumulatives
initTimedCumulatives = TimedCumulatives
  { tcTime = timestampFromSeconds 100
  , tcTick = TickCumulative 0 (TickIndex 0)
  , tcLps = LpsCumulative (mkX @Double 0) 1
  }

data CumulativesBuffer = CumulativesBuffer
  { tbMap :: BigMap Natural TimedCumulatives
  , tbFirst :: Natural
  , tbLast :: Natural
  , tbReservedLength :: Natural
  }

initCumulativesBuffer :: Natural -> CumulativesBuffer
initCumulativesBuffer extraReservedSlots = CumulativesBuffer
  { tbMap = mkBigMap $ foldMap (one . (, initTimedCumulatives)) [0 .. extraReservedSlots]
  , tbFirst = 0
  , tbLast = 0
  , tbReservedLength = extraReservedSlots + 1
  }

-- | Stored constants picked at origination
data Constants = Constants
  { cFeeBps :: Natural
  , cCtezBurnFeeBps :: Natural
  , cXTokenId :: Natural
  , cYTokenId :: Natural
  , cXTokenAddress :: Address
  , cYTokenAddress :: Address
  }

------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------

data Operator = Operator
  { oOwner :: Address
  , oOperator :: Address
  } deriving stock (Eq, Ord)

deriving via (GenericBuildable (a, b)) instance (Buildable a, Buildable b) => Buildable (a, b)

type Operators = BigMap Operator ()

instance Buildable () where
  build _ = "()"

-----------------------------------------------------------------
-- Helper
-----------------------------------------------------------------

segCfmmAnnOptions :: AnnOptions
segCfmmAnnOptions = defaultAnnOptions
  { fieldAnnModifier = dropPrefixThen toSnake }

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "XToYParam" ligoLayout
deriving via (GenericBuildable XToYParam) instance Buildable XToYParam
deriving anyclass instance IsoValue XToYParam
instance HasAnnotation XToYParam where
  annOptions = segCfmmAnnOptions

customGeneric "YToXParam" ligoLayout
deriving via (GenericBuildable YToXParam) instance Buildable YToXParam
deriving anyclass instance IsoValue YToXParam
instance HasAnnotation YToXParam where
  annOptions = segCfmmAnnOptions

customGeneric "XToXPrimeParam" ligoLayout
deriving via (GenericBuildable XToXPrimeParam) instance Buildable XToXPrimeParam
deriving anyclass instance IsoValue XToXPrimeParam
instance HasAnnotation XToXPrimeParam where
  annOptions = segCfmmAnnOptions

customGeneric "SetPositionParam" ligoLayout
deriving via (GenericBuildable SetPositionParam) instance Buildable SetPositionParam
deriving anyclass instance IsoValue SetPositionParam
instance HasAnnotation SetPositionParam where
  annOptions = segCfmmAnnOptions

customGeneric "PositionInfo" ligoLayout
deriving via (GenericBuildable PositionInfo) instance Buildable PositionInfo
deriving anyclass instance IsoValue PositionInfo
instance HasAnnotation PositionInfo where
  annOptions = segCfmmAnnOptions

customGeneric "Operator" ligoLayout
deriving via (GenericBuildable Operator) instance Buildable Operator
deriving anyclass instance IsoValue Operator
instance HasAnnotation Operator where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesInsideSnapshot" ligoLayout
deriving via (GenericBuildable CumulativesInsideSnapshot) instance Buildable CumulativesInsideSnapshot
deriving anyclass instance IsoValue CumulativesInsideSnapshot
instance HasAnnotation CumulativesInsideSnapshot where
  annOptions = segCfmmAnnOptions

customGeneric "SnapshotCumulativesInsideParam" ligoLayout
deriving via (GenericBuildable SnapshotCumulativesInsideParam) instance Buildable SnapshotCumulativesInsideParam
deriving anyclass instance IsoValue SnapshotCumulativesInsideParam
instance HasAnnotation SnapshotCumulativesInsideParam where
  annOptions = segCfmmAnnOptions

customGeneric "Parameter" ligoLayout
deriving via (GenericBuildable Parameter) instance Buildable Parameter
deriving anyclass instance IsoValue Parameter

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate


customGeneric "PerToken" ligoLayout
deriving via (GenericBuildable (PerToken a)) instance Buildable a => Buildable (PerToken a)
deriving anyclass instance IsoValue a => IsoValue (PerToken a)
instance HasAnnotation a => HasAnnotation (PerToken a) where
  annOptions = segCfmmAnnOptions

customGeneric "TickState" ligoLayout
deriving via (GenericBuildable TickState) instance Buildable TickState
deriving anyclass instance IsoValue TickState
instance HasAnnotation TickState where
  annOptions = segCfmmAnnOptions


customGeneric "PositionIndex" ligoLayout
deriving via (GenericBuildable PositionIndex) instance Buildable PositionIndex
deriving anyclass instance IsoValue PositionIndex
instance HasAnnotation PositionIndex where
  annOptions = segCfmmAnnOptions

customGeneric "PositionState" ligoLayout
deriving via (GenericBuildable PositionState) instance Buildable PositionState
deriving anyclass instance IsoValue PositionState
instance HasAnnotation PositionState where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesValue" ligoLayout
deriving anyclass instance IsoValue CumulativesValue
instance HasAnnotation CumulativesValue where
  annOptions = segCfmmAnnOptions

customGeneric "TickCumulative" ligoLayout
deriving via (GenericBuildable TickCumulative) instance Buildable TickCumulative
deriving anyclass instance IsoValue TickCumulative
instance HasAnnotation TickCumulative where
  annOptions = segCfmmAnnOptions

customGeneric "LpsCumulative" ligoLayout
deriving via (GenericBuildable LpsCumulative) instance Buildable LpsCumulative
deriving anyclass instance IsoValue LpsCumulative
instance HasAnnotation LpsCumulative where
  annOptions = segCfmmAnnOptions

customGeneric "TimedCumulatives" ligoLayout
deriving via (GenericBuildable TimedCumulatives) instance Buildable TimedCumulatives
deriving anyclass instance IsoValue TimedCumulatives
instance HasAnnotation TimedCumulatives where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesBuffer" ligoLayout
deriving via (GenericBuildable CumulativesBuffer) instance Buildable CumulativesBuffer
deriving anyclass instance IsoValue CumulativesBuffer
instance HasAnnotation CumulativesBuffer where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "CumulativesBuffer" ligoLayout
deriving via (GenericBuildable CumulativesBufferRPC) instance Buildable CumulativesBufferRPC

customGeneric "Constants" ligoLayout
deriving via (GenericBuildable Constants) instance Buildable Constants
deriving anyclass instance IsoValue Constants
instance HasAnnotation Constants where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "Constants" ligoLayout
deriving via (GenericBuildable ConstantsRPC) instance Buildable ConstantsRPC

customGeneric "Storage" ligoLayout
deriving via (GenericBuildable Storage) instance Buildable Storage
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "Storage" ligoLayout
deriving via (GenericBuildable StorageRPC) instance Buildable StorageRPC
