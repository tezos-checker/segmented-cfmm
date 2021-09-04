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
  ) where

import Universum

import Fmt (Buildable, build, genericF)

import Lorentz hiding (now)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16

-- | A value with @2^-n@ precision.
newtype X (n :: Nat) a = X
  { pickX :: a
    -- ^ Get the value multiplied by @2^n@.
  } deriving stock (Show, Eq, Generic)
    deriving newtype (IsoValue, HasAnnotation)

instance (Buildable a, KnownNat n) => Buildable (X n a) where
  build x = build (pickX x) <> " X 2^" <> build (powerOfX x)

powerOfX :: KnownNat n => X n a -> Natural
powerOfX (X{} :: X n a) = natVal (Proxy @n)

-- | Convert a fraction to 'X'.
mkX :: forall n a. (KnownNat n, RealFrac a, Integral a) => a -> X n a
mkX = X . round . (* 2 ^ natVal (Proxy @n))

data Parameter
  = X_to_Y XToYParam
    -- ^ Trade up to a quantity dx of asset x, receives dy
  | Y_to_X YToXParam
    -- ^ Trade up to a quantity dy of asset y, receives dx
  | Set_position SetPositionParam
    -- ^ TODO: Add deadline, maximum tokens contributed, and maximum liquidity present
  | X_to_X_prime Address
    -- ^ Equivalent to token_to_token
  | Get_time_weighted_sum (ContractRef Views)
  | Call_FA2 FA2.Parameter

instance Buildable Parameter where
  build = genericF


data Views =
  IC_sum Integer

instance Buildable Views where
  build = genericF

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

instance Buildable XToYParam where
  build = genericF

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

instance Buildable YToXParam where
  build = genericF


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
  }

instance Buildable SetPositionParam where
  build = genericF



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
  , sBalance :: PerToken Natural
  , sTicks :: TickMap
    -- ^ Ticks' states.
  , sPositions :: PositionMap
    -- ^ Positions' states.
  , sPositionIndexes :: PositionIndexMap
    -- ^ One-to-one relation from `postion_id` to `position_index`.
  , sTimeWeightIcSum :: Integer
    -- ^ Cumulative time-weighted sum of the 'sIC'.
  , sLastIcSumUpdate :: Timestamp
    -- ^ Last time 'sLastIcSumUpdate' was updated.
  , sSecondsPerLiquidityCumulative :: Natural

  , sMetadata :: TZIP16.MetadataMap BigMap
    -- ^ TZIP-16 metadata.
  , sNewPositionId :: PositionId
    -- ^ Incremental position id to be assigned to new position.
  , sOperators :: Operators
    -- ^ FA2 operators
  }

instance Buildable Storage where
  build = genericF

-- Needed by `sMetadata`
instance Buildable (ByteString) where
  build = build . show @Text

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT


data PerToken a = PerToken
  { ptX :: a
  , ptY :: a
  }

instance Buildable a => Buildable (PerToken a) where
  build = genericF

-- | Tick types, representing pieces of the curve offered between different tick segments.
newtype TickIndex = TickIndex Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

instance Buildable TickIndex where
  build = genericF

instance HasAnnotation TickIndex where
  annOptions = segCfmmAnnOptions

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
  , tsFeeGrowthOutside :: PerToken (X 128 Natural)
    -- ^ Track fees accumulated below or above this tick.
  , tsSecondsPerLiquidityOutside :: X 128 Natural
    -- ^ Track seconds-weighted 1/L value below or above this tick.
  , tsSqrtPrice :: X 80 Natural
    -- ^ Square root of the price associated with this tick.
  }

instance Buildable TickState where
  build = genericF


type TickMap = BigMap TickIndex TickState

-- | Position types, representing LP positions.
data PositionIndex = PositionIndex
  { piOwner :: Address
  , piLowerTickIndex :: TickIndex
    -- ^ Lower bound.
  , piUpperTickIndex :: TickIndex
    -- ^ Upper bound.
  } deriving stock (Ord, Eq)

instance Buildable PositionIndex where
  build = genericF


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

instance Buildable PositionState where
  build = genericF


newtype PositionId = PositionId Natural
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

instance Buildable PositionId where
  build = genericF

instance HasAnnotation PositionId where
  annOptions = segCfmmAnnOptions


-- | Map containing Liquidity providers.
type PositionMap = BigMap PositionIndex PositionState

-- | One-to-one relation from `postion_id` to `position_index`.
-- Used for querying `position_state` with just a `position_id`.
type PositionIndexMap = BigMap PositionId PositionIndex


------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------

data Operator = Operator
  { oOwner :: Address
  , oOperator :: Address
  } deriving stock (Eq, Ord)

instance Buildable Operator where
  build = genericF

instance (Buildable a, Buildable b) => Buildable (a, b) where
  build = genericF

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

customGeneric "Views" ligoLayout
deriving anyclass instance IsoValue Views
instance HasAnnotation Views where
  annOptions = segCfmmAnnOptions

customGeneric "XToYParam" ligoLayout
deriving anyclass instance IsoValue XToYParam
instance HasAnnotation XToYParam where
  annOptions = segCfmmAnnOptions

customGeneric "YToXParam" ligoLayout
deriving anyclass instance IsoValue YToXParam
instance HasAnnotation YToXParam where
  annOptions = segCfmmAnnOptions

customGeneric "SetPositionParam" ligoLayout
deriving anyclass instance IsoValue SetPositionParam
instance HasAnnotation SetPositionParam where
  annOptions = segCfmmAnnOptions


customGeneric "Operator" ligoLayout
deriving anyclass instance IsoValue Operator
instance HasAnnotation Operator where
  annOptions = segCfmmAnnOptions

customGeneric "FA2.Parameter" ligoLayout
deriving anyclass instance IsoValue FA2.Parameter
instance HasAnnotation FA2.Parameter where
  annOptions = segCfmmAnnOptions
instance ParameterHasEntrypoints FA2.Parameter where
  type ParameterEntrypointsDerivation FA2.Parameter = EpdPlain


customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate


customGeneric "PerToken" ligoLayout
deriving anyclass instance IsoValue a => IsoValue (PerToken a)
instance HasAnnotation a => HasAnnotation (PerToken a) where
  annOptions = segCfmmAnnOptions

customGeneric "TickState" ligoLayout
deriving anyclass instance IsoValue TickState
instance HasAnnotation TickState where
  annOptions = segCfmmAnnOptions


customGeneric "PositionIndex" ligoLayout
deriving anyclass instance IsoValue PositionIndex
instance HasAnnotation PositionIndex where
  annOptions = segCfmmAnnOptions

customGeneric "PositionState" ligoLayout
deriving anyclass instance IsoValue PositionState
instance HasAnnotation PositionState where
  annOptions = segCfmmAnnOptions


customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = segCfmmAnnOptions
