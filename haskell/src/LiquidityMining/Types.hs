-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module LiquidityMining.Types
  ( module LiquidityMining.Types
  ) where

import Fmt (Buildable, GenericBuildable(..))
import Universum

import Lorentz.Annotation
import Lorentz.Entrypoints
import Lorentz.Value

import FA2.Types
import SegCFMM.Types (PositionId, TickIndex, X, segCfmmAnnOptions)
import qualified SegCFMM.Types as CFMM

data IncentiveParams = IncentiveParams
  { ipRewardToken :: FA2Token
  , ipTotalReward :: Natural
  , ipStartTime :: Timestamp
  , ipEndTime :: Timestamp
  , ipRefundee :: Address
  }

data Incentive = Incentive
  { iParams :: IncentiveParams
  , iTotalRewardUnclaimed :: Natural
  , iTotalSecondsClaimed :: X 128 Natural
  , iNumberOfStakes :: Natural
  }

newtype IncentiveId = IncentiveId Natural
  deriving stock (Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation, Buildable)

data Deposit = Deposit
  { dOwner :: Address
  , dNumberOfStakes :: Natural
  , dTickIndexRange :: Maybe (TickIndex, TickIndex)
  }

data Stake = Stake
  { dSecondsPerLiquidityInsideInitial :: X 128 Integer
  , dLiquidity :: Natural
  }

data StakeTokenPhase
  = StNotStarted
  | StAskedPositionInfo (IncentiveId, PositionId)
  | StAskedCumulativesInfo ((IncentiveId, PositionId), CFMM.PositionInfo)

data UnstakeTokenPhase
  = UstNotStarted
  | UstAskedCumulativesInfo ((IncentiveId, PositionId), Address)

data Config = Config
  { cMaxIncentiveStartLeadTime :: Natural
  , cMaxIncentiveDuration :: Natural
  }

data Storage = Storage
  { sUniswap :: TAddress CFMM.Parameter
  , sConfig :: Config
  , sIncentives :: BigMap IncentiveId Incentive
  , sNextIncentiveId :: Natural
  , sDeposits :: BigMap PositionId Deposit
  , sStakes :: BigMap (IncentiveId, PositionId) Stake
  , sRewards :: BigMap (FA2Token, Address) Natural
  , sStakeTokenPhase :: StakeTokenPhase
  , sUnstakeTokenPhase :: UnstakeTokenPhase
  }

initStorage :: Config -> TAddress CFMM.Parameter -> Storage
initStorage config uniswap = Storage
  { sUniswap = uniswap
  , sConfig = config
  , sIncentives = mkBigMap mempty
  , sNextIncentiveId = 0
  , sDeposits = mkBigMap mempty
  , sStakes = mkBigMap mempty
  , sRewards = mkBigMap mempty
  , sStakeTokenPhase = StNotStarted
  , sUnstakeTokenPhase = UstNotStarted
  }

type DestinationAddress = Address

data Parameter
  = Create_incentive IncentiveParams
  | End_incentive IncentiveId
  | Register_deposit PositionId
  | Transfer_deposit (PositionId, DestinationAddress)
  | Withdraw_deposit (PositionId, DestinationAddress)
  | Stake_token (IncentiveId, PositionId)
  | Stake_token_on_position_info CFMM.PositionInfo
  | Stake_token_on_cum_info CFMM.CumulativesInsideSnapshot
  | Unstake_token (IncentiveId, PositionId)
  | Unstake_token_on_cum_info CFMM.CumulativesInsideSnapshot
  | Claim_reward (FA2Token, DestinationAddress, Maybe Natural)

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "IncentiveParams" ligoLayout
deriving via (GenericBuildable IncentiveParams) instance Buildable IncentiveParams
deriving anyclass instance IsoValue IncentiveParams
instance HasAnnotation IncentiveParams where
  annOptions = segCfmmAnnOptions

customGeneric "Incentive" ligoLayout
deriving via (GenericBuildable Incentive) instance Buildable Incentive
deriving anyclass instance IsoValue Incentive
instance HasAnnotation Incentive where
  annOptions = segCfmmAnnOptions

customGeneric "Deposit" ligoLayout
deriving via (GenericBuildable Deposit) instance Buildable Deposit
deriving anyclass instance IsoValue Deposit
instance HasAnnotation Deposit where
  annOptions = segCfmmAnnOptions

customGeneric "Stake" ligoLayout
deriving via (GenericBuildable Stake) instance Buildable Stake
deriving anyclass instance IsoValue Stake
instance HasAnnotation Stake where
  annOptions = segCfmmAnnOptions

customGeneric "StakeTokenPhase" ligoLayout
deriving via (GenericBuildable StakeTokenPhase) instance Buildable StakeTokenPhase
deriving anyclass instance IsoValue StakeTokenPhase
instance HasAnnotation StakeTokenPhase where
  annOptions = segCfmmAnnOptions

customGeneric "UnstakeTokenPhase" ligoLayout
deriving via (GenericBuildable UnstakeTokenPhase) instance Buildable UnstakeTokenPhase
deriving anyclass instance IsoValue UnstakeTokenPhase
instance HasAnnotation UnstakeTokenPhase where
  annOptions = segCfmmAnnOptions

customGeneric "Config" ligoLayout
deriving via (GenericBuildable Config) instance Buildable Config
deriving anyclass instance IsoValue Config
instance HasAnnotation Config where
  annOptions = segCfmmAnnOptions

customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = segCfmmAnnOptions

customGeneric "Parameter" ligoLayout
deriving via (GenericBuildable Parameter) instance Buildable Parameter
deriving anyclass instance IsoValue Parameter
instance HasAnnotation Parameter where
  annOptions = segCfmmAnnOptions

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain
