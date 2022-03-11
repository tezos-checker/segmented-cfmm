-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | This module implements some of the equations described in the Uniswap v3 whitepaper.
module Test.Math
  ( sqrtPriceFor
  , Accumulators(..)
  , tickAccumulatorsInside
  , liquidityDeltaToTokensDelta
  , initTickAccumulators
  , calcSwapFee
  , calcNewPriceX
  , calcNewPriceY
  , receivedX
  , receivedY
  ) where

import Prelude

import qualified Data.Map as Map
import Fmt (Buildable, GenericBuildable(..))
import Lorentz hiding (assert, not, now, (>>))
import Morley.Nettest
import Tezos.Core (timestampToSeconds)

import SegCFMM.Types
import Test.Util

_280 :: Num a => a
_280 = 2^(80 :: Integer)

data Accumulators = Accumulators
  { aSeconds :: Integer
  , aTickCumulative :: Integer
  , aFeeGrowth :: PerToken (X 128 Integer)
  , aSecondsPerLiquidity :: X 128 Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving Buildable via (GenericBuildable Accumulators)

-- | Calculate the expected @sqrt_price@ for a given tick index.
sqrtPriceFor :: TickIndex -> X 30 Natural
sqrtPriceFor (TickIndex i) =
  -- We're doing floating point math in Haskell, so we lose a lot of precision.
  -- To be able to compare a value calculated in Haskell to one calculated in Michelson,
  -- we need to account for that loss of precision, so we reduce the scale
  -- of each number from 2^80 to 2^30.
  adjustScale @30 $
    mkX' @Double @80 (sqrt (exp 0.0001) ^^ i)

-- | Calculate the fee growth within a given range since the contract was originated.
tickAccumulatorsInside
  :: forall caps base m
   . (MonadEmulated caps base m, HasCallStack)
  => ContractHandler Parameter Storage
  -> Storage
  -> TickIndex
  -> TickIndex
  -> m Accumulators
tickAccumulatorsInside cfmm st lowerTi upperTi = do
  lowerTs <- st & sTicks & bmMap & Map.lookup lowerTi & evalJust
  upperTs <- st & sTicks & bmMap & Map.lookup upperTi & evalJust

  currentTime <- getNow
  CumulativesValue {cvTickCumulative, cvSecondsPerLiquidityCumulative} <- observe cfmm
  pure Accumulators
    { aSeconds
        = tickAccumulatorInside lowerTs upperTs
            (fromIntegral @Natural @Integer $ timestampToSeconds currentTime)
            (fromIntegral @Natural @Integer . tsSecondsOutside)
    , aTickCumulative
        = tickAccumulatorInside lowerTs upperTs cvTickCumulative tsTickCumulativeOutside
    , aFeeGrowth
        = tickAccumulatorInside lowerTs upperTs
            (fmap (fromIntegral @Natural @Integer) <$> sFeeGrowth st)
            (\ts -> fmap (fromIntegral @Natural @Integer) <$> tsFeeGrowthOutside ts)
    , aSecondsPerLiquidity
        = tickAccumulatorInside lowerTs upperTs
            (fromIntegral @Natural @Integer <$> cvSecondsPerLiquidityCumulative)
            (\ts -> fromIntegral @Natural @Integer <$> tsSecondsPerLiquidityOutside ts)
    }
  where
    -- Equation 6.17
    tickAccumulatorAbove :: Num a => (TickIndex, TickState) -> a -> (TickState -> a) -> a
    tickAccumulatorAbove (idx, ts) globalAcc tickAccOutside =
      if sCurTickIndex st >= idx then globalAcc - tickAccOutside ts else tickAccOutside ts

    -- Equation 6.18
    tickAccumulatorBelow :: Num a => (TickIndex, TickState) -> a -> (TickState -> a) -> a
    tickAccumulatorBelow (idx, ts) globalAcc tickAccOutside =
        if sCurTickIndex st >= idx then tickAccOutside ts else globalAcc - tickAccOutside ts

    -- Equation 6.19
    tickAccumulatorInside :: Num a => TickState -> TickState -> a -> (TickState -> a) -> a
    tickAccumulatorInside lowerTs upperTs globalAcc tickAccOutside =
      globalAcc
      - tickAccumulatorBelow (lowerTi, lowerTs) globalAcc tickAccOutside
      - tickAccumulatorAbove (upperTi, upperTs) globalAcc tickAccOutside

-- | When adding @liquidity_delta@ to a position, calculate how many tokens will need to be deposited/withdrawn.
-- Due to the floating-point math used in `sqrtPriceFor`, this function has a certain margin of error.
liquidityDeltaToTokensDelta :: Integer -> TickIndex -> TickIndex -> TickIndex -> X 80 Natural -> PerToken Integer
liquidityDeltaToTokensDelta liquidityDelta lowerTickIndex upperTickIndex currentTickIndex sqrtPrice' =
  let
      sqrtPrice     = fromIntegral @Natural @Integer $ pickX sqrtPrice'
      sqrtPriceLower = fromIntegral @Natural @Integer $ pickX $ adjustScale @80 $ sqrtPriceFor lowerTickIndex
      sqrtPriceUpper = fromIntegral @Natural @Integer $ pickX $ adjustScale @80 $ sqrtPriceFor upperTickIndex

      -- Equation 6.29
      deltaY
        | currentTickIndex < lowerTickIndex = 0
        | lowerTickIndex <= currentTickIndex && currentTickIndex < upperTickIndex =
            {-
              ΔL * (√P - √pil)

              Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
                liquidityDelta * (sqrtPrice / 2^80 - sqrtPriceLower / 2^80)
              Using the distributive property of division:
                liquidityDelta * (sqrtPrice - sqrtPriceLower) / 2^80
            -}
            liquidityDelta * (sqrtPrice - sqrtPriceLower) `divUp` _280
        | otherwise =
            liquidityDelta * (sqrtPriceUpper - sqrtPriceLower) `divUp` _280

      -- Equation 6.30
      deltaX
        | currentTickIndex < lowerTickIndex =
            (liquidityDelta * _280 * (-sqrtPriceLower + sqrtPriceUpper)) `divUp` (sqrtPriceLower * sqrtPriceUpper)
        | lowerTickIndex <= currentTickIndex && currentTickIndex < upperTickIndex =
            {-
              ΔL * (1/√P - 1/√piu)

              Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
                liquidityDelta * (1 / (sqrtPrice / 2^80) - 1 / (sqrtPriceUpper / 2^80))
              Simplifying the fractions:
                liquidityDelta * (2^80 / sqrtPrice) - (2^80 / sqrtPriceUpper)
              The least common denominator is `sqrtPrice * sqrtPriceUpper)`,
              so we multiply the first fraction by sqrtPriceUpper and the second by sqrtPrice:
                liquidityDelta * ((2^80 * sqrtPriceUpper) / (sqrtPrice * sqrtPriceUpper)) - ((2^80 * sqrtPrice) / (sqrtPriceUpper * sqrtPrice))
              Subtracting the two fractions:
                liquidityDelta * (2^80 * sqrtPriceUpper - 2^80 * sqrtPrice) / (sqrtPrice * sqrtPriceUpper)
              Using the distributive property of multiplication:
                liquidityDelta * 2^80 * (sqrtPriceUpper - sqrtPrice) / (sqrtPrice * sqrtPriceUpper)
            -}
            (liquidityDelta * _280 * (sqrtPriceUpper - sqrtPrice)) `divUp` (sqrtPrice * sqrtPriceUpper)
        | otherwise = 0
  in  PerToken deltaX deltaY

-- | Equation 6.21
--
-- Calculates the initial value of the accumulators tracked by a tick's state.
initTickAccumulators
  :: MonadEmulated caps base m
  => ContractHandler Parameter st -> Storage -> TickIndex
  -> m Accumulators
initTickAccumulators cfmm st tickIndex =
  if sCurTickIndex st >= tickIndex
    then do
      secondsOutside <- getNow <&> timestampToSeconds
      CumulativesValue tickCumulative secondsPerLiquidity <- observe cfmm
      pure Accumulators
        { aSeconds = secondsOutside
        , aTickCumulative = tickCumulative
        , aFeeGrowth = fmap (fromIntegral @Natural @Integer) <$> sFeeGrowth st
        , aSecondsPerLiquidity = fromIntegral @Natural @Integer <$> secondsPerLiquidity
        }
    else do
      -- pure (0, 0, PerToken 0 0, 0)
      pure Accumulators
        { aSeconds = 0
        , aTickCumulative = 0
        , aFeeGrowth = PerToken 0 0
        , aSecondsPerLiquidity = 0
        }

-- | Calculate the swap fee paid when depositing @tokensDelta@ tokens.
calcSwapFee :: Natural -> Natural -> Natural
calcSwapFee feeBps tokensDelta =
  ceiling $
    fromIntegral @Natural @Double (tokensDelta * feeBps)
    /
    10000

{- | Calculate the new price after depositing @dx@ tokens **while swapping within a single tick**.

Equation 6.15
  Δ(1 / √P) = Δx / L
  1 / √P_new - 1 / √P_old = Δx / L

  Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
    1 / (sqrt_price_new / 2^80) - 1 / (sqrt_price_old / 2^80) = dx / liquidity
  Simplifying the fractions:
    2^80 / sqrt_price_new - 2^80 / sqrt_price_old = dx / liquidity
  Adding `2^80 / sqrt_price_old` to both sides:
    2^80 / sqrt_price_new = dx / liquidity + 2^80 / sqrt_price_old
  Multiplying both sides by sqrt_price_new:
    2^80 = (dx / liquidity + 2^80 / sqrt_price_old) * sqrt_price_new
  Dividing both sides by (dx / liquidity + 2^80 / sqrt_price_old):
    2^80 / (dx / liquidity + 2^80 / sqrt_price_old) = sqrt_price_new
 -}
calcNewPriceX :: X 80 Natural -> Natural -> Natural -> X 30 Natural
calcNewPriceX (X sqrtPriceOld) liquidity dx =
  adjustScale @30 $
    X @80 $
      round @Double @Natural $
         _280 / (fromIntegral dx / fromIntegral liquidity + _280 / fromIntegral sqrtPriceOld)

{- | Calculate the new price after depositing @dy@ tokens **while swapping within a single tick**.

Equation 6.13
  Δ(√P) = Δy / L
  √P_new - √P_old = Δy / L

  Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
    sqrt_price_new / 2^80 - sqrt_price_old / 2^80 = dy / liquidity
  Adding sqrt_price_old / 2^80 to both sides:
    sqrt_price_new / 2^80 = dy / liquidity + sqrt_price_old / 2^80
  Multiplying both sides by 2^80:
    sqrt_price_new = 2^80 * dy / liquidity + sqrt_price_old

Keep in mind that the protocol fee is subtracted before the conversion, so
those tokens do not contribute to the price change.
-}
calcNewPriceY :: X 80 Natural -> Natural -> Natural -> Natural -> X 30 Natural
calcNewPriceY (X sqrtPriceOld) liquidity dy protoFeeBps =
  adjustScale @30 $
    X @80 $
      _280 * (removeProtocolFee dy protoFeeBps) `div` liquidity + sqrtPriceOld

{- | Calculate how many X tokens should be given to the user after depositing Y tokens.

Equation 6.16
  Δx = Δ(1/√P) * L
  Δx = (1/√P_new - 1/√P_old) * L
Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
  dx = L * ( 1                     - 1                     )
           ( ---------------------   --------------------- )
           ( sqrt_price_new / 2^80   sqrt_price_old / 2^80 )
Simplifying the fractions:
  dx = L * ( 2^80           - 2^80           )
           ( --------------   -------------- )
           ( sqrt_price_new   sqrt_price_old )
-}
receivedX :: X 80 Natural -> X 80 Natural -> Natural -> Integer
receivedX (X sqrtPriceOld) (X sqrtPriceNew) liquidity =
  let dx =
        fromIntegral @Natural @Double (liquidity * _280) / fromIntegral sqrtPriceNew
        -
        fromIntegral @Natural @Double (liquidity * _280) / fromIntegral sqrtPriceOld

  -- dx is the amount of tokens to add to the pool.
  -- To calculate how many tokens will be sent to the user, we flip the sign.
  in
    floor @Double @Integer (-dx)

{- | Calculate how many Y tokens should be given to the user after depositing X tokens.

Equation 6.14
  Δy = Δ√P * L
  Δy = (√P_new - √P_old) * L
Since sqrtPrice = √P * 2^80, we can subtitute √P with sqrtPrice / 2^80:
  dy = (sqrtPriceNew / 2^80 - sqrtPriceOld / 2^80) * L

Keep in mind that the protocol fee is subtracted after the conversion, so the
received @Y@s can be calculated from the same price difference.
-}
receivedY :: X 80 Natural -> X 80 Natural -> Natural -> Natural -> Integer
receivedY (X sqrtPriceOld) (X sqrtPriceNew) liquidity protoFeeBps = received
  where
    dy :: Double =
      (fromIntegral sqrtPriceNew / _280 - fromIntegral sqrtPriceOld / _280) * fromIntegral liquidity
    -- dy is the amount of tokens to add to the pool.
    -- To calculate how many tokens will be removed from the pool we need to
    -- flip the sign and round this value down.
    dyOut = floor @Double @Natural (-dy)
    -- To calculate how many tokens will be sent to the user, we also need to
    -- remove the protocol fee.
    -- Note: because the protocol fee is subtracted after the conversion this
    -- is rounded down once again.
    received = toInteger $ removeProtocolFee dyOut protoFeeBps

-- | Subtract the protocol fee from an amount of @Y@ tokens.
-- Note that this rounds down, as we always want to risk giving the user a bit
-- less rather than giving more than we have.
removeProtocolFee :: Natural -> Natural -> Natural
removeProtocolFee dy protoFeeBps = dy * (10_000 - protoFeeBps) `div` 10_000
