-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | This module implements some of the equations described in the Uniswap v3 whitepaper.
module Test.Math
  ( sqrtPriceFor
  , feeGrowthInside
  , liquidityDeltaToTokensDelta
  , initTickAccumulators
  ) where

import Prelude

import qualified Data.Map as Map
import Lorentz hiding (assert, not, now, (>>))
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Tezos.Core (timestampToSeconds)

import SegCFMM.Types
import Test.Util

_280 :: Integral a => a
_280 = 2^(80 :: Integer)

-- | Calculate the expected @sqrt_price@ for a given tick index.
sqrtPriceFor :: TickIndex -> X 30 Natural
sqrtPriceFor (TickIndex i) =
  -- We're doing floating point math in Haskell, so we lose a lot of precision.
  -- To be able to compare a value calculated in Haskell to one calculated in Michelson,
  -- we need to account for that loss of precision, so we reduce the scale
  -- of each number from 2^80 to 2^30.
  adjustScale @30 $
    mkX @Double @80 (sqrt (exp 0.0001) ^^ i)

-- | Calculate the fee growth within a given range since the contract was originated.
feeGrowthInside :: (MonadEmulated caps base m, HasCallStack) => Storage -> TickIndex -> TickIndex -> m (PerToken (X 128 Natural))
feeGrowthInside st lowerTickIndex upperTickIndex = do
  -- Equation 6.17
  let feeGrowthAbove ti ts = if sCurTickIndex st >= ti then sFeeGrowth st - tsFeeGrowthOutside ts else tsFeeGrowthOutside ts
  -- Equation 6.18
  let feeGrowthBelow ti ts = if sCurTickIndex st >= ti then tsFeeGrowthOutside ts else sFeeGrowth st - tsFeeGrowthOutside ts
  -- Equation 6.18
  let feeGrowthInside' lowerTi lowerTs upperTi upperTs =
        sFeeGrowth st - feeGrowthBelow lowerTi lowerTs - feeGrowthAbove upperTi upperTs

  lowerTick <- st & sTicks & bmMap & Map.lookup lowerTickIndex & evalJust
  upperTick <- st & sTicks & bmMap & Map.lookup upperTickIndex & evalJust
  pure $ feeGrowthInside' lowerTickIndex lowerTick upperTickIndex upperTick

-- | When adding @liquidity_delta@ to a position, calculate how many tokens will need to be deposited/withdrawn.
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
            liquidityDelta * (sqrtPrice - sqrtPriceLower) `div` _280
        | otherwise =
            liquidityDelta * (sqrtPriceUpper - sqrtPriceLower) `div` _280

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
  :: MonadNettest caps base m
  => ContractHandler Parameter st -> Storage -> TickIndex
  -> m (Natural, Integer, PerToken (X 128 Natural), X 128 Natural)
initTickAccumulators cfmm st tickIndex =
  if sCurTickIndex st >= tickIndex
    then do
      currentTime <- getNow
      let secondsOutside = timestampToSeconds currentTime
      let feeGrowthOutside = sFeeGrowth st

      consumer <- originateSimple @[CumulativesValue] "consumer" [] contractConsumer
      call cfmm (Call @"Observe") $ mkView [currentTime] consumer
      (tickCumulativeOutside, secondsPerLiquidityOutside) <-
        getStorage consumer >>= \case
          [[CumulativesValueRPC {cvTickCumulativeRPC, cvSecondsPerLiquidityCumulativeRPC}]] ->
            pure (cvTickCumulativeRPC, cvSecondsPerLiquidityCumulativeRPC)
          _ -> failure "Expected to get exactly 1 CumulativeValue"

      pure (secondsOutside, tickCumulativeOutside, feeGrowthOutside, secondsPerLiquidityOutside)
    else do
      pure (0, 0, PerToken 0 0, 0)
