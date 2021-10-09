-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- NOTE: This file should not be modified directly.
-- Use @stack scripts/generate_error_code.hs@ instead.

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SegCFMM.Errors where

import Universum

----------------------------------------------------------------------------
-- Invalid Input Error Codes
----------------------------------------------------------------------------

-- | Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick.
invalidWitnessErr :: Natural
invalidWitnessErr = 100

-- | The action would apply too big of a change to the price, which is not allowed. We assume that the amount of X or Y tokens in the contract should not change by more than 30% at once (in some circumstances, a larger change may be allowed).
tooBigPriceChangeErr :: Natural
tooBigPriceChangeErr = 101

-- | The action would put the price out of bounds. Used tick indices should remain within `[-1048575; 1048575]` range, and, respectively, amount of one token type in the pair should not exceed `exp(0.0001)^1048575 ≈ 3.46 * 10^45` times the amount in the other token.
priceOutOfBoundsErr :: Natural
priceOutOfBoundsErr = 102

-- | Swap has expired: now > deadline.
pastDeadlineErr :: Natural
pastDeadlineErr = 103

-- | Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`.
smallerThanMinAssetErr :: Natural
smallerThanMinAssetErr = 104

-- | User provided tick is not initialized.
tickNotExistErr :: Natural
tickNotExistErr = 105

-- | The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`.
highTokensErr :: Natural
highTokensErr = 106

-- | Some of the timestamps passed to the `observe` entrypoint are too far back in the past.
invalidTimestampErr :: Natural
invalidTimestampErr = 107

-- | The X prime contract address provided is not a segmented-cfmm contract.
invalidXPrimeContractErr :: Natural
invalidXPrimeContractErr = 108

-- | Some of the timestamps passed to the `observe` entrypoint are too far back in the past.
observeOutdatedTimestampErr :: Natural
observeOutdatedTimestampErr = 109

-- | Some of the timestamps passed to the `observe` entrypoint are yet in the future.
observeFutureTimestampErr :: Natural
observeFutureTimestampErr = 110

-- | When setting a new position, `upper_tick_index` must be strictly greater than `lower_tick_index`.
tickOrderErr :: Natural
tickOrderErr = 111


----------------------------------------------------------------------------
-- Contract Configuration Error Codes
----------------------------------------------------------------------------

-- | The `x_token_address` or `y_token_address` has no transfer entrypoint.
assetTransferInvalidEntrypointsErr :: Natural
assetTransferInvalidEntrypointsErr = 200

-- | The `x_token_address` or `y_token_address` has no `update_operator` entrypoint.
assetUpdateOperatorInvalidEntrypointsErr :: Natural
assetUpdateOperatorInvalidEntrypointsErr = 201

-- | The `x_token_address` or `y_token_address` has no `approve` entrypoint.
assetApproveInvalidEntrypointsErr :: Natural
assetApproveInvalidEntrypointsErr = 202


----------------------------------------------------------------------------
-- Internal Error Codes
----------------------------------------------------------------------------

-- | Generic impossible error.
internalImpossibleErr :: Natural
internalImpossibleErr = 300

-- | Tick is not initialized.
internalTickNotExistErr :: Natural
internalTickNotExistErr = 301

-- | Time now is smaller than epoch time.
internalEpochBiggerThanNowErr :: Natural
internalEpochBiggerThanNowErr = 302

-- | The `fee_bps` is initialized to be higher than 10000 (100%).
internalFeeMoreThan100PercentErr :: Natural
internalFeeMoreThan100PercentErr = 303

-- | Unexpected price direction movement after sqrt_price_move_x.
internalBadSqrtPriceMoveXDirection :: Natural
internalBadSqrtPriceMoveXDirection = 304

-- | Unexpected price direction movement after sqrt_price_move_y.
internalBadSqrtPriceMoveYDirection :: Natural
internalBadSqrtPriceMoveYDirection = 305

-- | Flip for `fee_growth_outside` failed. (This is an invariant of the contract).
internalFlipFeeGrowthOutsideErr :: Natural
internalFlipFeeGrowthOutsideErr = 306

-- | Thrown when `(p.dx - dx_consumed)` or `(p.dy - dy_consumed)` is not nat.
internal307 :: Natural
internal307 = 307

-- | Liquidity went below zero.
internalLiquidityBelowZeroErr :: Natural
internalLiquidityBelowZeroErr = 308

-- | Thrown when `(p.dx - r.dx)` is not nat.
internal309 :: Natural
internal309 = 309

-- | Thrown when `s.cur_tick_index.i >= upper_tick_index.i` and `(s.fee_growth.x - upper_tick.fee_growth_outside.x)` (or `y`) is not nat.
internal311 :: Natural
internal311 = 311

-- | Thrown when `s.cur_tick_index.i < lower_tick_index.i` and `(s.fee_growth.x - lower_tick.fee_growth_outside.x)` (or `y`) is not nat.
internal312 :: Natural
internal312 = 312

-- | Number of positions underflow.
internalPositionUnderflowErr :: Natural
internalPositionUnderflowErr = 313

-- | Thrown when `(s.fee_growth.x - f_a.x - f_b.x)` is not nat.
internal314 :: Natural
internal314 = 314

-- | Thrown when `(s.fee_growth.y - f_a.y - f_b.y)` is not nat.
internal315 :: Natural
internal315 = 315

-- | Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat.
internal316 :: Natural
internal316 = 316

-- | Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat.
internal317 :: Natural
internal317 = 317

-- | Thrown when `s.cur_tick_index.i < p.lower_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract).
internalSqrtPriceGrowErr1 :: Natural
internalSqrtPriceGrowErr1 = 318

-- | Thrown when `p.lower_tick_index.i <= s.cur_tick_index.i && s.cur_tick_index.i < p.upper_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract).
internalSqrtPriceGrowErr2 :: Natural
internalSqrtPriceGrowErr2 = 319

-- | Thrown when `seconds_outside` is negative.
internalNegativeSecondsOutsideErr :: Natural
internalNegativeSecondsOutsideErr = 320

-- | Failed to access a value in time-weighted i_c cumulative sums buffer.
internalBadAccessToObservationBuffer :: Natural
internalBadAccessToObservationBuffer = 321

-- | Some issue with binary search in `observe` entrypoint.
internalObserveBinSearchFailed :: Natural
internalObserveBinSearchFailed = 322

-- | Attempt to garbade collect a tick with non-zero liquidity net.
internalNonEmptyPositionGcErr :: Natural
internalNonEmptyPositionGcErr = 323

-- | Flip of `seconds_per_liquidity_outside` failed. (This is an invariant of the contract).
internalFlipSecondsPerLiquidityOutsideErr :: Natural
internalFlipSecondsPerLiquidityOutsideErr = 324

-- | Position creation/change unexpectedly transferred tokens to someone
internalUnexpectedIncomeErr :: Natural
internalUnexpectedIncomeErr = 325
