// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

// NOTE: This file should not be modified directly.
// Use `stack scripts/generate_error_code.hs` instead.

#if ERRORS_MLIGO
#else
#define ERRORS_MLIGO

#include "types.mligo"

// ---------------------------------------------------------------------------
// -- Invalid input error codes
// ---------------------------------------------------------------------------

(* Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick. *)
[@inline] let invalid_witness_err = 100n

(* The action would apply too big of a change to the price, which is not allowed. We assume that the amount of X or Y tokens in the contract should not change by more than 30% at once (in some circumstances, a larger change may be allowed). *)
[@inline] let too_big_price_change_err = 101n

(* The action would put the price out of bounds. Used tick indices should remain within `[-1048575; 1048575]` range, and, respectively, amount of one token type in the pair should not exceed `exp(0.0001)^1048575 ≈ 3.46 * 10^45` times the amount in the other token. *)
[@inline] let price_out_of_bounds_err = 102n

(* Swap has expired: now > deadline. *)
[@inline] let past_deadline_err = 103n

(* Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`. *)
[@inline] let smaller_than_min_asset_err = 104n

(* User provided tick is not initialized. *)
[@inline] let tick_not_exist_err = 105n

(* The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`. *)
[@inline] let high_tokens_err = 106n

(* The X prime contract address provided is not a segmented-cfmm contract. *)
[@inline] let invalid_x_prime_contract_err = 107n

(* Some of the timestamps passed to the `observe` entrypoint are too far back in the past. *)
[@inline] let observe_outdated_timestamp_err = 108n

(* Some of the timestamps passed to the `observe` entrypoint are yet in the future. *)
[@inline] let observe_future_timestamp_err = 109n

(* When setting a new position, `upper_tick_index` must be strictly greater than `lower_tick_index`. When observing cumulative values at range, `upper_tick_index` must be greater or equal than `lower_tick_index`. *)
[@inline] let tick_order_err = 110n

(* Liquidity of a position went below zero. *)
[@inline] let position_liquidity_below_zero_err = 111n

(* Tick indexes must be a multiple of the tick spacing. *)
[@inline] let incorrect_tick_spacing_err = 112n

(* Contract call also transfers some XTZ; this is not allowed, it would be stuck. *)
[@inline] let non_zero_transfer_err = 113n



// ---------------------------------------------------------------------------
// -- Contract configuration error codes
// ---------------------------------------------------------------------------

(* The `x_token_address` or `y_token_address` has no transfer entrypoint. *)
[@inline] let asset_transfer_invalid_entrypoints_err = 200n

(* The `x_token_address` or `y_token_address` has no `update_operator` entrypoint. *)
[@inline] let asset_update_operator_invalid_entrypoints_err = 201n

(* The `x_token_address` or `y_token_address` has no `approve` entrypoint. *)
[@inline] let asset_approve_invalid_entrypoints_err = 202n



// ---------------------------------------------------------------------------
// -- Internal error codes
// ---------------------------------------------------------------------------

(* Generic impossible error. *)
[@inline] let internal_impossible_err = 300n

(* Tick is not initialized. *)
[@inline] let internal_tick_not_exist_err = 301n

(* Time now is smaller than epoch time. *)
[@inline] let internal_epoch_bigger_than_now_err = 302n

(* The `fee_bps` is initialized to be higher than 10000 (100%). *)
[@inline] let internal_fee_more_than_100_percent_err = 303n

(* Unexpected price direction movement after sqrt_price_move_x. *)
[@inline] let internal_bad_sqrt_price_move_x_direction = 304n

(* Unexpected price direction movement after sqrt_price_move_y. *)
[@inline] let internal_bad_sqrt_price_move_y_direction = 305n

(* Flip for `fee_growth_outside` failed. (This is an invariant of the contract). *)
[@inline] let internal_flip_fee_growth_outside_err = 306n

(* Thrown when `(p.dx - dx_consumed)` or `(p.dy - dy_consumed)` is not nat. *)
[@inline] let internal_307 = 307n

(* Liquidity of a tick went below zero. *)
[@inline] let internal_liquidity_below_zero_err = 308n

(* Thrown when `(p.dx - r.dx)` is not nat. *)
[@inline] let internal_309 = 309n

(* Thrown when `s.cur_tick_index.i >= upper_tick_index.i` and `(s.fee_growth.x - upper_tick.fee_growth_outside.x)` (or `y`) is not nat. *)
[@inline] let internal_311 = 311n

(* Thrown when `s.cur_tick_index.i < lower_tick_index.i` and `(s.fee_growth.x - lower_tick.fee_growth_outside.x)` (or `y`) is not nat. *)
[@inline] let internal_312 = 312n

(* Number of positions underflow. *)
[@inline] let internal_position_underflow_err = 313n

(* Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat. *)
[@inline] let internal_316 = 316n

(* Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat. *)
[@inline] let internal_317 = 317n

(* Thrown when `s.cur_tick_index.i < p.lower_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). *)
[@inline] let internal_sqrt_price_grow_err_1 = 318n

(* Thrown when `p.lower_tick_index.i <= s.cur_tick_index.i && s.cur_tick_index.i < p.upper_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). *)
[@inline] let internal_sqrt_price_grow_err_2 = 319n

(* Thrown when `seconds_outside` is negative. *)
[@inline] let internal_negative_seconds_outside_err = 320n

(* Failed to access a value in time-weighted i_c cumulative sums buffer. *)
[@inline] let internal_bad_access_to_observation_buffer = 321n

(* Some issue with binary search in `observe` entrypoint. *)
[@inline] let internal_observe_bin_search_failed = 322n

(* Attempt to garbade collect a tick with non-zero liquidity net. *)
[@inline] let internal_non_empty_position_gc_err = 323n

(* Flip of `seconds_per_liquidity_outside` failed. (This is an invariant of the contract). *)
[@inline] let internal_flip_seconds_per_liquidity_outside_err = 324n

(* Position creation/change unexpectedly transferred tokens to someone *)
[@inline] let internal_unexpected_income_err = 325n

(* Price became negative when crossing a tick *)
[@inline] let internal_negative_price = 326n



#endif
