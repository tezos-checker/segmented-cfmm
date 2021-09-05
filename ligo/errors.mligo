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

(* Log out of bounds. *)
[@inline] let log_out_of_bounds_err = 101n

(* Should not reach end of ladder. *)
[@inline] let end_ladder_reached_err = 102n

(* Swap has expired: now > deadline. *)
[@inline] let past_deadline_err = 103n

(* Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`. *)
[@inline] let smaller_than_min_asset_err = 104n

(* User provided tick is not initialized. *)
[@inline] let tick_not_exist_err = 105n

(* The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`. *)
[@inline] let high_tokens_err = 106n

(* Some of the timestamps passed to the `observe` entrypoint are too far back in the past. *)
[@inline] let invalid_timestamp_err = 107n



// ---------------------------------------------------------------------------
// -- Contract configuration error codes
// ---------------------------------------------------------------------------

(* The `const_x_token_entrypoint` or `const_y_token_entrypoint` has no transfer entrypoint. *)
[@inline] let asset_transfer_invalid_entrypoints_err = 200n



// ---------------------------------------------------------------------------
// -- Internal error codes
// ---------------------------------------------------------------------------

(* Tick is not initialized. *)
[@inline] let internal_tick_not_exist_err = 300n

(* Time now is smaller than epoch time. *)
[@inline] let internal_epoch_bigger_than_now_err = 301n

(* The `const_fee_bps` is initialized to be higher than 10000 (100%). *)
[@inline] let internal_fee_more_than_100_percent_err = 302n

(* Thrown when `(p.s.sqrt_price - sqrt_price_new)` is not nat. *)
[@inline] let internal_303 = 303n

(* Thrown when `(sqrt_price_new - p.s.sqrt_price)` is not nat. *)
[@inline] let internal_304 = 304n

(* Flip for `fee_growth_outside` failed. (This is an invariant of the contract). *)
[@inline] let flip_fee_growth_outside_err = 305n

(* Thrown when `(p.dx - dx_consummed)` is not nat. *)
[@inline] let internal_306 = 306n

(* Liquidity went below zero. *)
[@inline] let internal_liquidity_below_zero_err = 307n

(* Thrown when `(p.dx - r.dx)` is not nat. *)
[@inline] let internal_309 = 308n

(* Contract does not have enough liquidity to execute the swap. *)
[@inline] let internal_insufficient_balance_err = 309n

(* Thrown when `s.i_c >= key.hi.i` and `(s.fee_growth.x - tick_hi.fee_growth_outside.x)` (or `y`) is not nat. *)
[@inline] let internal_311 = 310n

(* Thrown when `s.i_c < key.hi.i` and `(s.fee_growth.x - tick_lo.fee_growth_outside.x)` (or `y`) is not nat. *)
[@inline] let internal_312 = 311n

(* Number of positions underflow. *)
[@inline] let internal_position_underflow_err = 312n

(* Thrown when `(s.fee_growth.x - f_a.x - f_b.x)` is not nat. *)
[@inline] let internal_314 = 313n

(* Thrown when `(s.fee_growth.y - f_a.y - f_b.y)` is not nat. *)
[@inline] let internal_315 = 314n

(* Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat. *)
[@inline] let internal_316 = 315n

(* Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat. *)
[@inline] let internal_317 = 316n

(* Thrown when `s.i_c < i_l.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). *)
[@inline] let internal_sqrt_price_grow_err_1 = 317n

(* Thrown when `i_l.i <= s.i_c && s.i_c < i_u.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). *)
[@inline] let internal_sqrt_price_grow_err_2 = 318n

(* Generic impossible error. *)
[@inline] let internal_impossible_err = 319n

(* Thrown when `seconds_outside` is negative. *)
[@inline] let internal_negative_seconds_outside_err = 320n



#endif
