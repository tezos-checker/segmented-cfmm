<!--
- SPDX-FileCopyrightText: 2021 Arthur Breitman
-
- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
-->

<!--
NOTE: This file should not be modified directly.
Use `stack scripts/generate_error_code.hs` instead.
-->

## Error Codes

Here is a summary of all the error codes thrown by the contract.
(The list of errors may be inaccurate and incomplete, it will be updated during the implementation.)


#### Invalid Input Error Codes

| Error Code       | Error Label  | Error Argument  | Description                             |
|------------------|--------------|-----------------|-----------------------------------------|
| 100 | `invalid_witness_err` | `-` | Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick. |
| 101 | `too_big_price_change_err` | `-` | The action would apply too big of a change to the price, which is not allowed. We assume that the amount of X or Y tokens in the contract should not change by more than 30% at once (in some circumstances, a larger change may be allowed). |
| 102 | `price_out_of_bounds_err` | `-` | The action would put the price out of bounds. Used tick indices should remain within `[-1048575; 1048575]` range, and, respectively, amount of one token type in the pair should not exceed `exp(0.0001)^1048575 ≈ 3.46 * 10^45` times the amount in the other token. |
| 103 | `past_deadline_err` | `(deadline :timestamp, executed_at :timestamp)` | Swap has expired: now > deadline. |
| 104 | `smaller_than_min_asset_err` | `(min :nat, actual :nat)` | Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`. |
| 105 | `tick_not_exist_err` | `-` | User provided tick is not initialized. |
| 106 | `high_tokens_err` | `(max :nat, actual :int)` | The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`. |
| 107 | `invalid_x_prime_contract_err` | `-` | The X prime contract address provided is not a segmented-cfmm contract. |
| 108 | `observe_outdated_timestamp_err` | `(oldest_stored :timestamp, requested :timestamp)` | Some of the timestamps passed to the `observe` entrypoint are too far back in the past. |
| 109 | `observe_future_timestamp_err` | `(newest_available :timestamp, requested :timestamp)` | Some of the timestamps passed to the `observe` entrypoint are yet in the future. |
| 110 | `tick_order_err` | `-` | When setting a new position, `upper_tick_index` must be strictly greater than `lower_tick_index`. When observing cumulative values at range, `upper_tick_index` must be greater or equal than `lower_tick_index`. |
| 111 | `position_liquidity_below_zero_err` | `-` | Liquidity of a position went below zero. |
| 112 | `incorrect_tick_spacing_err` | `-` | Tick indexes must be a multiple of the tick spacing. |
| 113 | `non_zero_transfer_err` | `-` | Contract call also transfers some XTZ; this is not allowed, it would be stuck. |


#### Contract Configuration Error Codes

| Error Code       | Error Label  | Error Argument  | Description                             |
|------------------|--------------|-----------------|-----------------------------------------|
| 200 | `asset_transfer_invalid_entrypoints_err` | `-` | The `x_token_address` or `y_token_address` has no transfer entrypoint. |
| 201 | `asset_update_operator_invalid_entrypoints_err` | `-` | The `x_token_address` or `y_token_address` has no `update_operator` entrypoint. |
| 202 | `asset_approve_invalid_entrypoints_err` | `-` | The `x_token_address` or `y_token_address` has no `approve` entrypoint. |



#### Internal Error Codes

| Error Code       | Error Label  | Error Argument  | Description                             |
|------------------|--------------|-----------------|-----------------------------------------|
| 300 | `internal_impossible_err` | `-` | Generic impossible error. |
| 301 | `internal_tick_not_exist_err` | `-` | Tick is not initialized. |
| 302 | `internal_epoch_bigger_than_now_err` | `-` | Time now is smaller than epoch time. |
| 303 | `internal_fee_more_than_100_percent_err` | `-` | The `fee_bps` is initialized to be higher than 10000 (100%). |
| 304 | `internal_bad_sqrt_price_move_x_direction` | `-` | Unexpected price direction movement after sqrt_price_move_x. |
| 305 | `internal_bad_sqrt_price_move_y_direction` | `-` | Unexpected price direction movement after sqrt_price_move_y. |
| 306 | `internal_flip_fee_growth_outside_err` | `-` | Flip for `fee_growth_outside` failed. (This is an invariant of the contract). |
| 307 | `internal_307` | `-` | Thrown when `(p.dx - dx_consumed)` or `(p.dy - dy_consumed)` is not nat. |
| 308 | `internal_liquidity_below_zero_err` | `-` | Liquidity of a tick went below zero. |
| 309 | `internal_309` | `-` | Thrown when `(p.dx - r.dx)` is not nat. |
| 311 | `internal_311` | `-` | Thrown when `s.cur_tick_index.i >= upper_tick_index.i` and `(s.fee_growth.x - upper_tick.fee_growth_outside.x)` (or `y`) is not nat. |
| 312 | `internal_312` | `-` | Thrown when `s.cur_tick_index.i < lower_tick_index.i` and `(s.fee_growth.x - lower_tick.fee_growth_outside.x)` (or `y`) is not nat. |
| 313 | `internal_position_underflow_err` | `-` | Number of positions underflow. |
| 316 | `internal_316` | `-` | Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat. |
| 317 | `internal_317` | `-` | Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat. |
| 318 | `internal_sqrt_price_grow_err_1` | `-` | Thrown when `s.cur_tick_index.i < p.lower_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). |
| 319 | `internal_sqrt_price_grow_err_2` | `-` | Thrown when `p.lower_tick_index.i <= s.cur_tick_index.i && s.cur_tick_index.i < p.upper_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). |
| 320 | `internal_negative_seconds_outside_err` | `-` | Thrown when `seconds_outside` is negative. |
| 321 | `internal_bad_access_to_observation_buffer` | `-` | Failed to access a value in time-weighted i_c cumulative sums buffer. |
| 322 | `internal_observe_bin_search_failed` | `-` | Some issue with binary search in `observe` entrypoint. |
| 323 | `internal_non_empty_position_gc_err` | `-` | Attempt to garbade collect a tick with non-zero liquidity net. |
| 324 | `internal_flip_seconds_per_liquidity_outside_err` | `-` | Flip of `seconds_per_liquidity_outside` failed. (This is an invariant of the contract). |
| 325 | `internal_unexpected_income_err` | `-` | Position creation/change unexpectedly transferred tokens to someone |
| 326 | `internal_negative_price` | `-` | Price became negative when crossing a tick |


