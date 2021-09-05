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

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
| 100 | `invalid_witness_err` | Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick. |
| 101 | `log_out_of_bounds_err` | Log out of bounds. |
| 102 | `end_ladder_reached_err` | Should not reach end of ladder. |
| 103 | `past_deadline_err` | Swap has expired: now > deadline. |
| 104 | `smaller_than_min_asset_err` | Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`. |
| 105 | `tick_not_exist_err` | User provided tick is not initialized. |
| 106 | `high_tokens_err` | The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`. |
| 107 | `invalid_timestamp_err` | Some of the timestamps passed to the `observe` entrypoint are too far back in the past. |


#### Contract Configuration Error Codes

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
| 200 | `asset_transfer_invalid_entrypoints_err` | The `const_x_token_entrypoint` or `const_y_token_entrypoint` has no transfer entrypoint. |



#### Internal Error Codes

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
| 300 | `internal_tick_not_exist_err` | Tick is not initialized. |
| 301 | `internal_epoch_bigger_than_now_err` | Time now is smaller than epoch time. |
| 302 | `internal_fee_more_than_100_percent_err` | The `const_fee_bps` is initialized to be higher than 10000 (100%). |
| 303 | `internal_303` | Thrown when `(p.s.sqrt_price - sqrt_price_new)` is not nat. |
| 304 | `internal_304` | Thrown when `(sqrt_price_new - p.s.sqrt_price)` is not nat. |
| 305 | `flip_fee_growth_outside_err` | Flip for `fee_growth_outside` failed. (This is an invariant of the contract). |
| 306 | `internal_306` | Thrown when `(p.dx - dx_consummed)` is not nat. |
| 307 | `internal_liquidity_below_zero_err` | Liquidity went below zero. |
| 308 | `internal_309` | Thrown when `(p.dx - r.dx)` is not nat. |
| 309 | `internal_insufficient_balance_err` | Contract does not have enough liquidity to execute the swap. |
| 310 | `internal_311` | Thrown when `s.i_c >= key.hi.i` and `(s.fee_growth.x - tick_hi.fee_growth_outside.x)` (or `y`) is not nat. |
| 311 | `internal_312` | Thrown when `s.i_c < key.hi.i` and `(s.fee_growth.x - tick_lo.fee_growth_outside.x)` (or `y`) is not nat. |
| 312 | `internal_position_underflow_err` | Number of positions underflow. |
| 313 | `internal_314` | Thrown when `(s.fee_growth.x - f_a.x - f_b.x)` is not nat. |
| 314 | `internal_315` | Thrown when `(s.fee_growth.y - f_a.y - f_b.y)` is not nat. |
| 315 | `internal_316` | Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat. |
| 316 | `internal_317` | Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat. |
| 317 | `internal_sqrt_price_grow_err_1` | Thrown when `s.i_c < i_l.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). |
| 318 | `internal_sqrt_price_grow_err_2` | Thrown when `i_l.i <= s.i_c && s.i_c < i_u.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract). |
| 319 | `internal_impossible_err` | Generic impossible error. |
| 320 | `internal_negative_seconds_outside_err` | Thrown when `seconds_outside` is negative. |


