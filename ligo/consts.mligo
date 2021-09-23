// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

(* Note: Some contract specific constants that can be edited per deployment
 * Need to be declared (use the Makefile to do so automatically):
 * - const_fee_bps : nat
 * - const_one_minus_fee_bps : nat
 * - const_ctez_burn_fee_bps : nat
*)

#if CONSTS_MLIGO
#else
#define CONSTS_MLIGO

(* Note: `half_bps_pow` only supports sqrt_price up to this tick index: `2^20 - 1`. *)
[@inline] let const_max_tick : nat = 1048575n

(* Invalid tick index. Shouldn't be reached. Cannot be defined as failwith
    due to `compile-storage` returning the error.
*)
[@inline] let impossible_tick : nat = const_max_tick + 1n


[@inline] let epoch_time = (0 : timestamp)

#endif
