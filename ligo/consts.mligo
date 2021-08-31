// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if CONSTS_MLIGO
#else
#define CONSTS_MLIGO

(* Note: `half_bps_pow` only supports sqrt_price up to this tick index: `2^20 - 1`. *)
[@inline] let const_max_tick : nat = 1048575n

(* Invalid tick index. Shouldn't be reached. Cannot be defined as failwith
    due to `compile-storage` returning the error.
*)
[@inline] let impossible_tick : nat = const_max_tick + 1n

(* Some contract specific constants, to be edited per deployment
 todo implement burn [@inline] let const_ctez_burn_fee_bps : nat = 5n *)

(* Invariant : const_fee_bps + const_one_minus_fee_bps = 10000n *)
[@inline] let const_fee_bps : nat = 10n  (* CHANGEME if need be *)
[@inline] let const_one_minus_fee_bps : nat = 9990n (* CHANGEME if need be*)
#endif
[@inline] let epoch_time = ("1970-01-01T00:00:00Z" : timestamp)