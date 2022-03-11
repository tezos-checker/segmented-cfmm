// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

(* Note: Some contract specific constants that can be edited per deployment
 * need to be set in the storage (use the Makefile to do so automatically):
 * - fee_bps : nat
 * - ctez_burn_fee_bps : nat
*)

#if CONSTS_MLIGO
#else
#define CONSTS_MLIGO

(* Note: `half_bps_pow` only supports sqrt_price up to this tick index: `2^20 - 1`
   when originated with the 'default_ladder'.
*)
[@inline] let const_max_tick : nat = 1048575n

(* Invalid tick index. Shouldn't be reached. Cannot be defined as failwith
    due to `compile-storage` returning the error.
*)
[@inline] let impossible_tick : nat = const_max_tick + 1n


[@inline] let epoch_time = (0 : timestamp)

// 2^80
[@inline] let pow_2_80n = 1208925819614629174706176n
[@inline] let pow_2_80  = 1208925819614629174706176

(* Not quite constants, but effectively so.
 * Both of these require that their respective inverse is smaller than 10000,
 * which is a requirement of bps numbers.
*)

[@inline] let one_minus_fee_bps (c : constants) : nat =
  abs(10000n - c.fee_bps)

[@inline] let one_minus_ctez_burn_fee_bps (c : constants) : nat =
  abs(10000n - c.ctez_burn_fee_bps)

#endif
