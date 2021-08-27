// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if HELPERS_MLIGO
#else
#define HELPERS_MLIGO

#include "errors.mligo"
#include "math.mligo"

let sqrt_price_move (liquidity : nat) (sqrt_price : x80n) (dx : nat) : x80n =
    (* floordiv because we want to overstate how much this trade lowers the price *)
    {x80 = floordiv
        (Bitwise.shift_left (liquidity * sqrt_price.x80) 80n)
        ((Bitwise.shift_left liquidity 80n) + dx * sqrt_price.x80)}

(* Helper function to grab a tick we know exists in the tick indexed state. *)
let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) (error_code: nat) : tick_state =
    match Big_map.find_opt index ticks with
    | None -> failwith error_code
    | Some state -> state

#endif
