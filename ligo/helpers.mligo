// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if HELPERS_MLIGO
#else
#define HELPERS_MLIGO

#include "errors.mligo"
#include "math.mligo"
#include "types.mligo"

(*  Calculate the new `sqrt_price` after a deposit of `dx` `x` tokens.
    Derived from equation 6.15:
        Δ(1 / √P) = Δx / L
        1 / √P_new - 1 / √P_old = Δx / L
    Since we store √P mutiplied by 2^80 (i.e. sqrt_price = √P * 2^80):
        1 / (sqrt_price_new / 2^80) - 1 / (sqrt_price_old / 2^80) = Δx / L
    Solving for sqrt_price_new:
        sqrt_price_new = (2^80 * L * sqrt_price_old) / (2^80 * L + Δx * sqrt_price_old)

    Example:
        Assume a pool with 10 `x` tokens and 1000 `y` tokens, which implies:
            L = sqrt(xy) = sqrt(10*1000) = 100
            P = y/x = 1000/10 = 100
            sqrt_price = sqrt(100) * 2^80 = 12089258196146291747061760

        Adding 10 `x` tokens to the pool should result in:
            x = 20
            y = L^2 / x = 500
            P = 500 / 20 = 25
            sqrt_price = sqrt(25) * 2^80 = 6044629098073145873530880

        And indeed:
            $ ligo compile-expression --init-file ligo/helpers.mligo cameligo \
              "sqrt_price_move_x 100n {x80 = 12089258196146291747061760n} 10n"
            6044629098073145873530880
   *)
let sqrt_price_move_x (liquidity : nat) (sqrt_price_old : x80n) (dx : nat) : x80n =
    (* floordiv because we want to overstate how much this trade lowers the price *)
    let sqrt_price_new =
        {x80 = floordiv
            (Bitwise.shift_left (liquidity * sqrt_price_old.x80) 80n)
            ((Bitwise.shift_left liquidity 80n) + dx * sqrt_price_old.x80)
        } in
#if DEBUG
    let _ : unit =
        if sqrt_price_new <= sqrt_price_old
            then unit
            else failwith "sqrt_price_move_x: sqrt_price moved in the wrong direction" in
#endif
    sqrt_price_new


(*  Calculate the new `sqrt_price` after a deposit of `dy` `y` tokens.
    Derived from equation 6.13:
        Δ(√P) = Δy /L
        √P_new - √P_old = Δy /L
    Since we store √P mutiplied by 2^80 (i.e. sqrt_price = √P * 2^80):
        sqrt_price_new / 2^80 - sqrt_price_old / 2^80 = Δy /L
    Solving for sqrt_price_new:
        sqrt_price_new = 2^80 * (Δy / L) + sqrt_price_old

    Example:
        Assume a pool with 10 `x` tokens and 1000 `y` tokens, which implies:
            L = sqrt(xy) = sqrt(10*1000) = 100
            P = y/x = 1000/10 = 100
            sqrt_price = sqrt(100) * 2^80 = 12089258196146291747061760

        Adding 1000 `y` tokens to the pool should result in:
            y = 2000
            x = L^2 / y = 5
            P = 2000 / 5 = 400
            sqrt_price = sqrt(400) * 2^80 = 24178516392292583494123520

        And indeed:
            $ ligo compile-expression --init-file ligo/helpers.mligo cameligo \
              "sqrt_price_move_y 100n {x80 = 12089258196146291747061760n} 1000n"
            24178516392292583494123520
   *)
let sqrt_price_move_y (liquidity : nat) (sqrt_price_old : x80n) (dy : nat) : x80n =
    (* ceildiv because we want to overstate how much this trade increases the price *)
    let sqrt_price_new =
        { x80 =
            ceildiv (Bitwise.shift_left dy 80n) liquidity + sqrt_price_old.x80
        } in
#if DEBUG
    let _ : unit =
        if sqrt_price_new >= sqrt_price_old
            then unit
            else failwith "sqrt_price_move_y: sqrt_price moved in the wrong direction" in
#endif
    sqrt_price_new

(* Helper function to grab a tick we know exists in the tick indexed state. *)
[@inline]
let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) (error_code: nat) : tick_state =
    match Big_map.find_opt index ticks with
    | None -> failwith error_code
    | Some state -> state

(* Check if a request has expired. *)
[@inline]
let check_deadline (deadline : timestamp) : unit =
    if Tezos.now > deadline
        then ([%Michelson ({| { FAILWITH } |} : nat * (timestamp * timestamp) -> unit)]
            (past_deadline_err, (deadline, Tezos.now)) : unit)
        else unit

[@inline]
let get_registered_cumulatives_unsafe (buffer : timed_cumulatives_buffer) (i : nat) : timed_cumulatives =
    match Big_map.find_opt i buffer.map with
    | None -> failwith internal_bad_access_to_observation_buffer
    | Some v -> v

[@inline]
let get_last_cumulatives (buffer : timed_cumulatives_buffer) : timed_cumulatives =
    get_registered_cumulatives_unsafe buffer buffer.last


(* Ensure tick index is multiple of tick spacing. *)
[@inline]
let check_multiple_of_tick_spacing (tick_index, tick_spacing: tick_index * nat) : unit =
    if (tick_index.i mod tick_spacing = 0n)
        then unit
        else failwith incorrect_tick_spacing_err

#endif
