// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if MATH_MLIGO
#else
#define MATH_MLIGO

#include "types.mligo"

[@inline] let fixed_point_mul (a : fixed_point) (b : fixed_point) : fixed_point =
    { v = a.v * b.v ; offset = a.offset + b.offset }

let ceildiv (numerator : nat) (denominator : nat) : nat = abs ((- numerator) / (int denominator))
let ceildiv_int (numerator : int) (denominator : int) : int = - ((- numerator) /  denominator)
let floordiv (numerator : nat) (denominator : nat) : nat =  numerator / denominator

(*
  When the `sqrt_price` moves from `y` to `x`, calculate the corresponding change to `cur_tick_index`:
    log_{sqrt(1.0001)}(x/y)
    2 * ln(x/y) / ln(1.0001)
 *)
(* accurate for x/y in [0.7, 1.5] *)
(* Note, for simplicity, our sqrt_prices are not on a grid of 0.5 bps, they are on a grid of 10000 (Exp[0.0005] - 1) bps *)
[@inline]
let floor_log_half_bps ((x, y, out_of_bounds_err) : nat * nat * nat) : int =
    let tenx = 10n * x in
    let _ : unit =
            if tenx < 7n * y or tenx > 15n * y then failwith out_of_bounds_err
            else unit
    in
    let x_plus_y = x + y in
    let num : int = 60003 * (x - y) * x_plus_y in
    let denom = x_plus_y * x_plus_y + 2n * x * y in
    num / denom

[@inline]
let floor_log_half_bps_x80 ((x, y, out_of_bounds_err) : x80n * x80n * nat) : int =
    match (x, y) with
        ({x80 = x0}, {x80 = y0}) -> floor_log_half_bps(x0, y0, out_of_bounds_err)

let assert_nat (x, error_code : int * nat) : nat =
    match is_nat x with
    | None -> (failwith error_code : nat)
    | Some n -> n

(* `Bitwise.shift_right x y` is only defined for `y <= 256n`.
    This function handles larger values of `y`.
 *)
let rec stepped_shift_right (x, y : nat * nat) : nat =
    let max_shift = 256n in
    if y <= max_shift then
        Bitwise.shift_right x y
    else
        let new_x = Bitwise.shift_right x max_shift in
        stepped_shift_right (new_x, abs (y - max_shift))

(* `Bitwise.shift_left x y` is only defined for `y <= 256n`.
    This function handles larger values of `y`.
 *)
let rec stepped_shift_left (x, y : nat * nat) : nat =
    let max_shift = 256n in
    if y <= max_shift then
        Bitwise.shift_left x y
    else
        let new_x = Bitwise.shift_left x max_shift in
        stepped_shift_left (new_x, abs (y - max_shift))

let rec half_bps_pow_rec ((tick, acc, ladder_key, ladder) : nat * fixed_point * ladder_key * ladder) : fixed_point =
    if tick = 0n then
        acc
    else
        let (half, rem) =
                match ediv tick 2n with
                | None -> (failwith internal_impossible_err : nat * nat)
                | Some d -> d
        in
        let fixed_point =
                match Big_map.find_opt ladder_key ladder with
                | None -> (failwith price_out_of_bounds_err : fixed_point)
                | Some h -> h
        in
        let new_acc = if rem = 0n then acc else fixed_point_mul fixed_point acc in
        let new_ladder_key = {ladder_key with exp = ladder_key.exp + 1n} in
        half_bps_pow_rec (half, new_acc, new_ladder_key, ladder)

(*
  For a tick index `i`, calculate the corresponding `sqrt_price`:
    sqrt(e^bps)^i * 2^80
  using the exponentiation by squaring method, where:
    bps = 0.0001
 *)
let half_bps_pow (tick, ladder : int * ladder) : x80n =
    let product = half_bps_pow_rec (abs tick, {v=1n;offset=0}, {exp=0n;positive=(tick > 0)}, ladder) in
    let doffset = -80 - product.offset in
    if doffset > 0 then
        {x80 = stepped_shift_right (product.v, abs doffset)}
    else
        (* This branch should almost never happen, in general the price we get is not a round number. *)
        {x80 = stepped_shift_left (product.v, abs doffset)}

#endif
