// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"
#include "helpers.mligo"
#include "transfers.mligo"
#include "math.mligo"
#include "swaps.mligo"

(* TODO: make positions into an FA2 *)

#if !DUMMY_PRAGMA1
This is an example of conditionally present code, remove it once normal pragmas are set.
#endif

let rec initialize_tick ((ticks, i, i_l,
    initial_fee_growth_outside,
    initial_seconds_outside,
    initial_seconds_per_liquidity_outside) : tick_map * tick_index * tick_index * balance_nat * nat * nat) : tick_map =
    if Big_map.mem i ticks then
        ticks
    else if i_l.i > i.i then
        (failwith "Invalid witness" : tick_map)
    else
        let tick = get_tick ticks i_l in
        let i_next = tick.next in
        if i_next.i > i.i then
            let tick_next = get_tick ticks i_next in
            let ticks = Big_map.update i_l (Some {tick with next = i}) ticks in
            let ticks = Big_map.update i_next (Some {tick_next with prev = i}) ticks in
            let ticks = Big_map.update i (Some {
                prev = i_l ;
                next = i_next ;
                delta_liquidity = 0 ;
                n_positions = 0n ;
                fee_growth_outside = initial_fee_growth_outside;
                seconds_outside = initial_seconds_outside;
                seconds_per_liquidity_outside = initial_seconds_per_liquidity_outside;
                sqrt_price = half_bps_pow i.i}) ticks in
            ticks
        else
            initialize_tick (ticks, i, i_next, initial_fee_growth_outside, initial_seconds_outside, initial_seconds_per_liquidity_outside)

(* Account for the fact that this tick is a boundary for one more (or one less) position. *)
let cover_tick_with_position (ticks : tick_map) (i : tick_index) (pos_delta : int) (liquidity_delta : int) =
    let tick = get_tick ticks i in
    let n_pos = assert_nat (tick.n_positions + pos_delta) in
    let new_liquidity = tick.liquidity_net + liquidity_delta in
    if n_pos = 0n then
        (*  Garbage collect the tick.
            The largest and smallest tick are initialized with n_positions = 1 so they cannot
            be accidentally garbage collected. *)
        let _ : unit = if new_liquidity <> 0 then
            failwith "a tick with non-zero liquidity was unexpectedly garbage collected"
            else unit in
        let prev = get_tick ticks tick.prev in
        let next = get_tick ticks tick.next in
        (* prev links to next and next to prev, skipping the deleted tick *)
        let prev = {prev with next = tick.next} in
        let next = {next with prev = tick.prev} in
        let ticks = Big_map.update i (None : tick_state option) ticks in
        let ticks = Big_map.update tick.prev (Some prev) ticks in
        let ticks = Big_map.update tick.next (Some next) ticks in
        ticks
    else
        Big_map.update i
            (Some {tick with
                    n_positions = n_pos;
                    liquidity_net = new_liquidity })
            ticks

let collect_fees (s : storage) (key : position_index) : storage * balance_nat =
    let position = match Big_map.find_opt key s.positions with
    | None -> (failwith "position does not exist" : position_state)
    | Some position -> position in
    let tick_lo = get_tick s.ticks key.lo in
    let tick_hi = get_tick s.ticks key.hi in
    let f_a = if s.i_c >= key.hi.i then
        { x = assert_nat (s.fee_growth.x - tick_hi.fee_growth_outside.x);
          y = assert_nat (s.fee_growth.y - tick_hi.fee_growth_outside.y)}
    else
        tick_hi.fee_growth_outside in
    let f_b = if s.i_c >= key.lo.i then
        tick_lo.fee_growth_outside
    else
        { x = assert_nat (s.fee_growth.x - tick_lo.fee_growth_outside.x) ;
          y = assert_nat (s.fee_growth.y - tick_lo.fee_growth_outside.y) } in
    let fee_growth_inside = {
        x = assert_nat (s.fee_growth.x - f_a.x - f_b.x) ;
        y = assert_nat (s.fee_growth.y - f_a.y - f_b.y) } in
    let fees = {
        x = Bitwise.shift_right ((assert_nat (fee_growth_inside.x - position.fee_growth_inside_last.x)) * position.liquidity) 128n;
        y = Bitwise.shift_right ((assert_nat (fee_growth_inside.y - position.fee_growth_inside_last.y)) * position.liquidity) 128n} in
    let position = {position with fee_growth_inside_last = fee_growth_inside} in
    let positions = Big_map.update key (Some position) s.positions in
    ({s with positions = positions}, fees)


let set_position (s : storage) (i_l : tick_index) (i_u : tick_index) (i_l_l : tick_index) (i_u_l : tick_index) (delta_liquidity : int) (to_x : address) (to_y : address) : result =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = if s.i_c >= i_l.i then
        initialize_tick (ticks, i_l, i_l_l, s.fee_growth, assert_nat (Tezos.now - epoch_time), 42n (*FIXME*))
    else
        initialize_tick (ticks, i_l, i_l_l, {x = 0n ; y = 0n}, 0n, 0n)  in
    let ticks = if s.i_c >= i_u.i then
        initialize_tick (ticks, i_u, i_u_l, s.fee_growth, assert_nat (Tezos.now - epoch_time), 42n (*FIXME*))
    else
        initialize_tick (ticks, i_u, i_u_l, {x = 0n ; y = 0n}, 0n, 0n)  in

    (* Form position key. *)
    let position_key = {owner=Tezos.sender ; lo=i_l; hi=i_u} in
    (* Grab existing position or create an empty one *)
    let (position, is_new) = match (Big_map.find_opt position_key s.positions) with
    | Some position -> (position, false)
    | None -> ({liquidity = 0n ; fee_growth_inside_last = {x = 0n; y = 0n} ; seconds_per_liquidity_inside = 0n}, true) in
    (* Get accumulated fees for this position. *)
    let s, fees = collect_fees s position_key in
    (* Update liquidity of position. *)
    let liquidity_new = assert_nat (position.liquidity + delta_liquidity) in
    let position = {position with liquidity = liquidity_new} in
    (* Reference counting the positions associated with a tick *)
    let ticks = (if liquidity_new = 0n then
        if is_new then
            ticks
        else
            let ticks = cover_tick_with_position ticks i_l (-1) (-liquidity_delta) in
            let ticks = cover_tick_with_position ticks i_u (-1) liquidity_delta in
            ticks
    else
        if is_new then
            let ticks = cover_tick_with_position ticks i_l (1) liquidity_delta in
            let ticks = cover_tick_with_position ticks i_u (1) (-liquidity_delta) in
            ticks
        else
            ticks) in
    (* delete the position if liquidity has fallen to 0 *)
    let position_entry : position_state option = if liquidity_new = 0n then None else Some {position with liquidity = liquidity_new} in
    let positions = Big_map.update position_key position_entry s.positions in
    (* Compute how much should be deposited / withdrawn to change liquidity by delta_liquidity *)

    (* Grab cached prices for the interval *)
    let tick_u = get_tick ticks i_u in
    let tick_l = get_tick ticks i_l in
    let srp_u = tick_u.sqrt_price in
    let srp_l = tick_l.sqrt_price in

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.i_c < i_l.i then
        (s, {
            (* If I'm adding liquidity, x will be positive, I want to overestimate it, if x I'm taking away
                liquidity, I want to to underestimate what I'm receiving. *)
            x = ceildiv_int (delta_liquidity * (int (Bitwise.shift_left (assert_nat (srp_u - srp_l)) 90n))) (int (srp_l * srp_u)) ;
            y = 0})
    else if i_l.i <= s.i_c && s.i_c < i_u.i then
        (* update interval we are in, if need be ... *)
        let s = {s with lo = if i_l.i > s.lo.i then i_l else s.lo ; liquidity = assert_nat (s.liquidity + delta_liquidity)} in
        (s, {
            x = ceildiv_int (delta_liquidity * (int (Bitwise.shift_left (assert_nat (srp_u - s.sqrt_price)) 90n))) (int (s.sqrt_price * srp_u)) ;
            y = shift_int (delta_liquidity * (s.sqrt_price - srp_l)) (-80)
            })
    else (* i_c >= i_u *)
        (s, {x = 0 ; y = shift_int (delta_liquidity * (srp_u - srp_l)) (-80) }) in

    (* Collect fees to increase withdrawal or reduce required deposit. *)
    let delta = {x = delta.x - fees.x ; y = delta.y - fees.y} in

    let op_x = if delta.x > 0 then
        x_transfer Tezos.sender Tezos.self_address (abs delta.x)
    else
        x_transfer Tezos.self_address to_x (abs delta.x) in

    let op_y = if delta.y > 0 then
        y_transfer Tezos.sender Tezos.self_address (abs delta.y)
    else
        y_transfer Tezos.self_address to_y (abs delta.y) in

    ([op_x ; op_y], {s with positions = positions; ticks = ticks})


type views =
    | IC_sum of int

let get_time_weighted_sum (s : storage) (c : views contract) : result =
    ([Tezos.transaction (IC_sum s.time_weighted_ic_sum) 0mutez c], s)

type parameter =
| X_to_Y of x_to_y_param
| Y_to_X of y_to_x_param
| Set_position of set_position_param (* TODO add deadline, maximum tokens contributed, and maximum liquidity present *)
| X_to_X_prime of address (* equivalent to token_to_token *)
| Get_time_weighted_sum of views contract

let update_time_weighted_sum (s : storage) : storage =
    let new_sum = s.time_weighted_ic_sum + (Tezos.now - s.last_ic_sum_update) * s.i_c
    in {s with time_weighted_ic_sum = new_sum ; last_ic_sum_update = Tezos.now}

let main ((p, s) : parameter * storage) : result =
(* start by updating the time weighted price oracle *)
let s = update_time_weighted_sum s in
(* dispatch call to the proper entrypoint *)
 match p with
| X_to_Y p -> x_to_y s p
| Y_to_X p -> y_to_x s p
| Set_position p -> set_position s p.i_l p.i_u p.i_l_l p.i_u_l p.delta_liquidity p.to_x p.to_y
| Get_time_weighted_sum contract -> get_time_weighted_sum s contract
| X_to_X_prime _ -> (failwith "not implemented" : result) (*TODO implement iff Y is FA12 *)