// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"
#include "helpers.mligo"
#include "transfers.mligo"
#include "math.mligo"
#include "swaps.mligo"
#include "token/fa2.mligo"
#include "defaults.mligo"


#if !DUMMY_PRAGMA1
This is an example of conditionally present code, remove it once normal pragmas are set.
#endif

let rec initialize_tick ((ticks, i, i_l,
    initial_fee_growth_outside,
    initial_seconds_outside,
    initial_seconds_per_liquidity_outside) : tick_map * tick_index * tick_index * balance_nat_x128 * nat * x128n) : tick_map =
    if Big_map.mem i ticks then
        ticks
    else if i_l.i > i.i then
        (failwith invalid_witness_err : tick_map)
    else
        let tick = get_tick ticks i_l tick_not_exist_err in
        let i_next = tick.next in
        if i_next.i > i.i then
            let tick_next = get_tick ticks i_next internal_tick_not_exist_err in
            let ticks = Big_map.update i_l (Some {tick with next = i}) ticks in
            let ticks = Big_map.update i_next (Some {tick_next with prev = i}) ticks in
            let ticks = Big_map.update i (Some {
                prev = i_l ;
                next = i_next ;
                liquidity_net = 0 ;
                n_positions = 0n ;
                fee_growth_outside = initial_fee_growth_outside;
                seconds_outside = initial_seconds_outside;
                seconds_per_liquidity_outside = initial_seconds_per_liquidity_outside;
                sqrt_price = half_bps_pow i.i}) ticks in
            ticks
        else
            initialize_tick (ticks, i, i_next, initial_fee_growth_outside, initial_seconds_outside, initial_seconds_per_liquidity_outside)

let incr_n_positions (ticks : tick_map) (i : tick_index) (incr : int) =
    let tick = get_tick ticks i internal_tick_not_exist_err in
    let n_pos = assert_nat (tick.n_positions + incr, internal_position_underflow_err) in
    if n_pos = 0n then
        (*  Garbage collect the tick.
            The largest and smallest tick are initialized with n_positions = 1 so they cannot
            be accidentally garbage collected. *)
        let prev = get_tick ticks tick.prev internal_tick_not_exist_err in
        let next = get_tick ticks tick.next internal_tick_not_exist_err in
        (* prev links to next and next to prev, skipping the deleted tick *)
        let prev = {prev with next = tick.next} in
        let next = {next with prev = tick.prev} in
        let ticks = Big_map.update i (None : tick_state option) ticks in
        let ticks = Big_map.update tick.prev (Some prev) ticks in
        let ticks = Big_map.update tick.next (Some next) ticks in
        ticks
    else
        Big_map.update i (Some {tick with n_positions = n_pos}) ticks

let calc_fee_growth_inside (s : storage) (lower_tick_index : tick_index) (upper_tick_index : tick_index) : balance_nat_x128 =
    let lower_tick = get_tick s.ticks lower_tick_index internal_tick_not_exist_err in
    let upper_tick = get_tick s.ticks upper_tick_index internal_tick_not_exist_err in

    // equation 6.17
    let fee_above =
        if s.cur_tick_index.i >= upper_tick_index.i then
            { x = {x128 = assert_nat (s.fee_growth.x.x128 - upper_tick.fee_growth_outside.x.x128, internal_311) };
              y = {x128 = assert_nat (s.fee_growth.y.x128 - upper_tick.fee_growth_outside.y.x128, internal_311) };
            }
        else
            upper_tick.fee_growth_outside in
    // equation 6.18
    let fee_below =
        if s.cur_tick_index.i >= lower_tick_index.i then
            lower_tick.fee_growth_outside
        else
            { x = {x128 = assert_nat (s.fee_growth.x.x128 - lower_tick.fee_growth_outside.x.x128, internal_312) };
              y = {x128 = assert_nat (s.fee_growth.y.x128 - lower_tick.fee_growth_outside.y.x128, internal_312) };
            } in
    // equation 6.19
    { x = {x128 = assert_nat (s.fee_growth.x.x128 - fee_above.x.x128 - fee_below.x.x128, internal_314) };
      y = {x128 = assert_nat (s.fee_growth.y.x128 - fee_above.y.x128 - fee_below.y.x128, internal_315) };
    }

let collect_fees (s : storage) (key : position_index) (position : position_state) : storage * balance_nat * position_state =
    let fee_growth_inside = calc_fee_growth_inside s key.lower_tick_index key.upper_tick_index in
    let fees = {
        x = Bitwise.shift_right ((assert_nat (fee_growth_inside.x.x128 - position.fee_growth_inside_last.x.x128, internal_316)) * position.liquidity) 128n;
        y = Bitwise.shift_right ((assert_nat (fee_growth_inside.y.x128 - position.fee_growth_inside_last.y.x128, internal_317)) * position.liquidity) 128n} in
    let position = {position with fee_growth_inside_last = fee_growth_inside} in
    let positions = Big_map.update key (Some position) s.positions in
    ({s with positions = positions}, fees, position)

let set_position (s : storage) (i_l : tick_index) (i_u : tick_index) (i_l_l : tick_index) (i_u_l : tick_index) (liquidity_delta : int) (to_x : address) (to_y : address) : result =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = if s.cur_tick_index.i >= i_l.i then
        initialize_tick (ticks, i_l, i_l_l, s.fee_growth, assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err), s.seconds_per_liquidity_cumulative)
    else
        initialize_tick (ticks, i_l, i_l_l, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in
    let ticks = if s.cur_tick_index.i >= i_u.i then
        initialize_tick (ticks, i_u, i_u_l, s.fee_growth, assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err), s.seconds_per_liquidity_cumulative)
    else
        initialize_tick (ticks, i_u, i_u_l, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in

    (* Form position key. *)
    let position_key = {owner=Tezos.sender ; lower_tick_index=i_l; upper_tick_index=i_u} in
    (* Grab existing position or create an empty one *)
    let (position, is_new) = match (Big_map.find_opt position_key s.positions) with
    | Some position -> (position, false)
    | None ->
        let new_position =
                { liquidity = 0n;
                  fee_growth_inside_last = calc_fee_growth_inside s position_key.lower_tick_index position_key.upper_tick_index;
                  position_id = s.new_position_id;
                } in
        (new_position, true) in
    (* Get accumulated fees for this position. *)
    let s, fees, position =
        if is_new then
            (s, {x = 0n; y = 0n}, position)
        else
            collect_fees s position_key position
        in
    (* Update liquidity of position. *)
    let liquidity_new = assert_nat (position.liquidity + liquidity_delta, internal_liquidity_below_zero_err) in
    let position = {position with liquidity = liquidity_new} in
    (* Reference counting the positions associated with a tick *)
    let ticks = (if liquidity_new = 0n then
        if is_new then
            ticks
        else
            let ticks = incr_n_positions ticks i_l (-1) in
            let ticks = incr_n_positions ticks i_u (-1) in
            ticks
    else
        if is_new then
            let ticks = incr_n_positions ticks i_l (1) in
            let ticks = incr_n_positions ticks i_u (1) in
            ticks
        else
            ticks) in
    (* delete the position if liquidity has fallen to 0 *)
    let (positions, position_indexes, new_position_id) =
        if liquidity_new = 0n then
            ( Big_map.remove position_key s.positions
            , Big_map.remove position.position_id s.position_indexes
            , s.new_position_id
            )
        else
            ( Big_map.add position_key ({position with liquidity = liquidity_new}) s.positions
            , ( if is_new then
                  Big_map.add position.position_id position_key s.position_indexes
                else s.position_indexes
              )
            , ( if is_new then s.new_position_id + 1n else s.new_position_id )
            ) in

    (* Compute how much should be deposited / withdrawn to change liquidity by liquidity_net *)

    (* Grab cached prices for the interval *)
    let tick_u = get_tick ticks i_u internal_tick_not_exist_err in
    let tick_l = get_tick ticks i_l internal_tick_not_exist_err in
    let srp_u = tick_u.sqrt_price in
    let srp_l = tick_l.sqrt_price in

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.cur_tick_index.i < i_l.i then
        (s, {
            (* If I'm adding liquidity, x will be positive, I want to overestimate it, if x I'm taking away
                liquidity, I want to to underestimate what I'm receiving. *)
            x = ceildiv_int (liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - srp_l.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (srp_l.x80 * srp_u.x80)) ;
            y = 0})
    else if i_l.i <= s.cur_tick_index.i && s.cur_tick_index.i < i_u.i then
        (* update interval we are in, if need be ... *)
        let s = {s with cur_tick_witness = if i_l.i > s.cur_tick_witness.i then i_l else s.cur_tick_witness ; liquidity = assert_nat (s.liquidity + liquidity_delta, internal_liquidity_below_zero_err)} in
        (s, {
            x = ceildiv_int (liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - s.sqrt_price.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (s.sqrt_price.x80 * srp_u.x80)) ;
            y = shift_int (liquidity_delta * (s.sqrt_price.x80 - srp_l.x80)) (-80)
            })
    else (* cur_tick_index >= i_u *)
        (s, {x = 0 ; y = shift_int (liquidity_delta * (srp_u.x80 - srp_l.x80)) (-80) }) in

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

    ( [op_x ; op_y]
    , { s with
        positions = positions
      ; position_indexes = position_indexes
      ; new_position_id = new_position_id
      ; ticks = ticks
      }
    )

let get_time_weighted_sum (s : storage) (c : views contract) : result =
    ([Tezos.transaction (IC_sum s.time_weighted_ic_sum) 0mutez c], s)

let update_time_weighted_sum (s : storage) : storage =
    let new_sum = s.time_weighted_ic_sum + (Tezos.now - s.last_ic_sum_update) * s.cur_tick_index.i
    in {s with time_weighted_ic_sum = new_sum ; last_ic_sum_update = Tezos.now}

let main ((p, s) : parameter * storage) : result =
(* start by updating the time weighted price oracle *)
let s = update_time_weighted_sum s in
(* dispatch call to the proper entrypoint *)
 match p with
| X_to_Y p -> x_to_y s p
| Y_to_X p -> y_to_x s p
| Set_position p -> set_position s p.lower_tick_index p.upper_tick_index p.lower_tick_witness p.upper_tick_witness p.liquidity_delta p.to_x p.to_y
| Get_time_weighted_sum contract -> get_time_weighted_sum s contract
| X_to_X_prime _ -> (failwith "not implemented" : result) (*TODO implement iff Y is FA12 *)
| Call_FA2 p -> call_fa2 s p
