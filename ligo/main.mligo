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

[@inline]
let get_registered_cumulatives_unsafe (buffer : timed_cumulatives_buffer) (i : nat) : timed_cumulatives =
    match Big_map.find_opt i buffer.map with
        | None -> failwith internal_bad_access_to_observation_buffer
        | Some v -> v

[@inline]
let get_last_cumulatives (buffer : timed_cumulatives_buffer) : timed_cumulatives =
    get_registered_cumulatives_unsafe buffer buffer.last

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

(* Account for the fact that this tick is a boundary for one more (or one less) position. *)
let cover_tick_with_position (ticks : tick_map) (tick_index : tick_index) (pos_delta : int) (liquidity_delta : int) =
    let tick = get_tick ticks tick_index internal_tick_not_exist_err in
    let n_pos = assert_nat (tick.n_positions + pos_delta, internal_position_underflow_err) in
    let new_liquidity = tick.liquidity_net + liquidity_delta in
    if n_pos = 0n then
        (*  Garbage collect the tick.
            The largest and smallest tick are initialized with n_positions = 1 so they cannot
            be accidentally garbage collected. *)
#if DEBUG
        let _ : unit = if new_liquidity <> 0 then
            failwith internal_non_empty_position_gc_err
            else unit in
#endif
        let prev = get_tick ticks tick.prev internal_tick_not_exist_err in
        let next = get_tick ticks tick.next internal_tick_not_exist_err in
        (* prev links to next and next to prev, skipping the deleted tick *)
        let prev = {prev with next = tick.next} in
        let next = {next with prev = tick.prev} in
        let ticks = Big_map.update tick_index (None : tick_state option) ticks in
        let ticks = Big_map.update tick.prev (Some prev) ticks in
        let ticks = Big_map.update tick.next (Some next) ticks in
        ticks
    else
        Big_map.add tick_index
            { tick with
                n_positions = n_pos;
                liquidity_net = new_liquidity
            } ticks

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

let set_position (s : storage) (p : set_position_param) : result =
    let _: unit = check_deadline p.deadline in
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = if s.cur_tick_index.i >= p.lower_tick_index.i then
        let sums = get_last_cumulatives s.cumulatives_buffer in
        initialize_tick
            ( ticks, p.lower_tick_index, p.lower_tick_witness, s.fee_growth
            , assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err)
            , sums.lps.sum
            )
    else
        initialize_tick (ticks, p.lower_tick_index, p.lower_tick_witness, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in
    let ticks = if s.cur_tick_index.i >= p.upper_tick_index.i then
        let sums = get_last_cumulatives s.cumulatives_buffer in
        initialize_tick
            ( ticks, p.upper_tick_index, p.upper_tick_witness, s.fee_growth
            , assert_nat (Tezos.now - epoch_time, internal_epoch_bigger_than_now_err)
            , sums.lps.sum
            )
    else
        initialize_tick (ticks, p.upper_tick_index, p.upper_tick_witness, {x = {x128 = 0n} ; y = {x128 = 0n}}, 0n, {x128 = 0n})  in

    (* Form position key. *)
    let position_key = {owner=Tezos.sender ; lower_tick_index=p.lower_tick_index; upper_tick_index=p.upper_tick_index} in
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
    let liquidity_new = assert_nat (position.liquidity + p.liquidity_delta, internal_liquidity_below_zero_err) in
    let position = {position with liquidity = liquidity_new} in
    (* How number of positions at related ticks changes. *)
    let positions_num_delta =
            if is_new && liquidity_new > 0n then 1
            else if not is_new && liquidity_new = 0n then -1
            else 0
        in
    (* Update related ticks. *)
    let ticks = cover_tick_with_position ticks p.lower_tick_index positions_num_delta p.liquidity_delta in
    let ticks = cover_tick_with_position ticks p.upper_tick_index positions_num_delta (-p.liquidity_delta) in
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
    let tick_u = get_tick ticks p.upper_tick_index internal_tick_not_exist_err in
    let tick_l = get_tick ticks p.lower_tick_index internal_tick_not_exist_err in
    let srp_u = tick_u.sqrt_price in
    let srp_l = tick_l.sqrt_price in

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.cur_tick_index.i < p.lower_tick_index.i then
        (s, {
            (* If I'm adding liquidity, x will be positive, I want to overestimate it, if x I'm taking away
                liquidity, I want to to underestimate what I'm receiving. *)
            x = ceildiv_int (p.liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - srp_l.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (srp_l.x80 * srp_u.x80)) ;
            y = 0})
    else if p.lower_tick_index.i <= s.cur_tick_index.i && s.cur_tick_index.i < p.upper_tick_index.i then
        (* update interval we are in, if need be ... *)
        let s = { s with
                    cur_tick_witness =
                        if p.lower_tick_index.i > s.cur_tick_witness.i
                            then p.lower_tick_index
                            else s.cur_tick_witness ;
                    liquidity = assert_nat (s.liquidity + p.liquidity_delta, internal_liquidity_below_zero_err)
                } in
        (s, {
            x = ceildiv_int (p.liquidity_delta * (int (Bitwise.shift_left (assert_nat (srp_u.x80 - s.sqrt_price.x80, internal_sqrt_price_grow_err_1)) 80n))) (int (s.sqrt_price.x80 * srp_u.x80)) ;
            y = shift_int (p.liquidity_delta * (s.sqrt_price.x80 - srp_l.x80)) (-80)
            })
    else (* cur_tick_index >= p.upper_tick_index *)
        (s, {x = 0 ; y = shift_int (p.liquidity_delta * (srp_u.x80 - srp_l.x80)) (-80) }) in

    (* Collect fees to increase withdrawal or reduce required deposit. *)
    let delta = {x = delta.x - fees.x ; y = delta.y - fees.y} in

    (* Check delta doesn't exceed maximum_tokens_contributed. *)
    let _: unit = if delta.x > int(p.maximum_tokens_contributed.x) then unit else failwith high_tokens_err in
    let _: unit = if delta.y > int(p.maximum_tokens_contributed.y) then unit else failwith high_tokens_err in

    let op_x = if delta.x > 0 then
        x_transfer Tezos.sender Tezos.self_address (abs delta.x)
    else
        x_transfer Tezos.self_address p.to_x (abs delta.x) in

    let op_y = if delta.y > 0 then
        y_transfer Tezos.sender Tezos.self_address (abs delta.y)
    else
        y_transfer Tezos.self_address p.to_y (abs delta.y) in

    ( [op_x ; op_y]
    , { s with
        positions = positions
      ; position_indexes = position_indexes
      ; new_position_id = new_position_id
      ; ticks = ticks
      }
    )

// Increase the number of stored accumulators.
let increase_observation_count (s, p : storage * increase_observation_count_param) : result =
    let buffer = s.cumulatives_buffer in
#if !DEBUG
    let dummy_timed_cumulatives = get_last_cumulatives buffer in
#else
    // This helps to faster detect access to garbage values
    let dummy_timed_cumulatives = init_timed_cumulatives in
#endif
    let new_reserved_length = buffer.reserved_length + p.added_observation_count in

    let stop_allocation_index = buffer.first + new_reserved_length in
    let rec allocate_buffer_slots (buffer_map, idx : (nat, timed_cumulatives) big_map * nat) : (nat, timed_cumulatives) big_map =
        if idx >= stop_allocation_index
        then buffer_map
        else
            let new_buffer_map = Big_map.add idx dummy_timed_cumulatives buffer_map
            in allocate_buffer_slots(new_buffer_map, idx + 1n)
        in

    let buffer_map = allocate_buffer_slots(buffer.map, buffer.last + 1n) in
    let buffer = {buffer with reserved_length = new_reserved_length; map = buffer_map}
    in (([] : operation list), {s with cumulatives_buffer = buffer})

// Recursive helper for `get_cumulatives`
let rec find_cumulatives_around (buffer, t, l, r : timed_cumulatives_buffer * timestamp * (nat * timed_cumulatives) * (nat * timed_cumulatives)) : (timed_cumulatives * timed_cumulatives * nat) =
    let (l_i, l_v) = l in
    let (r_i, r_v) = r in
    // Binary search, invariant: l_v.time <= t && t < r_v.time
    if l_i + 1n < r_i
    then
        let m_i = (l_i + r_i) / 2n in
        let m_v = get_registered_cumulatives_unsafe buffer m_i in
        let m = (m_i, m_v) in
        if m_v.time > t
        then find_cumulatives_around (buffer, t, l, m)
        else find_cumulatives_around (buffer, t, m, r)
    else
        (l_v, r_v, assert_nat (t - l_v.time, internal_observe_bin_search_failed))

let get_cumulatives (buffer : timed_cumulatives_buffer) (t : timestamp) : cumulatives_value =
    let l_i = buffer.first in
    let r_i = buffer.last in
    let l_v = get_registered_cumulatives_unsafe buffer l_i in
    let r_v = get_registered_cumulatives_unsafe buffer r_i in

    let _: unit = if t < l_v.time then failwith observe_outdated_timestamp_err else unit in
    let _: unit = if t > r_v.time then failwith observe_future_timestamp_err else unit in

    if t < r_v.time then
        let (sums_at_left, sums_at_right, time_delta) = find_cumulatives_around (buffer, t, (l_i, l_v), (r_i, r_v))

        // When no updates to contract are performed, time-weighted accumulators grow
        // linearly. Extrapolating to get the value at timestamp in-between.
        //
        // tick_cumulative(t) and liqudity_per_second_cumulative(t) functions produced
        // by this extrapolation are continuous.
        // 1. At [left, right) range found by the binary search above, cumulatives are
        //    continuous by construction - our extrapolation is linear.
        // 2. At (right - o, right] range they are also continous, because we will
        //    use the same formula for calculating cumulatives at `right - o` (here)
        //    and at `right` (see how `sum` fields are updated in `update_timed_cumulatives`).
        in  { tick_cumulative =
                let at_left_block_end_tick_value = sums_at_right.tick.block_start_value
                in sums_at_left.tick.sum + time_delta * at_left_block_end_tick_value.i
            ; seconds_per_liquidity_cumulative =
                let at_left_block_end_lps_value = sums_at_right.lps.block_start_liquidity_value
                in {x128 = sums_at_left.lps.sum.x128 +
                    Bitwise.shift_left time_delta 128n / at_left_block_end_lps_value }
            }
    else // t = r_v.time
        // This means that t = timestamp of the last recorded entry,
        // and we cannot use extrapolation as above
        { tick_cumulative = r_v.tick.sum
        ; seconds_per_liquidity_cumulative = r_v.lps.sum
        }

let observe (s : storage) (p : observe_param) : result =
    let value = List.map (get_cumulatives s.cumulatives_buffer) p.times
    in ([Tezos.transaction value 0mutez p.callback], s)

// Update the cumulative values stored for the recent timestamps.
//
// This has to be called on every update to the contract, not necessarily
// for each block. Currently all cumulatives keep time-weighted sum of something,
// so we can extrapolate these cumulatives on periods of the contract's inactivity.
let update_timed_cumulatives (s : storage) : storage =
    let buffer = s.cumulatives_buffer in

    let last_value = get_last_cumulatives buffer in
    (* Update not more often than once per block *)
    if last_value.time = Tezos.now then s
    else
        let time_passed = abs (Tezos.now - last_value.time) in
        let new_value =
            { tick =
                { block_start_value = s.cur_tick_index
                ; sum = last_value.tick.sum + time_passed * s.cur_tick_index.i
                }
            ; lps =
                { block_start_liquidity_value = s.liquidity
                ; sum = {x128 = last_value.lps.sum.x128 + Bitwise.shift_left time_passed 128n / s.liquidity};
                }
            ; time = Tezos.now
            } in

        let new_last = buffer.last + 1n in
        let new_first =
            // preserve the oldest element if reserves allow this
            if buffer.last - buffer.first < buffer.reserved_length - 1
            then buffer.first else buffer.first + 1n in

        let new_buffer = {
            map = Big_map.add new_last new_value buffer.map ;
            last = new_last ;
            first = new_first ;
            reserved_length = buffer.reserved_length ;
        }
        in {s with cumulatives_buffer = new_buffer}

let main ((p, s) : parameter * storage) : result =
(* start by updating the oracles *)
let s = update_timed_cumulatives s in
(* dispatch call to the proper entrypoint *)
 match p with
| X_to_y p -> x_to_y s p
| Y_to_x p -> y_to_x s p
| Set_position p -> set_position s p
| X_to_x_prime p -> x_to_x_prime s p
| Call_fa2 p -> call_fa2 s p
| Observe p -> observe s p
| Increase_observation_count n -> increase_observation_count(s, n)
