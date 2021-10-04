// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "errors.mligo"
#include "consts.mligo"
#include "math.mligo"
#include "helpers.mligo"

(*
Note [Rounding the swap result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When calculating how many tokens to give the user for a swap, we can either round the
amount up (using `ceildiv`) or down (using `floordiv`) to the nearest integer.

Rounding it up would mean we'd sometimes give the user ~1 extra token.
Over time, this effect could compound and slowly drain the contract's balance faster than expected.
This, in turn, would mean some Liquidity Providers would not be able to liquidate their positions.

Furthermore, rounding it up would open the door to exploits.
Imagine a scenario where, at the current exchange rate, it costs 20 Y tokens to buy 1 X token.
A user could simply trade in 1 Y token, and receive 0.05 X tokens.
0.05 would be rounded up to 1, and the user would make an easy 1900% profit.

To prevent this, we must round it down.
*)

(* Calculates the new `cur_tick_index` after a given price change. *)
let calc_new_cur_tick_index (cur_tick_index : tick_index) (sqrt_price_old : x80n) (sqrt_price_new : x80n) (l : ladder): tick_index =
    let cur_tick_index_delta = floor_log_half_bps_x80(sqrt_price_new, sqrt_price_old, too_big_price_change_err) in
    let cur_tick_index_new = {i = cur_tick_index.i + cur_tick_index_delta } in

    (*
      When a very small change in `sqrt_price` occurs, `cur_tick_index_delta` might be
      small enough that it gets rounded down to 0.

      For example, assuming liquidity=1,000,000, and the contract is at index 0, the current `sqrt_price` should be:
        $ ligo repl cameligo
        > #use "ligo/main.mligo";;
          #use "ligo/defaults.mligo";;
          let cur_sqrt_price = half_bps_pow(0, default_ladder);;
          cur_sqrt_price;;
        < record[x80 -> +1208925819614629174706176]
      Depositing 10 Y tokens will move the price as follows:
        > let new_sqrt_price = sqrt_price_move_y 1000000n (half_bps_pow(0, default_ladder)) 10n;;
          new_sqrt_price;;
        < record[x80 -> +1208937908872825320997924]
      In this instance, `floor_log_half_bps_x80` would round the `cur_tick_index_delta` down to 0:
        > floor_log_half_bps_x80(new_sqrt_price, cur_sqrt_price, 0n);;
        < 0
      This means that placing many small swaps would slowly move the `sqrt_price` upwards,
      but the `cur_tick_index` would always be stuck without moving (due to all the deltas being rounded down to 0).

      To avoid `cur_tick_index` being stuck, we check whether `sqrt_price_new` has moved beyond
      the price for `cur_tick_index + 1`.
      If it has, then we need to bump up `cur_tick_index` by 1 to unstuck it.

      ---

      Similarly, in some edge cases, `floor_log_half_bps_x80` may end up rounding the `cur_tick_index_delta` up!
      Depositing 50 Y tokens will move the price as follows:
        > let new_sqrt_price = sqrt_price_move_y 1000000n (half_bps_pow(0, default_ladder)) 50n;;
      Which sits between the sqrt_price for ticks 0 and 1, so we would
      expect `cur_tick_index_delta` to be 0 and `cur_tick_index` to stay at 0.
        > half_bps_pow(0, default_ladder) <= new_sqrt_price && new_sqrt_price < half_bps_pow(1, default_ladder);;
        < true(unit)
      However, it is so close to the sqrt_price of tick 1 that `floor_log_half_bps_x80` ends up overshooting and rounds
      the `cur_tick_index_delta` up to 1.
        > floor_log_half_bps_x80(new_sqrt_price, cur_sqrt_price, 0n);;
        < 1

      In this case, we check whether `sqrt_price_new` is below the price for `cur_tick_index`.
      If it is, we decrement `cur_tick_index` by 1.
    *)
    let next_index_sqrt_price = half_bps_pow (cur_tick_index_new.i + 1, l) in
    let cur_index_sqrt_price = half_bps_pow (cur_tick_index_new.i, l) in
    let cur_tick_index_new =
            if next_index_sqrt_price.x80 <= sqrt_price_new.x80
                then {i = cur_tick_index_new.i + 1}
            else if sqrt_price_new.x80 < cur_index_sqrt_price.x80
                then {i = cur_tick_index_new.i - 1}
            else cur_tick_index_new in
    cur_tick_index_new

(* Helper function for x_to_y, recursively loops over ticks to execute a trade. *)
let rec x_to_y_rec (p : x_to_y_rec_param) : x_to_y_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dx. *)
        let fee  = ceildiv (p.dx * p.s.constants.fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move_x p.s.liquidity p.s.sqrt_price (assert_nat (p.dx - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of cur_tick_index will be. *)
        let cur_tick_index_new = calc_new_cur_tick_index p.s.cur_tick_index p.s.sqrt_price sqrt_price_new p.s.ladder in
        if cur_tick_index_new.i >= p.s.cur_tick_witness.i then
            (* The trade did not push us past the current tick. *)
            let dy = Bitwise.shift_right ((assert_nat (p.s.sqrt_price.x80 - sqrt_price_new.x80, internal_bad_sqrt_price_move_x_direction)) * p.s.liquidity) 80n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_index = cur_tick_index_new ;
                fee_growth = {p.s.fee_growth with x = {x128 = p.s.fee_growth.x.x128 + fee / p.s.liquidity}}} in
            {p with s = s_new ; dx = 0n ; dy = p.dy + dy}
        else
            (*We did cross the tick. *)
            (* The tick we are currently in. *)
            let tick = get_tick p.s.ticks p.s.cur_tick_witness internal_tick_not_exist_err in
            (* The tick index below that. *)
            let lo_new = tick.prev in
            (* The cached price corresponding to cur_tick_witness. *)
            let sqrt_price_new = tick.sqrt_price in
            (* How much dY will we receive for going all the way to cur_tick_witness. *)
            (* From 6.14 formula. *)
            let dy = Bitwise.shift_right (p.s.liquidity * (assert_nat (p.s.sqrt_price.x80 - sqrt_price_new.x80, internal_bad_sqrt_price_move_x_direction))) 80n in
            (* How much dX does that correspond to. *)
            let dx_for_dy = ceildiv (Bitwise.shift_left dy 160n) (p.s.sqrt_price.x80 * sqrt_price_new.x80) in
            (* We will have to consume more dx than that because a fee will be applied. *)
            let dx_consumed = ceildiv (dx_for_dy * 10000n) (one_minus_fee_bps(p.s.constants)) in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dx_consumed - dx_for_dy, internal_impossible_err) in
            let fee_growth_x_new = {x128 = p.s.fee_growth.x.x128 + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity)} in
            (* Flip tick cumulative growth. *)
            let sums = get_last_cumulatives p.s.cumulatives_buffer in
            let tick_cumulative_outside_new = sums.tick.sum - tick.tick_cumulative_outside in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with
                x = {x128 = assert_nat (fee_growth_x_new.x128 - tick.fee_growth_outside.x.x128, flip_fee_growth_outside_err)}} in
            let fee_growth_new = {p.s.fee_growth with x=fee_growth_x_new} in
            (* Flip time growth. *)
            let seconds_outside_new = assert_nat ((Tezos.now - epoch_time) - tick.seconds_outside, internal_negative_seconds_outside_err) in
            (* Update tick state. *)
            let tick_new = {tick with
                    tick_cumulative_outside = tick_cumulative_outside_new ;
                    fee_growth_outside = fee_growth_outside_new ;
                    seconds_outside = seconds_outside_new ;
                } in
            let ticks_new = Big_map.update p.s.cur_tick_witness (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_witness = lo_new ;
                cur_tick_index = p.s.cur_tick_witness ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity - tick.liquidity_net, internal_liquidity_below_zero_err)
                } in
            let p_new = {p with s = s_new ; dx = assert_nat (p.dx - dx_consumed, internal_307) ; dy = p.dy + dy} in
            x_to_y_rec p_new

let rec y_to_x_rec (p : y_to_x_rec_param) : y_to_x_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dy. *)
        let fee  = ceildiv (p.dy * p.s.constants.fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move_y p.s.liquidity p.s.sqrt_price (assert_nat (p.dy - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of cur_tick_index will be. *)
        let cur_tick_index_new = calc_new_cur_tick_index p.s.cur_tick_index p.s.sqrt_price sqrt_price_new p.s.ladder in
        let tick = get_tick p.s.ticks p.s.cur_tick_witness internal_tick_not_exist_err in
        let next_tick_index = tick.next in
        if cur_tick_index_new.i < next_tick_index.i then
            (* The trade did not push us past the current tick. *)
            (* From 6.16 formula: dx = L * (1 / old sqrt_price - 1 / new sqrt_price), where dx is how X decreases *)
            // Note [Rounding the swap result]
            let dx = floordiv (p.s.liquidity * Bitwise.shift_left (assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_bad_sqrt_price_move_y_direction)) 80n)
                             (sqrt_price_new.x80 * p.s.sqrt_price.x80) in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_index = cur_tick_index_new ;
                fee_growth = {p.s.fee_growth with y = {x128 = p.s.fee_growth.y.x128 + fee / p.s.liquidity}}} in
            {p with s = s_new ; dy = 0n ; dx = p.dx + dx}
        else
            (*We did cross the tick. *)
            (* The cached price corresponding to the next tick. *)
            let next_tick = get_tick p.s.ticks next_tick_index internal_tick_not_exist_err in
            let sqrt_price_new = next_tick.sqrt_price in

            (* How much dx will we receive for going all the way to cur_tick_witness. *)
            (* From 6.16 formula: dx = L * (1 / old sqrt_price - 1 / new sqrt_price), where dx is how X decreases *)
            // Note [Rounding the swap result]
            let dx = floordiv (p.s.liquidity * Bitwise.shift_left (assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_bad_sqrt_price_move_y_direction)) 80n)
                             (sqrt_price_new.x80 * p.s.sqrt_price.x80) in
            (* How much dy does that correspond to. *)
            let dy_for_dx = ceildiv (Bitwise.shift_left dx 160n) (p.s.sqrt_price.x80 * sqrt_price_new.x80) in
            (* plouf *)


            (* We will have to consume more dy than that because a fee will be applied. *)
            let dy_consumed = ceildiv (dy_for_dx * 10000n) (one_minus_fee_bps(p.s.constants)) in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dy_consumed - dy_for_dx, internal_impossible_err) in
            let fee_growth_y_new = {x128 = p.s.fee_growth.y.x128 + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity)} in
            (* Flip tick cumulative growth. *)
            let sums = get_last_cumulatives p.s.cumulatives_buffer in
            let tick_cumulative_outside_new = sums.tick.sum - tick.tick_cumulative_outside in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with
                y = {x128 = assert_nat (fee_growth_y_new.x128 - tick.fee_growth_outside.y.x128, flip_fee_growth_outside_err)}} in
            let fee_growth_new = {p.s.fee_growth with y=fee_growth_y_new} in
            (* Flip time growth. *)
            let seconds_outside_new = assert_nat ((Tezos.now - epoch_time) - tick.seconds_outside, internal_negative_seconds_outside_err) in
            (* Update tick state. *)
            let tick_new = { tick with
                    tick_cumulative_outside = tick_cumulative_outside_new ;
                    fee_growth_outside = fee_growth_outside_new ;
                    seconds_outside = seconds_outside_new ;
                } in
            let ticks_new = Big_map.update p.s.cur_tick_witness (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_witness = next_tick_index ;
                cur_tick_index = next_tick_index ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity + tick.liquidity_net, internal_liquidity_below_zero_err)
                } in
            let p_new = {p with s = s_new ; dy = assert_nat (p.dy - dy_consumed, internal_307) ; dx = p.dx + dx} in
            y_to_x_rec p_new

(* Get amount of X spent, Y received, and updated storage. *)
let update_storage_x_to_y (s : storage) (dx : nat) : (nat * nat * storage) =
    let r = x_to_y_rec {s = s ; dx = dx ; dy = 0n} in
    let dx_spent = assert_nat (dx - r.dx, internal_309) in
    let dy_received = r.dy in
    (dx_spent, dy_received, r.s)


(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (p : x_to_y_param) : result =
    let _: unit = check_deadline p.deadline in
    let (dx_spent, dy_received, s_new) = update_storage_x_to_y s p.dx in
    if dy_received < p.min_dy then
        (failwith smaller_than_min_asset_err : result)
    else
        let op_receive_x = x_transfer Tezos.sender Tezos.self_address dx_spent s.constants in
        let op_send_y = y_transfer Tezos.self_address p.to_dy dy_received s.constants in
        ([op_receive_x ; op_send_y], s_new)


(* Trade up to a quantity dy of asset y, receives dx *)
let y_to_x (s : storage) (p : y_to_x_param) : result =
    let _: unit = check_deadline p.deadline in
    let r = y_to_x_rec {s = s ; dy = p.dy ; dx = 0n} in
    let dy_spent = assert_nat (p.dy - r.dy, internal_309) in
    let dx_received = r.dx in
    if dx_received < p.min_dx then
        (failwith smaller_than_min_asset_err : result)
    else
        let op_receive_y = y_transfer Tezos.sender Tezos.self_address dy_spent s.constants in
        let op_send_x = x_transfer Tezos.self_address p.to_dx dx_received s.constants in
        ([op_receive_y ; op_send_x], r.s)


(* Trade X with X' in a different contract. *)
let x_to_x_prime (s : storage) (p : x_to_x_prime_param) : result =

    (* Ensure provided contract is a CFMM contract. *)
    let cfmm2_contract =
        match (Tezos.get_entrypoint_opt "%y_to_x" p.x_prime_contract
            : y_to_x_param contract option) with
        | Some contract -> contract
        | None -> (failwith invalid_x_prime_contract_err : y_to_x_param contract) in

    (* Swap X to Y *)
    let (dx_spent, dy_received, s_new) = update_storage_x_to_y s p.dx in

    (* Y to X' parameter *)
    let y_to_x_prime_param =
            { dy = dy_received
            ; deadline = p.deadline
            ; min_dx = p.min_dx_prime
            ; to_dx = p.to_dx_prime
            } in

    (* Transfer X token to the current CFMM contract. *)
    let op_transfer_x_to_cfmm1 = x_transfer Tezos.sender Tezos.self_address dx_spent s.constants in

    (* Make X' CFMM contract as an operator of current CFMM contract for Y. *)
    let op_make_cfmm2_operator = make_operator_in_y p.x_prime_contract dy_received s.constants in

    (* Make a call to X' CFMM contract to swap Y for X'. *)
    let op_call_y_to_x_prime = Tezos.transaction y_to_x_prime_param 0mutez cfmm2_contract in

    (* Remove X' CFMM contract as an operator of current CFMM contract. *)
    let op_remove_cfmm2_operator = remove_operator_in_y p.x_prime_contract s.constants in

    ( [ op_transfer_x_to_cfmm1
      ; op_make_cfmm2_operator
      ; op_call_y_to_x_prime
      ; op_remove_cfmm2_operator
      ]
    , s_new)
