// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "errors.mligo"
#include "consts.mligo"
#include "math.mligo"
#include "helpers.mligo"


(* Helper function for x_to_y, recursively loops over ticks to execute a trade. *)
let rec x_to_y_rec (p : x_to_y_rec_param) : x_to_y_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dx. *)
        let fee  = ceildiv (p.dx * const_fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move_x p.s.liquidity p.s.sqrt_price (assert_nat (p.dx - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of cur_tick_index will be. *)
        let cur_tick_index_new = {i = p.s.cur_tick_index.i + floor_log_half_bps_x80(sqrt_price_new, p.s.sqrt_price, too_big_price_change_err)} in
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
            (* We will have to consumme more dx than that because a fee will be applied. *)
            let dx_consummed = ceildiv (dx_for_dy * 10000n) const_one_minus_fee_bps in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dx_consummed - dx_for_dy, internal_impossible_err) in
            let fee_growth_x_new = {x128 = p.s.fee_growth.x.x128 + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity)} in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with
                x = {x128 = assert_nat (fee_growth_x_new.x128 - tick.fee_growth_outside.x.x128, flip_fee_growth_outside_err)}} in
            let fee_growth_new = {p.s.fee_growth with x=fee_growth_x_new} in
            (* Flip time growth. *)
            let seconds_outside_new = assert_nat ((Tezos.now - epoch_time) - tick.seconds_outside, internal_negative_seconds_outside_err) in
            (* Update tick state. *)
            let tick_new = {tick with
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
            let p_new = {p with s = s_new ; dx = assert_nat (p.dx - dx_consummed, internal_307) ; dy = p.dy + dy} in
            x_to_y_rec p_new

let rec y_to_x_rec (p : y_to_x_rec_param) : y_to_x_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dy. *)
        let fee  = ceildiv (p.dy * const_fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move_y p.s.liquidity p.s.sqrt_price (assert_nat (p.dy - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of cur_tick_index will be. *)
        let cur_tick_index_new = {i = p.s.cur_tick_index.i + floor_log_half_bps_x80(sqrt_price_new, p.s.sqrt_price, too_big_price_change_err)} in
        let tick = get_tick p.s.ticks p.s.cur_tick_witness internal_tick_not_exist_err in
        let i_u = tick.next in
        if cur_tick_index_new.i < i_u.i then
            (* The trade did not push us past the current tick. *)
            let dx = Bitwise.shift_right ((assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_bad_sqrt_price_move_y_direction)) * p.s.liquidity) 80n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_index = cur_tick_index_new ;
                fee_growth = {p.s.fee_growth with y = {x128 = p.s.fee_growth.y.x128 + fee / p.s.liquidity}}} in
            {p with s = s_new ; dy = 0n ; dx = p.dx + dx}
        else
            (*We did cross the tick. *)
            (* The cached price corresponding to hi. *)
            let next_tick = get_tick p.s.ticks i_u internal_tick_not_exist_err in
            let sqrt_price_new = next_tick.sqrt_price in

            (* How much dx will we receive for going all the way to cur_tick_witness. *)
            (* From 6.16 formula: dx = L * (1 / old sqrt_price - 1 / new sqrt_price), where dx is how X decreases *)
            let dx = ceildiv (p.s.liquidity * Bitwise.shift_left (assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_bad_sqrt_price_move_y_direction)) 80n)
                             (sqrt_price_new.x80 * p.s.sqrt_price.x80) in
            (* How much dy does that correspond to. *)
            let dy_for_dx = ceildiv (Bitwise.shift_left dx 160n) (p.s.sqrt_price.x80 * sqrt_price_new.x80) in
            (* plouf *)


            (* We will have to consumme more dy than that because a fee will be applied. *)
            let dy_consummed = ceildiv (dy_for_dx * 10000n) const_one_minus_fee_bps in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dy_consummed - dy_for_dx, internal_impossible_err) in
            let fee_growth_y_new = {x128 = p.s.fee_growth.y.x128 + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity)} in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with
                y = {x128 = assert_nat (fee_growth_y_new.x128 - tick.fee_growth_outside.y.x128, flip_fee_growth_outside_err)}} in
            let fee_growth_new = {p.s.fee_growth with y=fee_growth_y_new} in
            (* Flip time growth. *)
            let seconds_outside_new = assert_nat ((Tezos.now - epoch_time) - tick.seconds_outside, internal_negative_seconds_outside_err) in
            (* Update tick state. *)
            let tick_new = { tick with
                    fee_growth_outside = fee_growth_outside_new ;
                    seconds_outside = seconds_outside_new ;
                } in
            let ticks_new = Big_map.update p.s.cur_tick_witness (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_witness = i_u ;
                cur_tick_index = i_u ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity + tick.liquidity_net, internal_liquidity_below_zero_err)
                } in
            let p_new = {p with s = s_new ; dy = assert_nat (p.dy - dy_consummed, internal_307) ; dx = p.dx + dx} in
            y_to_x_rec p_new

(* Get amount of X spent, Y received, and updated storage. *)
let update_storage_x_to_y (s : storage) (dx : nat) : (nat * nat * storage) =
    let r = x_to_y_rec {s = s ; dx = dx ; dy = 0n} in
    let dx_spent = assert_nat (dx - r.dx, internal_309) in
    let dy_received = r.dy in
    let s_new = {s with balance = {x = s.balance.x + dx_spent ;  y = assert_nat (s.balance.y - dy_received, internal_insufficient_balance_err)}} in
    (dx_spent, dy_received, s_new)


(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (p : x_to_y_param) : result =
    let _: unit = check_deadline p.deadline in
    let (dx_spent, dy_received, s_new) = update_storage_x_to_y s p.dx in
    if dy_received < p.min_dy then
        (failwith smaller_than_min_asset_err : result)
    else
        let op_receive_x = x_transfer Tezos.sender Tezos.self_address dx_spent in
        let op_send_y = y_transfer Tezos.self_address p.to_dy dy_received in
        ([op_receive_x ; op_send_y], s_new)


(* Trade up to a quantity dy of asset y, receives dx *)
let y_to_x (s : storage) (p : y_to_x_param) : result =
    let _: unit = check_deadline p.deadline in
    let r = y_to_x_rec {s = s ; dy = p.dy ; dx = 0n} in
    let dy_spent = assert_nat (p.dy - r.dy, internal_309) in
    let dx_received = r.dx in
    let s_new = {s with balance = {y = s.balance.y + dy_spent ;  x = assert_nat (s.balance.x - dx_received, internal_insufficient_balance_err)}} in
    if dx_received < p.min_dx then
        (failwith smaller_than_min_asset_err : result)
    else
        let op_receive_y = y_transfer Tezos.sender Tezos.self_address dy_spent in
        let op_send_x = x_transfer Tezos.self_address p.to_dx dx_received in
        ([op_receive_y ; op_send_x], s_new)


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
    let op_transfer_x_to_cfmm1 = x_transfer Tezos.sender Tezos.self_address dx_spent in

    (* Make X' CFMM contract as an operator of current CFMM contract for Y. *)
    let op_make_cfmm2_operator = make_operator_in_y p.x_prime_contract dy_received in

    (* Make a call to X' CFMM contract to swap Y for X'. *)
    let op_call_y_to_x_prime = Tezos.transaction y_to_x_prime_param 0mutez cfmm2_contract in

    (* Remove X' CFMM contract as an operator of current CFMM contract. *)
    let op_remove_cfmm2_operator = remove_operator_in_y p.x_prime_contract in

    ( [ op_transfer_x_to_cfmm1
      ; op_make_cfmm2_operator
      ; op_call_y_to_x_prime
      ; op_remove_cfmm2_operator
      ]
    , s_new)
