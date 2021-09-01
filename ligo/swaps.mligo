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
        let sqrt_price_new = sqrt_price_move p.s.liquidity p.s.sqrt_price (assert_nat (p.dx - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of ic will be. *)
        let i_c_new = {i = p.s.cur_tick_index.i + floor_log_half_bps_x80(sqrt_price_new, p.s.sqrt_price)} in
        if i_c_new.i >= p.s.cur_tick_witness.i then
            (* The trade did not push us past the current tick. *)
            let dy = Bitwise.shift_right ((assert_nat (p.s.sqrt_price.x80 - sqrt_price_new.x80, internal_303)) * p.s.liquidity) 80n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_index = i_c_new ;
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
            let dy = Bitwise.shift_right (p.s.liquidity * (assert_nat (p.s.sqrt_price.x80 - sqrt_price_new.x80, internal_303))) 80n in
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
            let seconds_outside_new = assert_nat ((Tezos.now - epoch_time) - tick.seconds_outside, internal_epoch_bigger_than_now_err) in
            let tick_new = {tick with
                fee_growth_outside = fee_growth_outside_new ;
                seconds_outside = seconds_outside_new } in
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
            let p_new = {p with s = s_new ; dx = assert_nat (p.dx - dx_consummed, internal_306) ; dy = p.dy + dy} in
            x_to_y_rec p_new


let rec y_to_x_rec (p : y_to_x_rec_param) : y_to_x_rec_param =
 if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dy. *)
        let fee  = ceildiv (p.dy * const_fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move p.s.liquidity p.s.sqrt_price (assert_nat (p.dy - fee, internal_fee_more_than_100_percent_err)) in
        (* What the new value of ic will be. *)
        let i_c_new = {i = p.s.cur_tick_index.i + floor_log_half_bps_x80(sqrt_price_new, p.s.sqrt_price)} in
        let tick = get_tick p.s.ticks p.s.cur_tick_witness internal_tick_not_exist_err in
        let i_u = tick.next in
        if i_c_new.i < i_u.i then
            (* The trade did not push us past the current tick. *)
            let dx = Bitwise.shift_right ((assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_304)) * p.s.liquidity) 80n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_index = i_c_new ;
                fee_growth = {p.s.fee_growth with y = {x128 = p.s.fee_growth.y.x128 + fee / p.s.liquidity}}} in
            {p with s = s_new ; dy = 0n ; dx = p.dx + dx}
        else
            (*We did cross the tick. *)
            (* The cached price corresponding to hi. *)
            let next_tick = get_tick p.s.ticks i_u internal_tick_not_exist_err in
            let sqrt_price_new = next_tick.sqrt_price in

            (* How much dx will we receive for going all the way to cur_tick_witness. *)
            (* From 6.16 formula: dx = L * (1 / old sqrt_price - 1 / new sqrt_price), where dx is how X decreases *)
            let dx = ceildiv (p.s.liquidity * Bitwise.shift_left (assert_nat (sqrt_price_new.x80 - p.s.sqrt_price.x80, internal_304)) 80n)
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
            let tick_new = {tick with fee_growth_outside = fee_growth_outside_new} in
            let ticks_new = Big_map.update p.s.cur_tick_witness (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                cur_tick_witness = i_u ;
                cur_tick_index = i_u ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity - tick.liquidity_net, internal_liquidity_below_zero_err)
                } in
            let p_new = {p with s = s_new ; dy = assert_nat (p.dy - dy_consummed, internal_306) ; dx = p.dx + dx} in
            y_to_x_rec p_new


(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (p : x_to_y_param) : result =
    if Tezos.now > p.deadline then
        (failwith past_deadline_err : result)
    else
        let r = x_to_y_rec {s = s ; dx = p.dx ; dy = 0n} in
        let dx_spent = assert_nat (p.dx - r.dx, internal_309) in
        let dy_received = r.dy in
        let s_new = {s with balance = {x = s.balance.x + dx_spent ;  y = assert_nat (s.balance.y - dy_received, internal_insufficient_balance_err)}} in
        if dy_received < p.min_dy then
            (failwith smaller_than_min_asset_err : result)
        else
            let op_receive_x = x_transfer Tezos.sender Tezos.self_address dx_spent in
            let op_send_y = y_transfer Tezos.self_address p.to_dy dy_received in
            ([op_receive_x ; op_send_y], s_new)


(* Trade up to a quantity dy of asset y, receives dx *)
let y_to_x (s : storage) (p : y_to_x_param) : result =
    if Tezos.now > p.deadline then
        (failwith past_deadline_err : result)
    else
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
