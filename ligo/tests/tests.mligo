// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "../types.mligo"
#include "../consts.mligo"

let main_file = "../main.mligo"


let max_tick_state = {
    prev = {i=-const_max_tick} ;
    next = {i=int(const_max_tick)} ;
    liquidity_net = 0 ;
    n_positions = 1n ; (* prevents garbage collection *)
    fee_growth_outside = {x = 0n ; y = 0n} ;
    sqrt_price = 71107673757466966990985105047137336834554167630n ; (* Round[Exp[5/100000*(2^20-1)]*2^80] *)
} (* TODO consider using 2^90 precision instead so that every tick has a distinct sqrt_price *)

let min_tick_state = {
    prev = {i=-const_max_tick} ;
    next = {i=int(const_max_tick)} ;
    liquidity_net = 0 ;
    n_positions = 1n ; (* prevents garbage collection *)
    fee_growth_outside = {x = 0n ; y = 0n} ;
    sqrt_price = 21n ; (* Round[Exp[-5/100000*(2^20-1)]*2^80] *)
}

let ticks = Big_map.literal [
    ({ i = -const_max_tick}, min_tick_state);
    ({ i = int(const_max_tick)}, max_tick_state)
]

let test =
    let init_storage =  Test.compile_expression (Some main_file) [%cameligo ({|
    {
        liquidity = 0n ;
        sqrt_price = 0n ;
        i_c = 0 ;
        cur_tick_witness  = { i = -const_max_tick } ;
        fee_growth = { x = 0n ; y = 0n } ;
        balance = { x = 0n ; y = 0n } ;
        ticks = ticks ;
        positions = Big_map.empty ;
        time_weightd_ic_sum = 0 ;
        last_ic_sum_update = epoch_time
    }|} : ligo_program)] in
    let (addr, _, _) = Test.originate testme_test "main" init_storage in
    Test.log addr
