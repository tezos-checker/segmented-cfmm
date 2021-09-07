// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"

let default_storage : storage =

  let min_tick_state =
    { prev = { i = -impossible_tick }
    ; next = { i = int(const_max_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; fee_growth_outside = {x = { x128 = 0n } ; y = { x128 = 0n }}
    ; seconds_per_liquidity_outside = {x128 = 0n}
    ; sqrt_price = { x80 = 21n }
    (* ^ round(sqrt(exp(0.0001)^-1048575) * 2^80) *)
    } in

  let max_tick_state =
    { prev = { i = -const_max_tick }
    ; next = { i = int(impossible_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; fee_growth_outside = {x = { x128 = 0n } ; y = { x128 = 0n }}
    ; seconds_per_liquidity_outside = {x128 = 0n}
    ; sqrt_price = { x80 = 71107673757466966990985105047137336834554167630n }
    (* ^ round(sqrt(exp(0.0001)^1048575) * 2^80) *)
    } in

  let ticks = Big_map.literal [
      ({ i = -const_max_tick }, min_tick_state);
      ({ i = int(const_max_tick) }, max_tick_state)
  ] in

  { liquidity = 0n
  ; sqrt_price = { x80 = 0n }
  ; cur_tick_index = { i = 0 }
  ; cur_tick_witness  = { i = -const_max_tick }
  ; fee_growth = { x = { x128 = 0n }; y = { x128 = 0n } }
  ; balance = { x = 0n ; y = 0n }
  ; ticks = ticks
  ; positions = (Big_map.empty : position_map)
  ; position_indexes = (Big_map.empty : position_index_map)
  ; time_weighted_ic_sum = 0
  ; last_ic_sum_update = epoch_time
  ; seconds_per_liquidity_cumulative = { x128 = 0n }
  ; new_position_id = 0n
  ; metadata = (Big_map.empty : metadata_map)
  ; operators = (Big_map.empty : operators)
  }
