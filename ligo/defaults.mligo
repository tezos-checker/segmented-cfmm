// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"
#include "errors.mligo"
#include "math.mligo"

let default_storage (constants : constants) : storage =

  let min_tick_state =
    { prev = { i = -impossible_tick }
    ; next = { i = int(const_max_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; tick_cumulative_outside = 0
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
    ; tick_cumulative_outside = 0
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
  ; sqrt_price = half_bps_pow 0
  ; cur_tick_index = { i = 0 }
  ; cur_tick_witness  = { i = -const_max_tick }
  ; fee_growth = { x = { x128 = 0n }; y = { x128 = 0n } }
  ; ticks = ticks
  ; positions = (Big_map.empty : position_map)
  ; position_indexes = (Big_map.empty : position_index_map)
  ; cumulatives_buffer = init_cumulatives_buffer 0n
  ; metadata = (Big_map.empty : metadata_map)
  ; new_position_id = 0n
  ; operators = (Big_map.empty : operators)
  ; constants = constants
  }

(* Identity contract using 'parameter' and 'storage'.
 *
 * This is only used for the purpose of providing an `ENTRY_POINT` to the
 * `compile-storage` command of LIGO.
*)
let entrypoint (_param, store : parameter * storage) : result =
  (([] : operation list), store)
