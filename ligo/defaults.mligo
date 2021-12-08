// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "types.mligo"
#include "consts.mligo"
#include "errors.mligo"
#include "math.mligo"

(* ladder explanation

relative error for each ladder element is  2^(-86)
In the worst case, the product of all the mantissas is

264282414466372656233620232085\
891076152819178199188386987876\
130399611230776317562564102356\
510887554505841649869995115306\
153657257592287884078198212867\
452334369951588805003086042064\
419487861191053453237580032868\
756342299942389805587432570501\
862667904215683136301357731738\
924376078014888356296733854672\
700413177506511695535173325976\
383558174492550937991343710044\
641323722927702345262447930316\
774974009739628156118404725209\
505333623465138071036374956137\
115703347618301958744836243752\
685553646224208937741458987598\
9769554995549619185305600000

A 1786 bit number.

Also in the worse case, the product of all the error terms is (1 + total_err) with |total_err| < 2^(-81)
This ensures that the tick index can be matched to a square root price with over 80 bits of precision

*)
let default_ladder : ladder = Big_map.literal
  [ ({exp=0n; positive=true}, {v=38687560557337355742483221n; offset=-85}) (* 2^0 *)
  ; ({exp=1n; positive=true}, {v=38689494983725479307861971n; offset=-85})
  ; ({exp=2n; positive=true}, {v=38693364126677775184793561n; offset=-85})
  ; ({exp=3n; positive=true}, {v=38701103573421987005215721n; offset=-85})
  ; ({exp=4n; positive=true}, {v=38716587111352494729706462n; offset=-85})
  ; ({exp=5n; positive=true}, {v=38747572773653928660613512n; offset=-85})
  ; ({exp=6n; positive=true}, {v=38809618513447185627569983n; offset=-85})
  ; ({exp=7n; positive=true}, {v=38934008210058939100663682n; offset=-85})
  ; ({exp=8n; positive=true}, {v=39183984934869404935943141n; offset=-85})
  ; ({exp=9n; positive=true}, {v=39688763633815974521145659n; offset=-85})
  ; ({exp=10n; positive=true}, {v=40717912888646086984030507n; offset=-85})
  ; ({exp=11n; positive=true}, {v=42856962434838368098529959n; offset=-85})
  ; ({exp=12n; positive=true}, {v=47478079282778087338933597n; offset=-85})
  ; ({exp=13n; positive=true}, {v=29134438707490415855866100n; offset=-84})
  ; ({exp=14n; positive=true}, {v=43882733799120415566608322n; offset=-84})
  ; ({exp=15n; positive=true}, {v=49778031622173924435819796n; offset=-83})
  ; ({exp=16n; positive=true}, {v=32025492072892644517427309n; offset=-80})
  ; ({exp=17n; positive=true}, {v=53023938993515524338629870n; offset=-76})
  ; ({exp=18n; positive=true}, {v=36338278329035183585718600n; offset=-66})
  ; ({exp=19n; positive=true}, {v=34133361681864713959105863n; offset=-47})
  (* ; ({exp=20n; positive=true}, {v=30116777038798852995368017; offset=-9})  2^20  *)
  ; ({exp=0n; positive=false}, {v=19341845997356488514015570n; offset=-84}) (* -2^0 *)
  ; ({exp=1n; positive=false}, {v=2417609866154190654524678n; offset=-81})
  ; ({exp=2n; positive=false}, {v=38677889876083546261210550n; offset=-85})
  ; ({exp=3n; positive=false}, {v=38670155071614559132217310n; offset=-85})
  ; ({exp=4n; positive=false}, {v=19327345051392939314248854n; offset=-84})
  ; ({exp=5n; positive=false}, {v=19311889358453304431405214n; offset=-84})
  ; ({exp=6n; positive=false}, {v=77124060166079386301517011n; offset=-86})
  ; ({exp=7n; positive=false}, {v=38438828813936263312862610n; offset=-85})
  ; ({exp=8n; positive=false}, {v=76387211720013513967242610n; offset=-86})
  ; ({exp=9n; positive=false}, {v=75415686436335201065707301n; offset=-86})
  ; ({exp=10n; positive=false}, {v=73509547540888574991368714n; offset=-86})
  ; ({exp=11n; positive=false}, {v=17460146398643019245576278n; offset=-84})
  ; ({exp=12n; positive=false}, {v=126085780994910985395717054n; offset=-87})
  ; ({exp=13n; positive=false}, {v=102735988268212419722671870n; offset=-87})
  ; ({exp=14n; positive=false}, {v=68208042073114503830679361n; offset=-87})
  ; ({exp=15n; positive=false}, {v=60130046442422405275353178n; offset=-88})
  ; ({exp=16n; positive=false}, {v=11682706336100247487260846n; offset=-88})
  ; ({exp=17n; positive=false}, {v=56449132412055094618915006n; offset=-95})
  ; ({exp=18n; positive=false}, {v=20592303012757789234393034n; offset=-103})
  ; ({exp=19n; positive=false}, {v=1370156647050591448120178n; offset=-118})
  (* ; ({exp=20n; positive=true}, {v=24846245577653162038756966;offset=-160}) -2^20  *)
  ]


let default_storage
    (constants : constants)
    (init_cumulatives_buffer_extra_slots : nat)
    (metadata_map : metadata_map) : storage =
  let min_tick_state =
    { prev = { i = -impossible_tick }
    ; next = { i = int(const_max_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; tick_cumulative_outside = 0
    ; fee_growth_outside = {x = { x128 = 0n } ; y = { x128 = 0n }}
    ; seconds_per_liquidity_outside = {x128 = 0n}
    ; sqrt_price = half_bps_pow (-const_max_tick, default_ladder)
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
    ; sqrt_price = half_bps_pow (int const_max_tick, default_ladder)
    } in

  let ticks = Big_map.literal [
      ({ i = -const_max_tick }, min_tick_state);
      ({ i = int(const_max_tick) }, max_tick_state)
  ] in

  { liquidity = 0n
  ; sqrt_price = half_bps_pow (0, default_ladder)
  ; cur_tick_index = { i = 0 }
  ; cur_tick_witness  = { i = -const_max_tick }
  ; fee_growth = { x = { x128 = 0n }; y = { x128 = 0n } }
  ; ticks = ticks
  ; positions = (Big_map.empty : position_map)
  ; cumulatives_buffer = init_cumulatives_buffer init_cumulatives_buffer_extra_slots
  ; metadata = metadata_map
  ; new_position_id = 0n
  ; operators = (Big_map.empty : operators)
  ; constants = constants
  ; ladder = default_ladder
  }

(* Identity contract using 'parameter' and 'storage'.
 *
 * This is only used for the purpose of providing an `--entry-point` to the
 * `compile storage` command of LIGO.
*)
let entrypoint (_param, store : parameter * storage) : result =
  (([] : operation list), store)
