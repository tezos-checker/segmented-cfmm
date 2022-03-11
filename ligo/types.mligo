// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if TYPES_MLIGO
#else
#define TYPES_MLIGO

(* Keeps a positive value with -2^80 precision. *)
type x80n = { x80 : nat }

(* Keeps a value with -2^128 precision. *)
type x128 = { x128 : int }

(* Keeps a positive value with -2^128 precision. *)
type x128n = { x128 : nat }

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}

(* FA2 related types *)

type position_id = nat

type token_id = nat

type transfer_destination =
  [@layout:comb]
  { to_ : address
  ; token_id : position_id
  ; amount : nat
  }

type transfer_item =
  [@layout:comb]
  { from_ : address
  ; txs : transfer_destination list
  }

type transfer_params = transfer_item list

type balance_request_item =
  [@layout:comb]
  { owner : address
  ; token_id : position_id
  }

type balance_response_item =
  [@layout:comb]
  { request : balance_request_item
  ; balance : nat
  }

type balance_request_params =
  [@layout:comb]
  { requests : balance_request_item list
  ; callback : balance_response_item list contract
  }

type operator_param =
  [@layout:comb]
  { owner : address
  ; operator : address
  ; token_id : position_id
  }

type update_operator =
  [@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

type update_operators_param = update_operator list

type operators = (operator_param, unit) big_map


type fa2_parameter =
  [@layout:comb]
  | Balance_of of balance_request_params
  | Transfer of transfer_params
  | Update_operators of update_operators_param


type balance_nat = {x : nat ; y : nat}
type balance_nat_x128 = {x : x128n ; y : x128n}
type balance_int_x128 = {x : x128 ; y : x128}

(* Information stored for every initialized tick. *)
type tick_state = {
    (* Index of the previous initialized tick.
        Here we diverge from the article, and effectively store a doubly-linked
        list of initialized ticks for speed-up
        (while the article proposes storing a bitmap for this purpose).
    *)
    prev : tick_index ;

    (* Index of the next initialized tick. *)
    next : tick_index ;

    (* Total amount of liquidity to add to the contract's global liquidity when
        this tick is crossed going up.
        (i.e. when the current tick index `i_c` becomes greater than this tick),
        or subtracted when the tick is crossed going down.
    *)
    liquidity_net : int ;

    (* Numbers of positions with an edge at the given tick.
        Used for garbage collection.
    *)
    n_positions : nat ;

    (* When the current tick index `i_c` is below this tick, this field tracks
        the overall number of seconds `i_c` spent above or at this tick.
        When `i_c` is above or equal to this tick, it tracks the number of
        seconds `i_c` spent below this tick.

        This field is updated every time `i_c` crosses this tick.

        Here we assume that, during all the time since Unix epoch start till
        the moment of tick initialization, i_c was below this tick
        (see equation 6.25 of the uniswap v3 whitepaper).
        So we actually track the number of seconds with some additive error Δ,
        but this Δ remains contant during the lifetime of the tick. Ticks
        created at different moments of time will have different Δ though.

        As example, let's say the tick was initialized at 1628440000 timestamp;
        then `seconds_outside` can be initialized with the same timestamp.
        If i_c crossed this tick 5 seconds later, this `seconds_outside` will
        be set respectively to 5.
        If i_c crossed this tick back 3 seconds later, we will get
        `1628440000 + 3 = 1628440003`
        (effectively this will be computed as `cur_time - last seconds_outside =
        1628440008 - 5 = 1628440003`).

        This field helps to evaluate, for instance, how many seconds i_c
        has spent in an any given ticks range.
    *)
    seconds_outside : nat ;

    (* Tick indices accumulator i_o, it keeps track of time-weighted sum of
        tick indices, but accounts them only for "outside" periods.
        For the intuition for "outside" word, see `seconds_outside`.
    *)
    tick_cumulative_outside : int ;

    (* Overall number of fees f_o that were accumulated during the period
        when the current tick index i_c was below (or above) this tick.

        For intuition for "outside" word, see `seconds_outside`.
       *)
    fee_growth_outside : balance_nat_x128 ;

    (* Seconds-weighted 1/L value accumulator s_lo, it accounts only for
        "outside" periods. For intuition for "outside" word, see `seconds_outside`.

        This helps us to implement liquidity oracle.
    *)
    seconds_per_liquidity_outside : x128n ;

    (* sqrt(P) = sqrt(X/Y) associated with this tick. *)
    sqrt_price : x80n
}

// Cumulative values at given range.
//
// Note that values are relative, e.g. if we never was at given range this
// does not mean that `seconds_inside` will be 0, it can be an arbitrary value.
type cumulatives_inside_snapshot = {
    tick_cumulative_inside : int ;
    seconds_per_liquidity_inside : x128 ;
    seconds_inside : int ;
}

// Data for storing intermediate results of computations over cumulatives.
type cumulatives_data = {
    seconds_per_liquidity : x128 ;
    seconds : int ;
    tick : int ;
}

type tick_map = (tick_index, tick_state) big_map

type position_state = {
    (* Position edge tick indices *)
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index ;
    (* The position's owner.
        By default - position's creator, but ownership can be transferred later.
    *)
    owner : address ;
    (* Position's liquidity. *)
    liquidity : nat ;
    (* Total fees earned by the position at the moment of last fees collection for this position.
        This helps to evaluate the next portion of fees to collect.
    *)
    fee_growth_inside_last : balance_int_x128 ;
}


(* Map containing Liquidity providers. *)
type position_map = (position_id, position_state) big_map

// What we return when someone requests for the values of cumulatives.
type cumulatives_value =
    { tick_cumulative : int
    ; seconds_per_liquidity_cumulative : x128n
    }

// Tick index cumulative
type tick_cumulative = {
    (* The time-weighted cumulative value. *)
    sum : int;
    (* Tick index value at the beginning of the block. *)
    block_start_value : tick_index
}

// Seconds per liquidity cumulative
type spl_cumulative = {
    (* The time-weighted cumulative value. *)
    sum : x128n ;
    (* Liquidity value at the beginning of the block. *)
    block_start_liquidity_value : nat
}

type timed_cumulatives =
    { time : timestamp
    ; tick : tick_cumulative
    ; spl : spl_cumulative
    }

let init_timed_cumulatives : timed_cumulatives =
    { time = (0 : timestamp)  // Should not really matter
    ; tick = { sum = 0; block_start_value = {i = 0} }
    ; spl = { sum = {x128 = 0n}; block_start_liquidity_value = 0n }
    }

// Extendable ring buffer with time-weighted 1/L cumulative values.
type timed_cumulatives_buffer = {
    // For each index this stores:
    // 1. Cumulative values for every second in the history of the contract
    //    till specific moment of time, as well as last known value for
    //    the sake of future linear extrapolation.
    // 2. Timestamp when this sum was registered.
    //    This allows for bin search by timestamp.
    //
    // Indices in the map are assigned to values sequentially starting from 0.
    //
    // Invariants:
    // a. The set of indices that have an associated element with them is continuous;
    // b. Timestamps in values grow strictly monotonically
    //    (as well as accumulators ofc);
    map : (nat, timed_cumulatives) big_map ;

    // Index of the oldest stored value.
    first : nat ;

    // Index of the most recently stored value.
    last : nat ;

    // Number of actually allocated slots.
    //
    // This value is normally equal to `last - first + 1`.
    // However, in case recently there was a request to extend the set of
    // stored values, this var will keep the demanded number of stored values,
    // while values in the map past `last` will be initialized with garbage.
    //
    // We need to have initialized slots with trash because when the size of
    // the map increases, someone has to pay for the storage diff.
    // And we want it to be paid by the one who requested the extension.
    reserved_length : nat ;
}

let init_cumulatives_buffer (extra_reserved_slots : nat) : timed_cumulatives_buffer =
    // Fill [0..n] slots with init values
    let rec fill_map (n, map : nat * (nat, timed_cumulatives) big_map) : (nat, timed_cumulatives) big_map =
        let map = Big_map.add n init_timed_cumulatives map in
        match is_nat(n - 1) with
        | None -> map
        | Some n_dec -> fill_map(n_dec, map)
        in
    { map = fill_map(extra_reserved_slots, (Big_map.empty : (nat, timed_cumulatives) big_map))
    ; first = 0n
    ; last = 0n
    ; reserved_length = extra_reserved_slots + 1n
    }

// TZIP-16 metadata map
type metadata_map = (string, bytes) big_map

type constants = {
    fee_bps : nat ;
    ctez_burn_fee_bps : nat ;
    x_token_id : token_id ;
    y_token_id : token_id ;
    x_token_address : address ;
    y_token_address : address ;
    tick_spacing : nat ;
}


(* See defaults.mligo for more info *)
type fixed_point = { v : nat ; offset : int }
type ladder_key = { exp : nat ; positive : bool }
type ladder = (ladder_key, fixed_point) big_map


type storage = {
    (* Virtual liquidity, the value L for which the curve locally looks like x * y = L^2. *)
    liquidity : nat ;

    (* Square root of the virtual price, the value P for which P = x / y. *)
    sqrt_price : x80n ;

    (* Index of the highest tick corresponding to a price less than or equal to sqrt_price^2,
        does not necessarily corresponds to a boundary.
        Article's notation: i_c, tick.
    *)
    cur_tick_index : tick_index ;

    (* The highest initialized tick lower than or equal to i_c. *)
    cur_tick_witness : tick_index ;

    (* The total amount of fees that have been earned per unit of virtual liquidity (L),
        over the entire history of the contract.
    *)
    fee_growth : balance_nat_x128 ;

    (* States of all initialized ticks. *)
    ticks : tick_map ;

    (* States of positions (with non-zero liquidity). *)
    positions : position_map ;

    (* Cumulative values stored for the recent timestamps. *)
    cumulatives_buffer : timed_cumulatives_buffer ;

    (* TZIP-16 metadata. *)
    metadata : metadata_map ;

    (* Incremental position id to be assigned to new position. *)
    new_position_id : position_id ;

    (* FA2-related *)
    operators : operators ;

    (* Constants for options that are settable at origination *)
    constants : constants ;

    (* Exponents ladder for the calculation of 'half_bps_pow' *)
    ladder : ladder;
}

(* Entrypoints types *)
type set_position_param = {
    (* Lower tick. *)
    lower_tick_index : tick_index ;
    (* Upper tick. *)
    upper_tick_index : tick_index ;
    (* Lower tick's witness calculated offchain.

        A witness of tick T is some (preferably highest) _initialized_ tick
        with index lower than or equal to T. Finding a witness in our linked
        list with ticks is too expensive to be done on-chain.
    *)
    lower_tick_witness : tick_index ;
    (* Upper tick's witness calculated offchain. *)
    upper_tick_witness : tick_index ;
    (* The liquidity of the new position. *)
    liquidity : nat ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The maximum number of tokens to contribute.
        If a higher amount is required, the entrypoint fails.
    *)
    maximum_tokens_contributed : balance_nat;
}

type update_position_param = {
    (* Position id. *)
    position_id : position_id ;
    (* How to change the liquidity of the existing position.

        If adding a delta (that can be negative) would result in a negative liquidity value,
        the call will abort.
    *)
    liquidity_delta : int ;
    (* Where to send the freed X tokens, if any. *)
    to_x : address ;
    (* Where to send the freed Y tokens, if any. *)
    to_y : address ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The maximum number of tokens to contribute.
        If a higher amount is required, the entrypoint fails.
    *)
    maximum_tokens_contributed : balance_nat;
}

type x_to_y_param = {
    (* X tokens to sell. *)
    dx : nat ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The transaction won't be executed if buying less than the given amount of Y tokens. *)
    min_dy : nat ;
    (* Recipient of dy. *)
    to_dy : address ;
}

type x_to_y_rec_param = {s : storage ; dx : nat ; dy : nat}

type y_to_x_param = {
    (* Y tokens to sell. *)
    dy : nat ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The transaction won't be executed if buying less than the given amount of X tokens. *)
    min_dx : nat ;
    (* Recipient of dx. *)
    to_dx : address ;
}

type y_to_x_rec_param = x_to_y_rec_param

type snapshot_cumulatives_inside_param = {
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index ;
    callback : cumulatives_inside_snapshot contract ;
}

type oracle_view_param = cumulatives_value list

type observe_param =
[@layout:comb]
{
    times : timestamp list;
    callback : oracle_view_param contract
}

type increase_observation_count_param = {
    added_observation_count: nat;
}

type position_info = {
    liquidity : nat;
    owner : address;
    lower_tick_index : tick_index;
    upper_tick_index : tick_index;
}


type get_position_info_param =
[@layout:comb]
{
    position_id : position_id;
    callback : position_info contract;
}

type result = (operation list) * storage

type x_to_x_prime_param = {
    (* Amount of X tokens to sell. *)
    dx : nat ;
    (* Address of another segmented-cfmm contract. *)
    x_prime_contract : address ;
    (* The transaction won't be executed past this point. *)
    deadline : timestamp ;
    (* The transaction won't be executed if buying less than the given amount of X' tokens. *)
    min_dx_prime : nat ;
    (* Recipient of dx'. *)
    to_dx_prime : address ;
}

(* Entrypoints *)

type views =
  | IC_sum of int

type parameter =
  | X_to_y of x_to_y_param
  | Y_to_x of y_to_x_param
  | X_to_x_prime of x_to_x_prime_param (* equivalent to token_to_token *)
  | Set_position of set_position_param
  | Update_position of update_position_param
  | Get_position_info of get_position_info_param
  | Call_fa2 of fa2_parameter
  | Snapshot_cumulatives_inside of snapshot_cumulatives_inside_param
  | Observe of observe_param
  | Increase_observation_count of increase_observation_count_param

#endif
