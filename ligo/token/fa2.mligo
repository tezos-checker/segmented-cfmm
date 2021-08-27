// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#include "../types.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let get_position_index (position_id, store : position_id * storage) : position_index =
  match (Big_map.find_opt position_id store.position_indexes) with
    | Some position_index -> position_index
    | None -> ([%Michelson ({| { FAILWITH } |} : string * unit -> position_index)]
          ("FA2_TOKEN_UNDEFINED", ()) : position_index)

[@inline]
let check_sender (from_ , store : address * storage): address =
  if (Tezos.sender = from_) then from_
  else
    let key: operator = { owner = from_; operator = Tezos.sender} in
    if Big_map.mem key store.operators then
      from_
    else
     ([%Michelson ({| { FAILWITH } |} : string * unit -> address)]
        ("FA2_NOT_OPERATOR", ()) : address)

(* A function that combines the usual FA2's `debit_from` and `credit_to`. *)
[@inline]
let change_position_owner (from_, tx, pos_index, store: address * transfer_destination * position_index * storage): position_index_map =
  if tx.amount = 0n then
    store.position_indexes // We allow 0 transfer
  else
    // Ensure `from_` is the owner of the position.
    let old_pos_index =
        if (pos_index.owner = from_ && tx.amount = 1n) then pos_index
        else
          ([%Michelson ({| { FAILWITH } |} : string * (nat * nat) -> position_index)]
            ("FA2_INSUFFICIENT_BALANCE", (tx.amount, 0n)) : position_index) in

    // Update `store.position_indexes`
    let position_indexes = Big_map.update tx.token_id (Some { old_pos_index with owner = tx.to_ }) store.position_indexes
    in position_indexes


// -----------------------------------------------------------------
// Transfer
// -----------------------------------------------------------------

let transfer_item (store, ti : storage * transfer_item): storage =
  let transfer_one (store, tx : storage * transfer_destination): storage =
    let valid_from = check_sender (ti.from_, store) in
    let position_index = get_position_index(tx.token_id, store) in

    let position_indexes = change_position_owner (valid_from, tx, position_index, store) in
    { store with
      position_indexes = position_indexes
    }
  in List.fold transfer_one ti.txs store

let transfer (params, store : transfer_params * storage): result =
  let store = List.fold transfer_item params store in
  (([] : operation list), store)


// -----------------------------------------------------------------
// Balance of
// -----------------------------------------------------------------

let balance_of (params, store : balance_request_params * storage): result =
  let check_one (req : balance_request_item): balance_response_item =
    let pos_index = get_position_index(req.token_id, store) in
    let bal = if (req.owner = pos_index.owner) then 1n else 0n in
    { request = req; balance = bal} in
  let result = List.map check_one params.requests in
  let transfer_operation = Tezos.transaction result 0mutez params.callback in
  (([transfer_operation] : operation list), store)

// -----------------------------------------------------------------
// Update operators entrypoint
// -----------------------------------------------------------------

let update_one (store, param: storage * update_operator): storage =
  let (operator_update, operator_param) =
    match param with
      Add_operator p -> (Some unit, p)
    | Remove_operator p -> ((None : unit option), p) in
  if (Tezos.sender = operator_param.owner) then
    let key: operator = { owner = operator_param.owner; operator = operator_param.operator} in
    let updated_operators = Big_map.update key operator_update store.operators
    in  { store with
          operators = updated_operators
        }
  else
    (failwith("FA2_NOT_OWNER") : storage)

let update_operators (params, store : update_operators_param * storage):result =
  let store = List.fold update_one params store in
  (([] : operation list), store)


let call_fa2 (store : storage) (param : fa2_parameter) : result =
  match param with
    Transfer (p) -> transfer (p, store)
  | Balance_of (p) -> balance_of(p, store)
  | Update_operators (p) -> update_operators(p, store)
