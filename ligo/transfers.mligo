// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

type token_id = nat

#if X_IS_FA2
[@inline] let const_x_token_id = 0n (* CHANGEME *)
type x_contract_transfer = (address * (address * (token_id * nat)) list) list
#else
type x_contract_transfer = address * (address * nat)
#endif

#if Y_IS_FA2
[@inline] let const_y_token_id = 0n (* CHANGEME *)
type y_contract_transfer = (address * (address * (token_id * nat)) list) list
type y_contract_operator_param = update_operators_param
#else
type y_contract_transfer = address * (address * nat)
type y_contract_operator_param = (address * nat) (* `approve` entrypoint. *)
#endif

[@inline] let const_x_token_entrypoint = ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address) (* CHANGEME *)
[@inline] let const_y_token_entrypoint = ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address) (* CHANGEME *)

(* Helper functions to create/remove an operator in x and y contracts. *)
let make_operator_in_y (operator : address) (limit : nat) : operation =
#if Y_IS_FA2
    let param = [ Add_operator
            { owner = Tezos.self_address
            ; operator = operator
            ; token_id = const_y_token_id
            } ] in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%update_operators" const_y_token_entrypoint
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_update_operator_invalid_entrypoints_err : y_contract_operator_param contract) in
#else
    let param = (operator, limit) in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%approve" const_y_token_entrypoint
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_approve_invalid_entrypoints_err : y_contract_operator_param contract) in
#endif
    Tezos.transaction param 0mutez y_contract

let remove_operator_in_y (operator : address) : operation =
#if Y_IS_FA2
    let param = [ Remove_operator
            { owner = Tezos.self_address
            ; operator = operator
            ; token_id = const_y_token_id
            } ] in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%update_operators" const_y_token_entrypoint
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_update_operator_invalid_entrypoints_err : y_contract_operator_param contract) in
#else
    let param = (operator, 0n) in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%approve" const_y_token_entrypoint
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_approve_invalid_entrypoints_err : y_contract_operator_param contract) in
#endif
    Tezos.transaction param 0mutez y_contract


(* Helper functions to make transfers in asset x and y. *)
let x_transfer (from : address) (to_ : address) (amnt : nat) : operation =
    let x_contract: x_contract_transfer contract =
    match (Tezos.get_contract_opt const_x_token_entrypoint : x_contract_transfer contract option) with
    | None -> (failwith asset_transfer_invalid_entrypoints_err : x_contract_transfer contract)
    | Some contract -> contract in
#if X_IS_FA2
    Tezos.transaction [(from, [(to_, (const_x_token_id, amnt))])] 0mutez x_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez x_contract
#endif

let y_transfer (from : address) (to_ : address) (amnt : nat) : operation =
    let y_contract: y_contract_transfer contract =
    match (Tezos.get_contract_opt const_y_token_entrypoint : y_contract_transfer contract option) with
    | None -> (failwith asset_transfer_invalid_entrypoints_err : y_contract_transfer contract)
    | Some contract -> contract in
#if Y_IS_FA2
    Tezos.transaction [(from, [(to_, (const_y_token_id, amnt))])] 0mutez y_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez y_contract
#endif
