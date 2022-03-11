// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

#if X_IS_FA2
type x_contract_transfer = (address * (address * (token_id * nat)) list) list
#else
type x_contract_transfer = address * (address * nat)
#endif

#if Y_IS_FA2
type y_contract_transfer = (address * (address * (token_id * nat)) list) list
type y_contract_operator_param = update_operators_param
#else
type y_contract_transfer = address * (address * nat)
type y_contract_operator_param = (address * nat) (* `approve` entrypoint. *)
#endif


(* Helper functions to create/remove an operator in x and y contracts. *)
let make_operator_in_y (operator : address) (limit : nat) (c : constants) : operation =
#if Y_IS_FA2
    let _limit = limit in // otherwise LIGO complains about 'limit' being unused
    let param = [ Add_operator
            { owner = Tezos.self_address
            ; operator = operator
            ; token_id = c.y_token_id
            } ] in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%update_operators" c.y_token_address
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_update_operator_invalid_entrypoints_err : y_contract_operator_param contract) in
#else
    let param = (operator, limit) in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%approve" c.y_token_address
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_approve_invalid_entrypoints_err : y_contract_operator_param contract) in
#endif
    Tezos.transaction param 0mutez y_contract

let remove_operator_in_y (operator : address) (c : constants) : operation =
#if Y_IS_FA2
    let param = [ Remove_operator
            { owner = Tezos.self_address
            ; operator = operator
            ; token_id = c.y_token_id
            } ] in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%update_operators" c.y_token_address
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_update_operator_invalid_entrypoints_err : y_contract_operator_param contract) in
#else
    let param = (operator, 0n) in
    let y_contract = match
        ( Tezos.get_entrypoint_opt "%approve" c.y_token_address
        : y_contract_operator_param contract option
        ) with
    | Some contract -> contract
    | None -> (failwith asset_approve_invalid_entrypoints_err : y_contract_operator_param contract) in
#endif
    Tezos.transaction param 0mutez y_contract


(* Helper functions to make transfers in asset x and y. *)
let x_transfer (from : address) (to_ : address) (amnt : nat) (c : constants) : operation =
    let x_contract: x_contract_transfer contract =
    match (Tezos.get_entrypoint_opt "%transfer" c.x_token_address : x_contract_transfer contract option) with
    | None -> (failwith asset_transfer_invalid_entrypoints_err : x_contract_transfer contract)
    | Some contract -> contract in
#if X_IS_FA2
    Tezos.transaction [(from, [(to_, (c.x_token_id, amnt))])] 0mutez x_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez x_contract
#endif

let y_transfer (from : address) (to_ : address) (amnt : nat) (c : constants) : operation =
    let y_contract: y_contract_transfer contract =
    match (Tezos.get_entrypoint_opt "%transfer" c.y_token_address : y_contract_transfer contract option) with
    | None -> (failwith asset_transfer_invalid_entrypoints_err : y_contract_transfer contract)
    | Some contract -> contract in
#if Y_IS_FA2
    Tezos.transaction [(from, [(to_, (c.y_token_id, amnt))])] 0mutez y_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez y_contract
#endif
