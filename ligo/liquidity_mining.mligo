// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-GPL3-Arthur-Breitman

(* This contract follows the implementation of Solidity staker-v3.

* Repository: https://github.com/Uniswap/v3-staker/
* Commit: eff32b5509f87b3e66734c42ba5fc7c607dc583c

Interesting points:
* Interface: https://github.com/Uniswap/v3-staker/blob/eff32b5509f87b3e66734c42ba5fc7c607dc583c/contracts/interfaces/IUniswapV3Staker.sol
* Implementation: https://github.com/Uniswap/v3-staker/blob/eff32b5509f87b3e66734c42ba5fc7c607dc583c/contracts/UniswapV3Staker.sol

DISCLAIMER: this contract is a demo version of liquidity mining functionality,
we provide it without any guarantees. Use in production is discouraged until
further analysis is performed.

*)

#include "types.mligo"

(*** Utils ***)

let assert_nat (x, error_code : int * string) : nat =
    match is_nat x with
    | None -> (failwith error_code : nat)
    | Some n -> n

(*** Types ***)

// Information about reward token.
type fa2_token = {
    // FA2 transfer capability
    address : address;
    // Token id in FA2
    token_id : nat;
}

// Immutable info about an incentive.
//
// In the solidity implementation this is IncentiveKey type.
// But we move away from this approach because using such info as a key is
// inconvenient.
type incentive_params = {
    // A FA2 token used for distributing rewards for participating in
    // incentive programs.
    reward_token : fa2_token;
    // Total reward proposed.
    // This field is not strictly necessary in storage,
    // we provide it for our convenience.
    total_reward : nat;
    // Start time of the incentive.
    start_time : timestamp;
    // The time after which the incentive can be closed.
    end_time : timestamp;
    // The address which receives any remaining reward tokens after the end time.
    refundee : address;
}

// One incentive usually corresponds to a single owner/refundee.
type incentive = {
    // Immutable fields of incentive.
    p : incentive_params;

    // Pending reward.
    total_reward_unclaimed : nat;

    // Seconds that have already been accounted taken into account in reward
    // calculation for this incentive.
    total_seconds_claimed : x128n;

    // Number of stakes referring this incentive.
    number_of_stakes : nat;
}

type incentive_id = nat

// Deposit keeps information about a single position in the Uniswap contract.
//
// One deposit corresponds to a single position in the uniswap contract.
// If a deposit is registered, that means that we are currently the owner
// of the respective position.
type deposit = {
    // The original owner of the deposit.
    owner : address;

    // Number of stakes created for this deposit.
    // Use bool?
    number_of_stakes : nat;

    // Lower and upper bound of the position.
    // Initialized when the deposit is first staked.
    tick_index_range : (tick_index * tick_index) option;
}

// Stake is deposit locked for mining.
//
// If a stake is registered for the given deposit, such deposit cannot be removed.
type stake = {
    // The initial 1/L cumulative value inside the position.
    seconds_per_liquidity_inside_initial : x128;

    // The liquidity amount provided to the stake.
    liquidity : nat;
}

// Information carried through different phases of `stake_token` implementation
// (that requires CPS style until the recent embeded on-chain views feature comes into play).
type stake_token_phase
    = StNotStarted
    | StAskedPositionInfo of incentive_id * position_id
    | StAskedCumulativesInfo of (incentive_id * position_id) * position_info

// Information carries through different phases of `unstake_token` implementation
type unstake_token_phase
    = UstNotStarted
    | UstAskedCumulativesInfo of (incentive_id * position_id) * address

type config = {
    // Max allowed delay before incentive creation and
    // its start time.
    max_incentive_start_lead_time : nat;

    // Max allowed duration of incentive, in seconds.
    max_incentive_duration : nat;
}

type storage = {
    // Address of the uniswap contract.
    uniswap : address;

    // Contract options.
    config : config;

    // Stored incentives.
    incentives : (incentive_id, incentive) big_map;

    // Next incentive_id to be used in incentive creation.
    next_incentive_id : incentive_id;

    // Stored deposits.
    deposits : (position_id, deposit) big_map;

    // Stored stakes.
    stakes : (incentive_id * position_id, stake) big_map;

    // Mapping from (token, owner) to how much reward this
    // owner has claimed.
    rewards : (fa2_token * address, nat) big_map;

    // The current phase of `stake_token` call.
    // If we are not in the process of calling this entrypoint, set to `StNotStarted`.
    stake_token_phase : stake_token_phase;

    // The current phase of `unstake_token` call.
    // If we are not in the process of calling this entrypoint, set to `UstNotStarted`.
    unstake_token_phase : unstake_token_phase;

}

let init_storage (c : config) (uniswap : address) : storage = {
    uniswap = uniswap;
    config = c;
    incentives = (Big_map.empty : (incentive_id, incentive) big_map);
    next_incentive_id = 0n;
    deposits = (Big_map.empty : (position_id, deposit) big_map);
    stakes = (Big_map.empty : (incentive_id * position_id, stake) big_map);
    rewards = (Big_map.empty : (fa2_token * address, nat) big_map);
    stake_token_phase = StNotStarted;
    unstake_token_phase = UstNotStarted;
}

type return = operation list * storage

type destination_address = address

type parameter =
    | Create_incentive of incentive_params
    | End_incentive of incentive_id
    | Register_deposit of position_id
    | Transfer_deposit of position_id * destination_address
    | Withdraw_deposit of position_id * destination_address
    | Stake_token of incentive_id * position_id
    | Stake_token_on_position_info of position_info
    | Stake_token_on_cum_info of cumulatives_inside_snapshot
    | Unstake_token of incentive_id * position_id
    | Unstake_token_on_cum_info of cumulatives_inside_snapshot
    | Claim_reward of fa2_token * (destination_address * nat option)
    // Get_reward_info: unlike the solidity impl, not provided, must be computed off-chain.
    // Note for future: The proper contract also needs getters, but for demo we probably don't care

(*** Entrypoints ***)

let require(cond : bool) (err : string) : unit =
    if cond then unit else failwith err

let transfer_ep(addr : address) : transfer_params contract =
    match (Tezos.get_entrypoint_opt "%transfer" addr : transfer_params contract option) with
    | None -> failwith "No %transfer entrypoint in token"
    | Some contract -> contract

type reward_computation_result = {
    reward : nat;
    remaining_reward_unclaimed : nat;
    seconds_inside : x128n;
    new_total_seconds_claimed : x128n;
}

let compute_reward_amount
    (total_reward_unclaimed : nat)
    (total_seconds_claimed : x128n)
    (start_time : timestamp)
    (end_time : timestamp)
    (liquidity : nat)
    (seconds_per_liqudity_inside_initial : x128)
    (seconds_per_liqudity_inside : x128)
        : reward_computation_result = begin
    // Follows the solidity impl:
    // https://github.com/Uniswap/v3-staker/blob/eff32b5509f87b3e66734c42ba5fc7c607dc583c/contracts/libraries/RewardMath.sol

    require (Tezos.now >= start_time)
        "Should not be called before start time";

    let max_timestamp(a, b : timestamp * timestamp) : timestamp =
        if a > b then a else b in

    let seconds_per_liquidity_inside_diff =
        {x128 = assert_nat
            ( seconds_per_liqudity_inside.x128 - seconds_per_liqudity_inside_initial.x128
            , "Invalid cumulative values returned"
            )} in
    let seconds_inside =
        {x128 = seconds_per_liquidity_inside_diff.x128 * liquidity} in

    let total_seconds_for_reward =
        assert_nat(max_timestamp(end_time, Tezos.now) - start_time, "impossible") in
    let total_seconds_unclaimed =
        {x128 =
            assert_nat
                ( Bitwise.shift_left total_seconds_for_reward 128n - total_seconds_claimed.x128
                , "Unexpectedly, claimed too much seconds by this point"
                )} in

    // reward
    //   = seconds_inside * reward_rate
    //   = seconds_inside * (total_reward_unclaimed / total_seconds_unclaimed)
    let reward = total_reward_unclaimed * seconds_inside.x128 / total_seconds_unclaimed.x128 in

    // TODO: what if total_seconds_unclaimed is zero?
    // This is possible when `compute_reward_amount` is called for the same
    // incentive twice in the same block,
    // or when `incentive.start_time == incentive.end_time` which we do not forbid.

    // The intuition behind the reward formula:
    //
    // The incentive exists for [start_time, end_time] range (but may also terminate later).
    // We want to distribute the reward among those who had their position active,
    // and the one's reward should be proportional to the
    // `position_is_active_duration / incentive_lifetime` fraction.
    //
    // If I'm the only participant and my position is active 100% of the time,
    // and I trigger computation reward in the incentive's end, I will get the
    // full reward (`seconds_inside = total_seconds_unclaimed = total_seconds_for_reward`).
    //
    // If I trigger reward computation multiple times during the incentive's lifetime,
    // I will get the same full reward in the respective portions.
    // E.g. if my position is active 50% of the time during the first half of the
    // incentive lifetime, I will get
    // `reward
    //    = total_reward_unclaimed * (0.5 * 0.5 * incentive_lifetime / incentive_lifetime)
    //    = initial_reward * 0.25`.
    // If then I immediately stake the token again and keep the position active
    // 50% of the time, I will get
    // `reward
    //    = total_reward_unclaimed * (0.5 * 0.5 * incentive_lifetime / (0.75 * incentive_lifetime))
    //    = (initial_reward * 0.75) * 1/3
    //    = initial_reward * 0.25`
    // which is the very same
    //
    // If I'm not the only participant (stake) in this incentive, overall
    // we can get more than 100% of the time when those positions (deposits)
    // were active, in such case the reward may get exhausted in the middle
    // of the incentive.
    // TODO: doesn't this encourage participants to constantly unstake and
    // stake tokens in order not to lose their reward to other participants?
    // If so, this distribution machanism does not sound good.

    let (reward, remaining_reward) =
        // TODO: for some reason this check is missing in the solidity impl
        // do I miss smth :thinking:
        match is_nat(total_reward_unclaimed - reward) with
        | Some remaining -> (reward, remaining)
        | None -> (total_reward_unclaimed, 0n)

    in  { reward = reward
        ; seconds_inside = seconds_inside
        ; remaining_reward_unclaimed = remaining_reward
        ; new_total_seconds_claimed =
            {x128 = total_seconds_claimed.x128 + seconds_inside.x128}
        }
end

// Create an incentive program.
let create_incentive(p, s : incentive_params * storage) : return = begin
    require (p.total_reward > 0n)
        "Reward must be positive";
    require (Tezos.now <= p.start_time)
        "Start time must be now or in the future";
    require (p.start_time - Tezos.now <= int s.config.max_incentive_start_lead_time)
        "Start time too far into future";
    require (p.start_time < p.end_time)
        "Start time must be before end time";
    require (p.end_time - p.start_time <= int s.config.max_incentive_duration)
        "Incentive duration is too long";

    // Unlike the solidity implementation, we always allocate new incentive.
    // Note for future: in prod, adding reward in existing incentive may be necessary
    let new_incentive : incentive = {
        total_reward_unclaimed = p.total_reward;
        total_seconds_claimed = {x128 = 0n};
        number_of_stakes = 0n;
        p = p;
    } in
    let s = {s with
        incentives = Big_map.add s.next_incentive_id new_incentive s.incentives;
        next_incentive_id = s.next_incentive_id + 1n;
    } in

    let op = Tezos.transaction
        [   { from_ = Tezos.sender
            ; txs = [{to_ = Tezos.self_address; token_id = p.reward_token.token_id; amount = p.total_reward}]
            }
        ] 0mutez (transfer_ep p.reward_token.address) in
    ([op], s)
end

// Terminate incentive, transferring the unclaimed reward to the incentive's refundee.
let end_incentive(id, s : incentive_id * storage) : return = begin
    let incentive = match Big_map.find_opt id s.incentives with
        | Some v -> v
        | None -> (failwith "No such incentive" : incentive) in

    require (Tezos.now >= incentive.p.end_time)
        "Cannot end incentive before end time";

    let refund = incentive.total_reward_unclaimed in

    require (refund > 0n)
        "No refund available";
    require (incentive.number_of_stakes = 0n)
        "Cannot end incentive while deposits are staked";

    // Issue the refund
    let incentive = {incentive with total_reward_unclaimed = 0n} in
    let s = {s with incentives = Big_map.add id incentive s.incentives} in

    let op = Tezos.transaction
        [   { from_ = Tezos.self_address
            ; txs = [{to_ = incentive.p.refundee; token_id = incentive.p.reward_token.token_id; amount = refund}]
            }
        ] 0mutez (transfer_ep incentive.p.reward_token.address) in

    // Note: we never clear `total_seconds_claimed`

    (([op] : operation list), s)
end

// Register deposit.
// This will lock the specified position by transferring it to our contract.
//
// Unlike in the solidity implementation, we don't execute this as position
// transfer callback, rather calling this entrypoint performs the position transfer.
//
// Another difference: we do not allow staking the deposit in the same entrypoint
// call, a separate `stake_token` call will be necessary.
// This might be improved in the future versions.
let register_deposit(position_id, s : position_id * storage) : return = begin
    let new_deposit : deposit =
        { owner = Tezos.sender
        ; number_of_stakes = 0n
        ; tick_index_range = (None : (tick_index * tick_index) option)
        } in
    // This insertion is safe - if it overwrites a value, the subsequent attempt
    // to transfer position will fail (since positions are NFT).
    let s = {s with deposits = Map.add position_id new_deposit s.deposits } in

    let op = Tezos.transaction
        [   { from_ = Tezos.sender
            ; txs = [{to_ = Tezos.self_address; token_id = position_id; amount = 1n}]
            }
        ] 0mutez (transfer_ep s.uniswap) in

    ([op], s)
end

// Change deposit's owner.
let transfer_deposit(position_id, to_, s : position_id * address * storage) : return = begin
    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in

    require (deposit.owner = Tezos.sender)
        "Can only be called by the deposit owner";

    let deposit = {deposit with owner = to_} in
    let s = {s with deposits = Big_map.add position_id deposit s.deposits} in
    (([] : operation list), s)
end

// Unlock position back.
let withdraw_deposit(position_id, to_, s : position_id * address * storage) : return = begin
    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in

    require (deposit.number_of_stakes = 0n)
        "Cannot withdraw token while staked";
    require (deposit.owner = Tezos.sender)
        "Only owner can withdraw token";

    let s = {s with deposits = Big_map.remove position_id s.deposits} in

    let op = Tezos.transaction
        [   { from_ = Tezos.self_address
            ; txs = [{to_ = to_; token_id = position_id; amount = 1n}]
            }
        ] 0mutez (transfer_ep s.uniswap) in

    ([op], s)
end

// Stake liquidity position token.
// During the period when the deposit was staked and respective position was active,
// the deposit's owner will receive rewards for providing liquidity.
let stake_token(incentive_id, position_id, s : incentive_id * position_id * storage) : return = begin
    let _ : unit = match s.stake_token_phase with
        | StNotStarted -> unit
        | _ -> failwith "Reentrant `stake_token` call" in

    let s = {s with stake_token_phase = StAskedPositionInfo (incentive_id, position_id)} in

    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in

    require (deposit.owner = Tezos.sender)
        "Only owner can stake token";

    let get_position_info_ep =
        match (Tezos.get_entrypoint_opt "%get_position_info" s.uniswap : get_position_info_param contract option) with
        | None -> (failwith "Bad CFMM contract" : get_position_info_param contract)
        | Some contract -> contract in
    let get_position_info_op =
        Tezos.transaction
        { position_id = position_id
        ; callback = (Tezos.self "%stake_token_on_position_info" : position_info contract)
        } 0mutez get_position_info_ep in

    (([get_position_info_op] : operation list), s)
end

// The 2-nd phase of stake_token.
let stake_token_on_position_info(position_info, s : position_info * storage) : return = begin
    let stored_info = match s.stake_token_phase with
        | StAskedPositionInfo stored_info -> stored_info
        | _ -> (failwith "Wrong `stake_token_on_position_info` call" : incentive_id * position_id) in

    let s = {s with stake_token_phase = StAskedCumulativesInfo (stored_info, position_info)} in

    require (position_info.liquidity > 0n)
        "Cannot stake token with 0 liquidity";

    let get_cumulatives_info_ep =
        match (Tezos.get_entrypoint_opt "%snapshot_cumulatives_inside" s.uniswap : snapshot_cumulatives_inside_param contract option) with
        | None -> (failwith "Bad CFMM contract" : snapshot_cumulatives_inside_param contract)
        | Some contract -> contract in
    let get_cumulatives_info_op =
        Tezos.transaction
        { lower_tick_index = position_info.index.lower_tick_index
        ; upper_tick_index = position_info.index.upper_tick_index
        ; callback = (Tezos.self "%stake_token_on_cum_info" : cumulatives_inside_snapshot contract)
        } 0mutez get_cumulatives_info_ep in

    (([get_cumulatives_info_op] : operation list), s)
end

// The 3-nd phase of stake_token.
let stake_token_on_cum_info(cumulatives_snapshot, s : cumulatives_inside_snapshot * storage) : return = begin
    let ((incentive_id, position_id), position_info) = match s.stake_token_phase with
        | StAskedCumulativesInfo stored_info -> stored_info
        | _ -> (failwith "Wrong `stake_token_on_cum_info` call" : (incentive_id * position_id) * position_info) in

    let s = {s with stake_token_phase = StNotStarted} in

    let incentive = match Big_map.find_opt incentive_id s.incentives with
        | Some v -> v
        | None -> (failwith "Incentive does not exist" : incentive) in
    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in
    let _ : unit = match Big_map.find_opt (incentive_id, position_id) s.stakes with
        | Some _ -> failwith "Token already staked"
        | None -> unit in

    let deposit = {deposit with
        tick_index_range = Some
            ( position_info.index.lower_tick_index
            , position_info.index.upper_tick_index
            );
        number_of_stakes = deposit.number_of_stakes + 1n;
    } in
    let incentive = {incentive with
        number_of_stakes = incentive.number_of_stakes + 1n
    } in

    let stake : stake = {
        seconds_per_liquidity_inside_initial = cumulatives_snapshot.seconds_per_liquidity_inside;
        liquidity = position_info.liquidity;
    } in

    let s = {s with
        deposits = Big_map.add position_id deposit s.deposits;
        incentives = Big_map.add incentive_id incentive s.incentives;
        stakes = Big_map.add (incentive_id, position_id) stake s.stakes;
    } in

    (([] : operation list), s)
end

// Unstake liquidity position.
let unstake_token(incentive_id, position_id, s : incentive_id * position_id * storage) : return = begin
    let _ : unit = match s.unstake_token_phase with
        | UstNotStarted -> unit
        | _ -> failwith "Reentrant `unstake_token` call" in

    let s = {s with unstake_token_phase = UstAskedCumulativesInfo ((incentive_id, position_id), Tezos.sender)} in

    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in
    let (lower_tick_index, upper_tick_index) =
        match deposit.tick_index_range with
        | None -> (failwith "Internal: ticks not yet initialized" : tick_index * tick_index)
        | Some v -> v in

    let get_cumulatives_info_ep =
        match (Tezos.get_entrypoint_opt "%snapshot_cumulatives_inside" s.uniswap : snapshot_cumulatives_inside_param contract option) with
        | None -> (failwith "Bad CFMM contract" : snapshot_cumulatives_inside_param contract)
        | Some contract -> contract in
    let get_cumulatives_info_op =
        Tezos.transaction
        { lower_tick_index = lower_tick_index
        ; upper_tick_index = upper_tick_index
        ; callback = (Tezos.self "%unstake_token_on_cum_info" : cumulatives_inside_snapshot contract)
        } 0mutez get_cumulatives_info_ep in

    ([get_cumulatives_info_op], s)
end

// The 2-nd phase of unstake_token.
let unstake_token_on_cum_info(cumulatives_snapshot, s : cumulatives_inside_snapshot * storage) : return = begin
    let ((incentive_id, position_id), orig_sender) = match s.unstake_token_phase with
        | UstAskedCumulativesInfo stored_info -> stored_info
        | _ -> (failwith "Wrong `unstake_token_on_cum_info` call" : (incentive_id * position_id) * address) in

    let s = {s with unstake_token_phase = UstNotStarted} in

    let incentive = match Big_map.find_opt incentive_id s.incentives with
        | Some v -> v
        | None -> (failwith "Incentive does not exist" : incentive) in
    let deposit = match Big_map.find_opt position_id s.deposits with
        | Some v -> v
        | None -> (failwith "Deposit does not exist" : deposit) in
    let stake = match Big_map.find_opt (incentive_id, position_id) s.stakes with
        | Some v -> v
        | None -> (failwith "Stake does not exist" : stake) in

    // Anyone can call unstake_token if the block time is after the end time
    // of the incentive.
    // This is necessary e.g. to later terminate the related incentive and
    // reward the incentive's refundee.
    if (Tezos.now < incentive.p.end_time) then
        require (deposit.owner = orig_sender)
            "Only owner can withdraw token before incentive end time"
    else ();

    let deposit = {deposit with number_of_stakes = assert_nat(deposit.number_of_stakes - 1n, "deposit.number_of_stakes < 0")} in
    let incentive = {incentive with number_of_stakes = assert_nat(incentive.number_of_stakes - 1n, "incentive.number_of_stakes < 0")} in

    let res = compute_reward_amount
        incentive.total_reward_unclaimed
        incentive.total_seconds_claimed
        incentive.p.start_time
        incentive.p.end_time
        stake.liquidity
        stake.seconds_per_liquidity_inside_initial
        cumulatives_snapshot.seconds_per_liquidity_inside
        in

    let incentive = {incentive with
        total_seconds_claimed = res.new_total_seconds_claimed;
        total_reward_unclaimed = res.remaining_reward_unclaimed;
    } in

    let total_reward = match Big_map.find_opt (incentive.p.reward_token, deposit.owner) s.rewards with
        | Some v -> v
        | None -> 0n in
    let total_reward = total_reward + res.reward in

    let s = {s with
        deposits = Big_map.add position_id deposit s.deposits;
        incentives = Big_map.add incentive_id incentive s.incentives;
        stakes = Big_map.remove (incentive_id, position_id) s.stakes;
        rewards = Big_map.add (incentive.p.reward_token, deposit.owner) total_reward s.rewards
    } in

    (([] : operation list), s)
end

// Claim reward earned previously and transfer it to the given address.
// You supply reward token, and reward is gathered from all the incentives that
// use this reward token.
//
// Reward for a stake is granted only after unstaking.
//
// If more reward is requested than available, this call fails (unlike in the solidity impl).
// If no explicit reward amount is requested (`reward_requested = None`), all the
// available reward is transferred.
let claim_reward(reward_token, to_, reward_requested, s : fa2_token * address * nat option * storage) : return = begin
    let reward_key = (reward_token, Tezos.sender) in
    let available_reward = match Big_map.find_opt reward_key s.rewards with
        | Some v -> v
        | None -> 0n in
    let (reward_picked, reward_remaining) = begin match reward_requested with
        | None -> (available_reward, 0n)
        | Some requested -> match is_nat(available_reward - requested) with
            | None -> (failwith "Insufficient reward available" : (nat * nat))
            | Some remaining -> (requested, remaining)
        end in

    let s = {s with rewards = Big_map.add reward_key reward_remaining s.rewards} in

    let op = Tezos.transaction
        [   { from_ = Tezos.self_address
            ; txs = [{to_ = to_; token_id = reward_token.token_id; amount = reward_picked}]
            }
        ] 0mutez (transfer_ep reward_token.address) in

    ([op], s)
end

let main(p, s : parameter * storage) : return =
    match p with
    | Create_incentive p -> create_incentive(p, s)
    | End_incentive k -> end_incentive(k, s)
    | Register_deposit pid -> register_deposit(pid, s)
    | Transfer_deposit (pid, to_) -> transfer_deposit(pid, to_, s)
    | Withdraw_deposit (pid, to_) -> transfer_deposit(pid, to_, s)
    | Stake_token (cid, pid) -> stake_token(cid, pid, s)
    | Stake_token_on_position_info pi -> stake_token_on_position_info(pi, s)
    | Stake_token_on_cum_info c -> stake_token_on_cum_info(c, s)
    | Unstake_token (cid, pid) -> unstake_token(cid, pid, s)
    | Unstake_token_on_cum_info c -> unstake_token_on_cum_info(c, s)
    | Claim_reward (rt, (to_, ra)) -> claim_reward(rt, to_, ra, s)
