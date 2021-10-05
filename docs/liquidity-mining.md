<!--
   - SPDX-FileCopyrightText: 2021 Arthur Breitman
   -
   - SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
   -->

# Liquidity mining

This document describes our example [implementation](/ligo/liquidity_mining.mligo) of liquidity mining based on the [CFMM Uniswap V3](specification.md) contract.

We mostly followed the design on [Solidity Staker-V3](https://github.com/Uniswap/v3-staker/tree/eff32b5509f87b3e66734c42ba5fc7c607dc583c) with small differences.

## Terms

* _Incentive_ — a reward program for liquidity providers.

* _Deposit_ — a position in the CFMM contract locked for the purposes of liquidity mining.

* _Stake_ — a fact of putting a deposit onto mining as part of the given incentive program.

## Workflow

### O. The liquidity mining contract is originated.

This contract is tied to exactly one Uniswap V3 contract.

There are some configuration options that are provided as part of initial storage, see `init_storage` function.

### I. A _rewarder_ calls `Create_incentive` entrypoint.

- This transfers a future reward in the given FA2 token to the mining contract.

- The reward will be distributed among those who create stakes as part of this incentive program.

- Expected start and end times of incentive are specified on creation.

The incentive can be terminated by calling `End_incentive` after the incentive end time has passed — this will send any reamining reward to the incentive's _refundee_.

### II. A _liquidity provider_ calls `Register_deposit` entrypoint.

- This transfers the sender's position to the liquidity mining contract.

- The new deposit is available for staking.

The deposit can be brought back by `Withdraw_deposit` call, assuming it has no active stakes.

Also, `Transfer_deposit` entrypoint allows moving deposit's ownership to someone else without withdrawing the deposit from the liquidity mining contract.

Step II does not depends on step I and can be performed before it.

### III. A liquidity provider stakes his deposit in the given incentive.

- This will start tracking for how long that position was active in the Uniswap contract.

- Any deposit can participate in an arbitrary number of incentives at a time.

### IV. The liquidity provider unstakes his deposit.

- This submits some reward to the deposit's owner. The reward will be proportional to the amount of time during which the position was active and to the reward rate assumed by the incentive (not necessarily constant).

### V. The liquidity provider claims his reward.

- All the rewards in any given token are stored for each LP within the contract.

- LP can obtain this reward via calling `Claim_reward`.
