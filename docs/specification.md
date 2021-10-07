<!--
   - SPDX-FileCopyrightText: 2021 Arthur Breitman
   -
   - SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
   -->

# Segmented CFMM

This contract implements a Constant Function Market Maker (CFMM) in Michelson inspired by [Uniswap v3][uniswap-v3].

# Background

In a traditional exchange, users will put in buy orders and sell orders.
The exchange will find a match between a buy order and a sell order and conduct the transaction.
If there aren't enough users, it might be difficult or even impossible to find a match and execute trades.
When that happens, the asset being traded is said to have low liquidity, i.e., it's hard to liquidate the asset.

A Market Maker (MM) is an entity that brings liquidity to a market, by constantly buying and selling assets from/to users.

In a Decentralized Exchange (DEX), users don't buy from/sell to each other directly.
Instead, they only buy from/sell to an Automated Market Maker (AMM).
The AMM manages one liquidity pool (or more), a pot made of funds deposited by those who wish to contribute - the Liquidity Providers (LPs).

For example, say we have two tokens, `x` and `y`, and a liquidity pool that maintains reserves of both tokens.
LPs can deposit their own tokens in this pool to increase its reserves.
Users can then sell their `x` tokens and get `y` tokens in return (swapping `x` for `y`),
or buy `x` tokens in exchange for their `y` tokens (swapping `y` for `x`).

## Overview

This contract is a Constant Function Market Maker (CFMM), a class of AMMs,
and is capable of managing a pool of any two FA1.2/FA2 tokens.

This CFMM uses a _function_ to ensure that, after every swap, the product of the
reserves `x` and `y` is always equal to some _constant_ `k`, i.e., `x * y = k`.

For example, say `k = 15600`, and LPs provide liquidity (see [Positions](#positions))
such that our pool contains 1560 `x` tokens and 10 `y` tokens,
satisfying the `x * y = k` requirement.

If a user sells 3 `y` tokens (see [Swaps](#swaps)), the pool's `y` reserves would increase to 13.
Since the product of the reserves must remain equal to `k` at all times, it follows that the `x`
reserves must now be `x = k / y = 15600 / 13 = 1200`.
Thus, the user would get `1560 - 1200 = 360` `x` tokens in exchange for their 3 `y` tokens.

A constant product ensures that the price of an asset goes up or down according to demand.
The more a token's reserves are depleted, the more expensive it'll be for a user to buy more of those tokens.
If a user were to sell 3 more `y` tokens, they would now receive _only_ `x - (k / y) = 1200 - (15600 / 16) = 225` `x` tokens.

In exchange for their contribution, LPs are rewarded with [fees subtracted from every swap](#fees).

This contract also exposes on-chain view entrypoints for implementing [price oracles](#price-oracle)
and [liquidity mining programs](#liquidity-mining).

## Positions

At any given time, in a pool of two tokens `x` and `y`, the [_spot price_][spot-price] of `x` can range from 0
(when the pool contains only `x` tokens) to ∞ (when the pool contains only `y` tokens).

However, in most cases, the price of a token will almost always be within a certain range.
For example, in a pool of 2 stablecoins pegged to the same currency,
we might expect the price to always stay within the range [0.99, 1.01].

In such a scenario, the `x` and `y` reserves would almost always remain very close to each other.
This means that most of the pool's tokens would never actually be used.

As such, instead of allocating their tokens to the entire [0, ∞] range, LPs can concentrate
their liquidity in a specific, narrower range.

In order to do this, we split the price spectrum [0, ∞] in _tick intervals_, each interval bound by two _ticks_.
For any integer `i` (the _tick index_), there is a tick at the price `p(i) = exp(0.0001)^i`.

```
0, ..., 0.99980001999, 0.999900005, 1, 1.000100005, 1.00020002, ..., ∞
```

LPs can open a _position_ (that is, allocate their liquidity inbetween any two ticks)
by calling the [`set_position`](#set_position) entrypoint.
This same entrypoint can also be used to close or update a position.

When the spot price is within a position's range, that position is said to be _active_.
The LP will earn fees taken from every swap that occurs while their position is active.

Once the spot price moves outside the range (because the tokens in that position
have all been converted to either `x` or `y`), the position will become _inactive_,
and will not accrue any fees until is becomes active again.

---

This partitioning system has some implications.

In particular, some ticks intervals may have more liquidity than others, which means
the spot price will swing more easily while within a tick interval with low liquidity than
within a tick interval with higher liquidity.

## Swaps

Users may call the [`x_to_y`](#x_to_y) or [`y_to_x`](#y_to_x) entrypoints
to swap their `x` or `y` tokens, respectively.

<a name="initialized-tick-definition"></a>

A tick is said to be _initialized_ if it is currently being used as a bound of a position,
or _uninitialized_ otherwise.

In the example above, we have two positions.
Position `p1` ranges from `p(0)` to `p(4)`, and `p2` from `p(2)` to `p(6)`.

```
                             p2
                  _______________________
                 |                       |
... p(0)  p(1)  p(2)  p(3)  p(4)  p(5)  p(6)  ...
     |_______________________|
                 p1
```

The ticks `p(0)`, `p(2)`, `p(4)` and `p(6)` are initialized,
while the remaining are uninitialized.

When a swap is initiated, the contract will first attempt to convert as many tokens as possible
with the liquidity available up to the nearest initialized tick.
While swapping inbetween two initialized ticks, the contract acts a constant product formula.

For example, if the current price is `p(1)`, then the contract will convert as many tokens
as possible using position `p1`'s liquidity alone.

Once the price crosses an initialized tick, then the right amount of liquidity is
added to/removed from the calculations.

For example, when the price moves from `p(1)` to `p(2)`, we start taking position `p2`'s
liquidity into account.
When it moves from `p(3)` to `p(4)`, we stop taking `p1`'s liquidity into account.

## Fees

At the beginning of every swap, the contract subtracts a _swap fee_ from the tokens sent in,
_before_ all other calculations. Whatever remains, is deposited into the liquidity pool and
exchanged for the other asset.

For example, if the swap fee is 0.3% and the user sends in 10000 `y` tokens, then:
* 30 `y` tokens will be collected as swap fees and set aside,
* 9970 `y` tokens will be deposited into the liquidity pool and exchanged for `x` tokens
  according to the _constant function_ calculations.

Furthermore, when the `y` token is [`CTEZ`][ctez], a _protocol fee_ is subtracted from the `CTEZ`
tokens being deposited/withdrawn on every swap.
This fee is burned immediately.

When an LP updates their position, they are rewarded with part of the swap fees paid by users,
in both `x` and `y` tokens.
This reward is proportional to:
* the position's size,
* and the amount of swap fees collected while the position remained _active_ since the last
  time the position was updated/created.

Both the swap fee and the protocol fee percentages are initialized when the contract is
originated and immutable thereafter.

## Price Oracle

Conceptually, at every block level, the contract will calculate a cumulative sum
of the _current tick index_ `ic` since the contract's inception.

At any point in time `t`, the accumulator `tick_cumulative(t)` is equal to:

```
tick_cumulative(t) = ic(0) + ic(1) + ... + ic(t)
```

The contract will take a checkpoint of the current `tick_cumulative(t)` and store it in a `big_map`.

Contracts in the periphery may use the [`observe`](#observe) view entrypoint
to retrieve these checkpoints and implement a price oracle.

To compute the time-weighted geometric mean price of the `x` token between two times `t1` and `t2`,
the price oracle contract may call `oracle [t1, t2]` to obtain `tick_cumulative_t1` and `tick_cumulative_t2`,
and apply the following formula:

```
PX(t1, t2) = exp(0.0001) ^ ( (tick_cumulative_t2 - tick_cumulative_t1) / (t2 - t1) )
```

To compute the time-weighted geometric mean price of the `x` token,
we simply calculate the reciprocal of `PX`:

```
PY(t1, t2) = 1 / PX (t1, t1)
```

## Liquidity Mining

A contract in the periphery may want to reward liquidity miners for staking their positions by distributing
some token `R` at a constant `rate` per second while the position is active.

In order to do this, when the user stakes their position, the contract may use the [`get_position_info`](#get_position_info)
view entrypoint to get the current liquidity, and [`snapshot_cumulatives_inside`](#snapshot_cumulatives_inside) view entrypoint
to obtain `seconds_per_liquidity_cumulative_t0` and keep a record of them.

Later, when the user unstakes their position, the contract calls the same view entrypoint once more to obtain
`seconds_per_liquidity_cumulative_t1`.
It can then apply the following formula to calculate how many `R` tokens to reward the user with,
where `position_liquidity` is the amount of liquidity provided by the user's position.

```
rate * position_liquidity * (seconds_per_liquidity_cumulative_t1 - seconds_per_liquidity_cumulative_t0)
```

# Configuration

This contract relies on some constants and known external conditions which can
differ between instantiations.

To accomodate this, some configuration options are given at compile-time,
which means that to make different choices a new compilation is required, others
at origination time, which means that a contract cannot change these choices
after deployment.

See [compilation](./compilation.md) for more information.

## Configuration options

- `x_token_type`: contract type of the `x` token, can be either [`FA1.2`][fa1.2] or [`FA2`][fa2].
- `y_token_type`: contract type of the `y` token, can be either [`FA1.2`][fa1.2], [`FA2`][fa2], or [`CTEZ`][ctez].
- `fee_bps`, a fraction determining how much of the tokens sent in a swap will
  be subtracted beforehand, see [fees](#fees) for more info.
- `ctez_burn_fee_bps`, a percentage to be subtracted from the `CTEZ` tokens being
  deposited/withdrawn on every swap.
  See [fees](#fees) for more info.
  This option is only valid when the `y_token_type` is `CTEZ`.
- `x_token_address` the `address` of the contract holding the `x` token.
- `y_token_address` the `address` of the contract holding the `y` token.
- `x_token_id` the [`FA2`][fa2] `token_id` for the `x` token.
  This option is only valid when the `x_token_type` is `CTEZ` or `FA2`.
- `y_token_id` the [`FA2`][fa2] `token_id` for the `y` token.
  This option is only valid when the `y_token_type` is `CTEZ` or `FA2`

# Entrypoints

## Standard FA2 entrypoints

Entrypoints present in the [*FA2 Standard*][fa2].

In the context of this contract, [positions](#positions) are NFTs and these
entrypoints can be used to manage them.

Note: this contract implements the [Default Transfer Permission Policy](https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#default-transfer-permission-policy).


### **transfer**

Can be used by the `owner` or the `operator` of a [position](#positions) to
transfer its ownership to a different address.

- This entrypoint adheres to the [FA2][fa2] requirements.
- If there is no position associated to the given `token_id`, fails with
  `FA2_TOKEN_UNDEFINED`
- If there used to be a position associated to the given `token_id` but it is closed, fails with
  `FA2_TOKEN_UNDEFINED`
- If an `amount` is `0` it does not fail, but effectively there will be no
  difference.
- If any `amount` is higher than `1`, fails with `FA2_INSUFFICIENT_BALANCE`, as
  NFTs/`position`s are unique.
- If any transfer was not initiated by the `position` `owner` or an allowed
  `operator`, fails with `FA2_NOT_OPERATOR`.

```ocaml
type position_id = nat

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
```

### **balance_of**

This can be used by another contract to query the ownership of a [position](#positions).

- This entrypoint adheres to the [FA2][fa2] requirements.
- If the given `owner` does not own the given `position_id`, the returned `balance`
  will be `0`, otherwise it will be `1`.

```ocaml
type position_id = nat

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
```

### **update_operators**

Updates (adds or removes) `operator`s of a [position](#positions) for the
specified token `owner`.

- This entrypoint adheres to the [FA2][fa2] requirements.
- If two different updates change the same `operator` for the same
  `owner` and `position`, the last update will take effect.
- Operators can be updated for for an `owner` that does not yet own the given
  `position`.
- Operator updates require the `owner` to be the `SENDER`, fails with
  `FA2_NOT_OWNER` otherwise.

```ocaml
type position_id = nat

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
```

## CFMM-specific entrypoints

### **x_to_y**

Perform a token [swap](#swaps) from `x` to `y`.

Note: in order to be able to perform a swap, this contract must be made an
`operator` or be `approve`d (depending if `x` is an FA2 or FA1.2 contract) to
`transfer` the requested amount of tokens.

- The `dx` amount of tokens `x` is `transfer`ed from the `SENDER` account to
  this contract's.
- The [swap fee](#fees) is subtracted from this amount and later awarded to LPs
  with active positions.
- When the `y` token is `CTEZ`, a [protocol fee](#fees) is subtracted
  after converting the `x` tokens to `CTEZ`.
- If the swap is no longer acceptable because the `deadline` was not met, fails
  with `past_deadline_err` error code.
- If less than `min_dy` amount of token `y` would be obtained from the swap, fails
  with `smaller_than_min_asset_err` error code.
- If the amount of `x` tokens would get extremely close to zero, fails with
  `price_out_of_bounds_err` error code. This case is barely possible in normal
  conditions.
- If the swap is successful, the computed converted `y` tokens will be `transfer`red
  to the `to_dy` account.

```ocaml
type x_to_y_param = {
    dx : nat ;
    deadline : timestamp ;
    min_dy : nat ;
    to_dy : address ;
}
```


### **y_to_x**

Perform a token [swap](#swaps) from `y` to `x`.

Analogous to the [`x_to_y`](#x_to_y) entrypoint, with the following caveats:

- When the `y` token is `CTEZ`, a [protocol fee](#fees) is subtracted
  _before_ converting the `CTEZ` tokens to `x` (and after applying the swap fee).

```ocaml
type y_to_x_param = {
    dy : nat ;
    deadline : timestamp ;
    min_dx : nat ;
    to_dx : address ;
}
```


### **x_to_x_prime**

Perform a token [swap](#swaps) across two Segmented-CFMM, from this contract's
`x` to the other's `x` (hereby called `x_prime`).

This entrypoint allows one to make a swap between two tokens that aren't directly
in a pair, but both of which are paired with the same `y`.

Note: in order to be able to perform a swap, this contract must be made an
`operator` or be `approve`d (depending if `x` is an FA1.2 or FA2 contract) to
`transfer` the requested amount of tokens.

- The `dx` amount of tokens `x` is `transfer`ed from the `SENDER` account to
  this contract's.
- The [swap fee](#fees) is subtracted from this amount and later awarded to LPs
  with active positions.
- When the `y` token is `CTEZ`, a [protocol fee](#fees) is subtracted
  after converting the `x` tokens to `CTEZ`.
- If the swap is no longer acceptable because the `deadline` was not met, fails
  with `past_deadline_err` error code.
- Instead of completing the single swap, the `x_prime_contract` will be called.
- If less than `min_dx_prime` amount of token `x_prime` would be obtained from
  the swap second swap, `x_prime_contract` is expected to fail with `UNDER_MIN`.
- If the swap is successful, the computed converted `x_prime` tokens are expected
  to be `transfer`red by the `x_prime_contract` to the `to_dx_prime` account.

```ocaml
type x_to_x_prime_param = {
    dx : nat ;
    x_prime_contract : y_to_x_param contract ;
    deadline : timestamp ;
    min_dx_prime : nat ;
    to_dx_prime : address ;
}
```


### **set_position**

Updates or creates a new [position](#positions) in the given range.

- `lower_tick_index` determines the lowest tick index in which this position will be active.
- `upper_tick_index` determines the highest tick index in which this position will be active.
- `lower_tick_witness` is a witness (already initialized tick) index lower than `lower_tick_index`.
  It should be as close as possible to `lower_tick_index`, for efficiency.
- `upper_tick_witness` is a witness (already initialized tick) index lower than `upper_tick_index`.
  It should be as close as possible to `upper_tick_index`, for efficiency.
- The liquidity of the `SENDER` will be updated by `delta_liquidity`, increased
  or decreased depending on the sign, for both tokens of the pair.
- In case, after adding eventual accrued fees, the `delta_liquidity` is:
  * positive in `x` and/or `y`: this amount will be `transfer`red **from** the `SENDER`
  * negative in `x`: this amount will be `transfer`red **to** `to_x`
  * negative in `y`: this amount will be `transfer`red **to** `to_y`
- If the position update is no longer acceptable because the `deadline` was not
  met, fails with `past_deadline_err` error code
- If the amount of tokens that needs to be `transfer`red to the contract
  (after taking accrued fees into account) is higher than `maximum_tokens_contributed`,
  fails with `high_tokens_err` error code.
- A user can set `delta_liquidity` to `0` on an existing position to simply retrieve
  any uncollected fees.
- This fails when a tick index out of the `[-1048575; 1048575]` range is provided.

```ocaml
type set_position_param = {
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index ;
    lower_tick_witness : tick_index ;
    upper_tick_witness : tick_index ;
    delta_liquidity : int ;
    to_x : address ;
    to_y : address ;
    deadline : timestamp ;
    maximum_tokens_contributed : balance_nat ;
}

type balance_nat = {x : nat ; y : nat}
```


### **get_position_info**

Obtains information about the position with the given identifier.
The used identifier is `token_id` that is used to select the position in `transfer` / `get_balance` FA2 entrypoints.

- `liquidity` is the current amount of liquidity added by this position.
- `lower_tick_index` and `upper_tick_index` are the boundary ticks in which the position is active. These values are constant for the given position.
- `owner` is the current position's owner in accordance to the position's semantics of FA2 asset.
- If the specified position identifier does not exist, fails with `position_not_exist_err` error code.

```ocaml
type position_info = {
    liquidity : nat;
    index : position_index;
}

type position_index = {
    owner : address ;
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index
}

type get_position_info_param =
[@layout:comb]
{
    position_id : position_id;
    callback : position_info contract;
}
```


### **snapshot_cumulatives_inside**

Oracle `view` for a snapshot of the tick cumulative, seconds per liquidity and
seconds inside the given range.

- `lower_tick_index` determines the lowest tick index of the range.
- `upper_tick_index` determines the highest tick index of the range.
- the `callback` contract will be called with the computed values.
- `tick_cumulative_inside` is the computed snapshot of the tick cumulative for
  the given range.
- `seconds_per_liquidity_inside` is the computed snapshot of the seconds per
  liquidity for the given range.
- `seconds_inside` is the computed snapshot of the seconds for the given range.
- This entrypoint accepts only indices of [initialized](#initialized-tick-definition)
  ticks; in case invalid tick index is provided, the call will fail with
  `tick_not_exist_err` error code.

```ocaml
type cumulatives_inside_snapshot = {
    tick_cumulative_inside : int ;
    seconds_per_liquidity_inside : nat ;
    seconds_inside : nat ;
}

type cumulatives_inside_snapshot_param = {
    lower_tick_index : tick_index ;
    upper_tick_index : tick_index ;
    callback : cumulatives_inside_snapshot contract ;
}
```


### **observe**

Oracle `view` for the cumulative tick and liquidity-in-range, calculated for each
`timestamp`s in a list.

- Each value in `times` will be used to calculate a `cumulative_entry`.
  The order of the computed entries is the same as that of the given `timestamp`s.
- The `callback` contract will be called with the computed values.
- For each `timestamp`/`cumulative_entry` pair:
  * `tick_cumulative` is the cumulative tick value at that `timestamp`.
  * `seconds_per_liquidity_cumulative` is the cumulative seconds per
    liquidity-in-range at that `timestamp`.
- The contract stores a fixed number of past observations, with recent observations overwriting the oldest.
  * As a result, if any of the timestamps given in the entrypoint's parameter
  is too far back in the past, the entrypoint fails with `observe_outdated_timestamp_err` error code.
  * Note that the amount of observations stored by the contract can be increased
  via the `increase_observation_count` entrypoint.
- If any of the timestamps given in the entrypoint's parameter is yet in the future, the entrypoint fails with `observe_future_timestamp_err`.

```ocaml
type cumulatives_value = {
    tick_cumulative : int ;
    seconds_per_liquidity_cumulative : nat ;
}

type oracle_view_param = cumulatives_value list

type observe_param =
[@layout:comb]
{
    times : timestamp list ;
    callback : oracle_view_param contract ;
}
```

### **increase_observation_count**

Increase the number of observations of `tick_cumulative` and
`seconds_per_liquidity_cumulative` taken and stored in the contract
by the given number.
The greater the number of tracked observations, the further back in time
users will be able to retrieve data using [`observe`](#observe).

The caller of this entrypoint will pay for the additional storage costs.

```ocaml
type increase_observation_count_param = {
    added_observation_count: nat;
}
```

# Errors

See the [Error Codes](/docs/error-codes.md) file for the list of error codes.

### Design decisions

* The contract stores a fixed number of past [observations](#observe).
  The alternative would be to store an unbound number of observations, which implies users would
  continually pay additional storage costs every time a block is baked, which is not desirable.

 [uniswap-v3]: https://uniswap.org/whitepaper-v3.pdf
 [spot-price]: https://www.investopedia.com/terms/s/spotprice.asp
 [fa1.2]: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-7/tzip-7.md
 [fa2]: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md
 [ctez]: https://github.com/tezos-checker/ctez
