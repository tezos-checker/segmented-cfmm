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

Furthermore, when the `y` token is [`ctez`][ctez], a _protocol fee_ is subtracted from the `ctez`
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

In order to do this, when the user stakes their position, the contract may use the [`snapshot_cumulatives_inside`](#snapshot_cumulatives_inside) view entrypoint
to obtain `seconds_per_liquidity_cumulative_t0` and keep a record of it.

Later, when the user unstakes their position, the contract calls the same view entrypoint once more to obtain
`seconds_per_liquidity_cumulative_t1`.
It can then apply the following formula to calculate how many `R` tokens to reward the user with,
where `position_liquidity` is the amount of liquidity provided by the user's position.

```
rate * position_liquidity * (seconds_per_liquidity_cumulative_t1 - seconds_per_liquidity_cumulative_t0)
```

 [uniswap-v3]: https://uniswap.org/whitepaper-v3.pdf
 [spot-price]: https://www.investopedia.com/terms/s/spotprice.asp
 [ctez]: https://github.com/tezos-checker/ctez
