<!--
SPDX-FileCopyrightText: 2021 Arthur Breitman
SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
-->

# Compiling the contract and its storage

This contract is implemented using the [LIGO programming language](https://ligolang.org/),
in particular its CameLIGO dialect.

To compile to Michelson the contract code and/or its storage, you'll need to have
the [ligo executable](https://ligolang.org/docs/intro/installation) installed.

The latest working version tested is [0.16.1](https://gitlab.com/ligolang/ligo/-/releases/0.16.1).

## Compiling the contract

The [`Makefile`](../Makefile) is the easiest way to compile the contract.

You can run:
```sh
make out/segmented_cfmm_default.tz
```
to obtain one using all the default options.

**Note:** these are the smart contract compile-time options.
They cannot be changed after origination.

This is a `make` invocation that uses all of them, with their respective
default values:
```sh
make out/segmented_cfmm_default.tz \
  x_token_type=FA2 \
  y_token_type=CTEZ
```

Please refer to the [compilation options](./specification.md#compilation-options)
section of the specification for an explanation of these options.

## Compiling the contract storage

The [`Makefile`](../Makefile) is the easiest way to compile the contract storage
as well.

You can run:
```sh
make out/storage_default.tz
```
to obtain one using all the default options.

**Note:** these are the smart contract origination-time options.
They cannot be changed after origination.

This is a `make` invocation that uses all of them, with their respective
default values:
```sh
make out/storage_default.tz \
  fee_bps=10 \
  ctez_burn_fee_bps=5 \
  x_token_id=0 \
  y_token_id=0 \
  x_token_address=KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn \
  y_token_address=KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn
```

At the very least, you'll probably want to change the tokens' contract address.

If you don't want to use the `Makefile`, you can use the `ligo compile-storage`
command. The `default_storage` is located in the [`defaults` LIGO module](../ligo/defaults.mligo).