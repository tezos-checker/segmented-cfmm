<!--
SPDX-FileCopyrightText: 2021 Arthur Breitman
SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
-->

# Compiling the contract and its storage

This contract is implemented using the [LIGO programming language](https://ligolang.org/),
in particular its CameLIGO dialect.

To compile to Michelson the contract code and/or its storage, you'll need to have
the [ligo executable](https://ligolang.org/docs/intro/installation) installed.

The latest working version tested is [0.30.0](https://gitlab.com/ligolang/ligo/-/releases/0.30.0).

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

Please refer to the [compilation options](./specification.md#configuration-options)
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
  y_token_address=KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn \
  tick_spacing=1 \
  init_cumulatives_buffer_extra_slots=0 \
  metadata_map="(Big_map.empty : metadata_map)"
```

At the very least, you'll probably want to change the tokens' contract address.

If you don't want to use the `Makefile`, you can use the `ligo compile-storage`
command. The `default_storage` is located in the [`defaults` LIGO module](../ligo/defaults.mligo).

### Generating the contract metadata

You can generate the `metadata_map` compliant with [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md)
to be used in the storage via the [`Makefile`](../Makefile) as well.

However, to do so you'll need the `stack` tool installed, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
tutorial for instructions on how to obtain it.

This is a `make` invocation with the default values for its options:
```
make out/metadata_map \
  x_token_symbol=x \
  x_token_name="Token X" \
  x_token_decimals=1 \
  y_token_symbol=y \
  y_token_name="Token Y" \
  y_token_decimals=1
```
Which you can then include in the storage by using the `metadata_map` option,
for example:
```
make out/storage_default.tz \
    metadata_map="$(cat out/metadata_map)"
```

Alternatively, you can also produce the metadata in JSON format with:
```bash
make out/metadata.json \
  x_token_symbol=x \
  x_token_name="Token X" \
  x_token_decimals=1 \
  y_token_symbol=y \
  y_token_name="Token Y" \
  y_token_decimals=1
```
that can then be modified `metadata.json` with more information.

To use this metadata, follow the [TZIP-16 Contract storage](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#contract-storage)
documentation, in particular:
> The encoding of the values must be the direct stream
of bytes of the data being stored. For instance, an URI starting with `http:`
will start with the 5 bytes `0x687474703a` (`h` is `0x68`, `t` is `0x74`,
etc.). There is no implicit conversion to Michelson's binary format (`PACK`) nor
quoting mechanism.
