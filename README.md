# Segmented CFMM

Implementation of a constant product market making smart-contract, which allows
the curve to be defined on price segments.

Based on the ideas described in the [Uniswap V3 whitepaper](https://uniswap.org/whitepaper-v3.pdf).

## Python

The [python](python) directory contains a quick and dirty implementation in Python that makes no effort to implement
integer based arithmetic, in order to ensure that the general logic of the contract is sound. The mutmut mutation
testing framework was used to improve the test coverage.

The [compute_ladders.py](python/compute_ladders.py) script uses Sage to generate some of the constants useful to compute
exponentiations in the contract and verify that the precision obtained is sufficient.

## CameLIGO

The [ligo](ligo) directory contains a [CameLIGO](https://ligolang.org/)
implementation of a smart-contract.

For instructions on how to compile the contract and/or its storage, please refer
to the [compilation document](docs/compilation.md).


## Generating a contract metadata

The contract metadata should be compliant with [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md).

You can generate the metadata, but you'll need the `stack` tool installed, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial for instructions on how to obtain it.

```bash
make metadata \
    x_token_symbol=X x_token_name="Token X" x_token_decimals=1 \
    y_token_symbol=Y y_token_name="Token Y" y_token_decimals=1 \
    output=metadata.json
```
You can then modify the produced `metadata.json` if you want to add more information.

This metadata should be stored:
- In the storage field `metadata_map`
- On IPFS: [Get started with IPFS](https://ipfs.io/)
- As a separate contract: [The `tezos-storage` URI Scheme](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#the-tezos-storage-uri-scheme)


To use this metadata, follow [TZIP-16 Contract storage](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#contract-storage)

Specifically:
> The encoding of the values must be the direct stream
of bytes of the data being stored. For instance, an URI starting with `http:`
will start with the 5 bytes `0x687474703a` (`h` is `0x68`, `t` is `0x74`,
etc.). There is no implicit conversion to Michelson's binary format (`PACK`) nor
quoting mechanism.

After the conversion, the bytes can be use in the field `metadata` when originating the contract.

## Error Codes

In error scenarios, the contract fails with an error code.

See the [Error Codes](docs/error-codes.md) file for the list of error codes.
