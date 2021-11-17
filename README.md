# Segmented CFMM

Implementation of a constant product market making smart-contract, which allows
the curve to be defined on price segments.

Based on the ideas described in the [Uniswap V3 whitepaper](https://uniswap.org/whitepaper-v3.pdf).

## CameLIGO

The [ligo](ligo) directory contains a [CameLIGO](https://ligolang.org/)
implementation of the smart-contract, which adhears to the
[contract specification](docs/specification.md).

For instructions on how to compile the contract and/or its storage, please refer
to the [compilation document](docs/compilation.md).

### Error Codes

In error scenarios, the contract fails with an error code.

See the [error codes document](docs/error-codes.md) for the list of possible
error codes and their description.

### Liquidity mining

An example implementation of a liquidity mining contract is also included, see
the [dedicated document](docs/liquidity-mining.md) for more info.

## Frontend

A frontend application is included to interact with a deployed instance of
the contract.

For more information and instructions on how to run it locally, please refer to
[its documentation](frontend/README.md).

## Testing

This repository includes several property and unit tests, implemented with the
[cleveland test framework](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland)
in the [haskell/test](haskell/test) directory.

In order to run these tests you'll need the `stack` tool installed, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
tutorial for instructions on how to obtain it.

With that, the simplest way to run these tests is to execute the
```bash
make test
```
command.

## Python

The [python](python) directory contains a quick and dirty implementation in Python that makes no effort to implement
integer based arithmetic, in order to ensure that the general logic of the contract is sound. The mutmut mutation
testing framework was used to improve the test coverage.

The [compute_ladders.py](python/compute_ladders.py) script uses Sage to generate some of the constants useful to compute
exponentiations in the contract and verify that the precision obtained is sufficient.
