# Segmented CFMM

Implementation of a constant product market making smart-contract, which allows the curve to be defined on price
segments. Based on the ideas described in
https://uniswap.org/whitepaper-v3.pdf.

## Python

The [python](python) directory contains a quick and dirty implementation in Python that makes no effort to implement
integer based arithmetic, in order to ensure that the general logic of the contract is sound. The mutmut mutation
testing framework was used to improve the test coverage.

The [compute_ladders.py](python/compute_ladders.py) script uses Sage to generate some of the constants useful to compute
exponentiations in the contract and verify that the precision obtained is sufficient.

## CameLIGO

The [ligo](ligo) directory contains a [CameLIGO](https://ligolang.org/)
implementation of a smart-contract.

This code is work in progress and should not be expected to run or even compile.