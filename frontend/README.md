# Segmented CFMM Frontend

A frontend application used to communicate with any Segmented CFMM contract.


## Pre-requisite

To communicate with the webapp, the user should install the [Temple wallet](https://templewallet.com/).

## How to run locally

The webapp can be built and run locally by invoking:
```bash
make frontend \
  env=development \
  testnet_url=https://granada.testnet.tezos.serokell.team/ \
  mainnet_url=https://mainnet.api.tez.ie/
```
Please refer to the Makefile for more details about the options.

## Limitations

The web app does not support all the contract entrypoints, the ones that need to
be called by a contract are excluded.

## Known Issues

If you encounter this error:
```
error:0308010C:digital envelope routines::unsupported
```
it is due to webpack not working well with nodejs 17.

To solve this, you can set this env variable:
```
export NODE_OPTIONS=--openssl-legacy-provider
```

For more details, see this thread: https://github.com/webpack/webpack/issues/14532
