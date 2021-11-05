# Segmented CFMM Frontend

A frontend application used to communicate with any Segmented CFMM contract.


## Pre-requisite

To communicate with the webapp, the user should install [Temple wallet](https://templewallet.com/).

## How to run locally


```bash
# See the Makefile for more details
make frontend \
  env=development \
  testnet_url=https://granada.testnet.tezos.serokell.team/ \
  mainnet_url=https://mainnet.api.tez.ie/
```

## Limitation

The web app does not support all the entrypoints, particularly the entrypoints that need to be called by a contract.

## Known Issues

```
error:0308010C:digital envelope routines::unsupported
```

If you encounter this error, this is due to webpack not working well with the nodejs 17. To solve this, you can set this env variable:

```
export NODE_OPTIONS=--openssl-legacy-provider
```

More details check this thread: https://github.com/webpack/webpack/issues/14532
