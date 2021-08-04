# Segmented CFMM Typescript API

This package implements a Typescript interface to interact with the Segmented CFMM
contract.

## Build instructions

You will need the `yarn` package manager to build this library. It can be built
as follows.

```sh
yarn install
yarn tsc
```

### Generating Typescript interface types

The Typescript types representing the Segmented CFMM contract parameter can be generated
automatically by using the `make typescript` command.

You will need the `stack` tool installed, see [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) tutorial for instructions on how to obtain it.

The files will be saved to the [/src/generated](src/generated) directory.

## Usage

To add this library as a dependency to your typescript app, you can use `yarn add` by specifiying
path to it as follows.


```sh
yarn add file:/path-to-this-repo/typescript/segmented-cfmm/
```

Then you will need the contract address of the Segmented CFMM contract and the secret key
of the sender to start using this library. A sample usage can be seen in the snippets below.

```
import { SegCFMMContract } from "segmented-cfmm-sdk";

let sender_secret_key : string = 'edsk36njbEALasoBeisebfAZgXXMECPL3xt9KLuiQqGB3tEFZrMq7e';
let contract_address : string =  'KT1VjT5AiAkek74kUyS5t41dmTM3abGTv9Kc';

const SegCFMM = new SegCFMMContract
  ( 'https://mainnet-tezos.giganode.io'
  , sender_secret_key
  , contract_address
  );

// call some entrypoints

SegCFMM
  .x_to_x_prime("tz1ZvZCqjaBLEyJLYHCfzq6B8MCFu2SRjzJG")
  .catch(err => console.log(JSON.stringify(err), null, 2));

```

### Contract validation

The target Segmented CFMM contract's storage and parameter can be validated against a known schema to make sure that
we are talking to a compatible contract.

You can obtain a parameter and storage schema with:

```
let contractStorageSchema = await SegCFMM.storageSchema();
let contractParameterSchema = await SegCFMM.parameterSchema();
```
and you can verify a contract schema with:

```
SegCFMM.ensureStorageSchema(contractStorageSchema).catch(err => console.log(err));
SegCFMM.ensureParamSchema(contractParameterSchema).catch(err => console.log(err));
```

If the schemas does not match, the actual schema that was found will be included as part of the
error.
