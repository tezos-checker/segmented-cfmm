# Segmented CFMM Tests and utilities

This folder includes:
* a test suite, implemented with the
[cleveland test framework](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland)
in the [./test](./test) directory
* a haskell CLI application with a few useful commands


## Running the tests

In order to run these tests you'll need the `stack` tool installed, see
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
tutorial for instructions on how to install it.

The most straightforward way to run them is to execute the following command at the repository's root:

```bash
# Run emulator tests
make test
```

This will run all tests, example-based and property-based, on the morley emulator.

### Network tests

To run network tests as well, use:

```bash
# Run emulator and network tests
make test TEST_ARGUMENTS='--nettest-mode all'

# Run network tests only
make test TEST_ARGUMENTS='--nettest-mode only-network'
```

Network tests:
* require having [`tezos-client`](https://github.com/serokell/tezos-packaging) installed and;
* assume an implicit account with alias "nettest" has been registered and has sufficient tez to run the tests.

They will be run against whatever chain `tezos-client` is configured to use.
To use a different chain, use the `--nettest-node-endpoint`/`-E` option.

For more configuration options, see `make test TEST_ARGUMENTS='--help'` .

## CLI App

This app contains some commands used to generate the contract's TZIP-16 metadata and typescript bindings.
The app is only used internally to implement other `MAKE` commands
(described [here](../README.md) and [here](../typescript/segmented-cfmm/README.md))
and end-users shouldn't need to use it directly.
