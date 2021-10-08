# Segmented CFMM Frontend

A frontend application used to communicate with any Segmented CFMM contract.


## Pre-requisite

To communicate with the webapp, the user should install [Temple wallet](https://templewallet.com/).

## How to run locally


```bash
cd frontend
yarn # install dependencies
yarn tailwind # generate required css
yarn dev # run a development server
```

## Limitation

The web app does not support all the entrypoints, particularly the entrypoints that are needed to be called by a contract.