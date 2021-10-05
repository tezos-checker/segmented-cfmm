// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import { RemoteData } from "@devexperts/remote-data-ts"
import { ContractAbstraction, ContractProvider, TezosToolkit, TransactionWalletOperation } from '@taquito/taquito'
import * as O from 'fp-ts/lib/Option'
import * as FH from './Shared/FormHelper'


// ------------------------------------------------------
// Model
// ------------------------------------------------------

export type ContractInfo = ContractAbstraction<ContractProvider>

export type Model = {
  appName: string,
  currentNet: NetworkTypes,
  contract: RemoteData<string, ContractInfo>,
  callEntrypointResult: RemoteData<string, TransactionWalletOperation>,

  // input
  addressInput: string,

  // form
  selectedEntrypoint: EntrypointTypes,


  // taquito state
  isWalletConnected: boolean,
  tezos: TezosToolkit,
  userAddress: O.Option<string>,

  // shared form
  xToYForm: FH.Model,
  yToXForm: FH.Model,
  setPositionForm: FH.Model,
  transferForm: FH.Model,
  updateOperatorsForm: FH.Model,
  xToXPrimeForm: FH.Model,
  increaseObservationCountForm: FH.Model,
}


export type NetworkTypes = 'Granada' | 'Mainnet'

export type EntrypointTypes
  = { type: 'x_to_y' }
  | { type: 'y_to_x' }
  | { type: 'set_position' }
  | { type: 'transfer' }
  | { type: 'update_operators' }
  | { type: 'x_to_x_prime' }
  | { type: 'increase_observation_count' }
