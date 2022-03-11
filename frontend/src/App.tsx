// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import * as RD from "@devexperts/remote-data-ts"
import { ContractAbstraction, ContractProvider, TezosToolkit, TransactionWalletOperation } from '@taquito/taquito'
import { cmd } from 'elm-ts/lib'
import * as Html from 'elm-ts/lib/React'
import { attempt, perform } from "elm-ts/lib/Task"
import * as A from "fp-ts/lib/Array"
import * as E from 'fp-ts/lib/Either'
import { identity, pipe } from "fp-ts/lib/function"
import * as M from "fp-ts/lib/Map"
import * as O from 'fp-ts/lib/Option'
import * as S from "fp-ts/lib/string"
import * as React from 'react'
import { testnetLink, mainnetLink } from './Generated/Env';
import { increaseObservationCountFormInit, setPositionFormInit, transferFormInit, updateOperatorsFormInit, updatePositionFormInit, xToXPrimeFormInit, xToYFormInit, yToXFormInit } from './Data/Form'
import * as FH from './Shared/FormHelper'
import { callEntrypoint, connectWallet, findContract, getStorage, getWalletAddress } from './Shared/Network'
import { ContractInfo, EntrypointTypes, Model, NetworkTypes } from './Type'

// ------------------------------------------------------
// Model
// ------------------------------------------------------

export const init = (): [Model, cmd.Cmd<Msg>] => {
  let model: Model = {
    appName: 'CFMM',
    contract: RD.initial,
    currentNet: 'Granada',

    // Result
    callEntrypointResult: RD.initial,
    addressInput: "",
    selectedEntrypoint: { type: "x_to_y" },
    isWalletConnected: false,
    userAddress: O.none,
    tezos: new TezosToolkit(testnetLink),
    storage: RD.initial,

    // shared
    xToYForm: { formMap: xToYFormInit() },
    yToXForm: { formMap: yToXFormInit() },
    setPositionForm: { formMap: setPositionFormInit() },
    updatePositionForm: { formMap: updatePositionFormInit() },
    transferForm: { formMap: transferFormInit() },
    updateOperatorsForm: { formMap: updateOperatorsFormInit() },
    xToXPrimeForm: { formMap: xToXPrimeFormInit() },
    increaseObservationCountForm: { formMap: increaseObservationCountFormInit() },
  }

  return [
    model,
    cmd.batch([
      connectWalletCmd(model),
      // findContractCmd(model, "KT1XMaGzMc7iGVkbnCC9gZyjs2b85A3NxJTz") // Find contract automatically when development
    ])
  ]
}


// ------------------------------------------------------
// Update
// ------------------------------------------------------

export type Msg
  = { type: 'ConnectWallet' }
  | { type: 'ConnectWalletResult', result: E.Either<string, TezosToolkit> }
  | { type: 'GetUserAddressResult', result: string }
  | { type: 'ContractAddressInput', input: string }
  | { type: 'FindContract' }
  | { type: 'FindContractResult', result: E.Either<any, ContractInfo> }
  | { type: 'GetStorage' }
  | { type: 'GetStorageResult', result: E.Either<any, any> }
  | { type: 'CallEntrypointResult', result: E.Either<any, TransactionWalletOperation> }
  | { type: 'ChangeForm', value: EntrypointTypes }
  | { type: 'FormHelperMsg', entrypoint: EntrypointTypes, value: FH.Msg }
  | { type: 'ChangeNet', value: NetworkTypes }


export function update(msg: Msg, model: Model): [Model, cmd.Cmd<Msg>] {
  console.log(msg)
  switch (msg.type) {
    case 'ConnectWallet':
      return [model, connectWalletCmd(model)]
    case 'ConnectWalletResult':
      console.log("ConnectWalletResult result", msg.result)
      // Make to_dy and to_dx with user address as default
      if (msg.result._tag === "Right") {
        return [
          {
            ...model,
            isWalletConnected: true,
            tezos: msg.result.right
          }
          , getWalletAddressCmd(model)
        ]
      }
      else {
        console.log("Cannot connect to wallet:", msg.result.left)
        return [model, cmd.none]
      }

    case 'GetUserAddressResult':
      // console.log("GetUserAddressResult result", msg.result)
      model.xToYForm.formMap = pipe(
        model.xToYForm.formMap,
        M.modifyAt(S.Ord)("to_dy", n => ({ ...n, value: msg.result })),
        O.fold(() => { throw new Error("GetUserAddressResult: impossbile") }, identity)
      )
      model.yToXForm.formMap = pipe(
        model.yToXForm.formMap,
        M.modifyAt(S.Ord)("to_dx", n => ({ ...n, value: msg.result })),
        O.fold(() => { throw new Error("GetUserAddressResult: impossbile") }, identity)
      )


      return [{ ...model, userAddress: O.some(msg.result) }, cmd.none]

    case 'ContractAddressInput':
      console.log(msg.input)
      return [{ ...model, addressInput: msg.input }, cmd.none]
    case 'FindContract':
      return [
        {
          ...model,
          contract: RD.pending
          // wallet: pipe(model.wallet, O.map(w => ({ ...w, contract: RD.pending })))
        }
        , findContractCmd(model, model.addressInput)
      ]

    case 'FindContractResult': {
      console.log("Contract Info", msg.result)
      const contract = pipe(msg.result, E.fold(RD.failure, r => RD.success(r)))
      const newModel = {
        ...model,
        contract,
        storage: RD.pending
      }
      return [
        newModel,
        getStorageIfContractExist(newModel)
      ]
    }
    case 'GetStorage': {
      const newModel = {
        ...model,
        storage: RD.pending
      }
      return [
        newModel,
        getStorageIfContractExist(newModel)
      ]
    }
    case 'GetStorageResult':
      let getStorageResult = pipe(msg.result, E.fold(RD.failure, r => RD.success(r)))

      return [
        {
          ...model,
          storage: getStorageResult
        }
        , cmd.none
      ]

    case 'CallEntrypointResult':
      console.log(msg.result)
      let callEntrypointResult = pipe(msg.result, E.fold(RD.failure, r => RD.success(r)))

      return [
        {
          ...model,
          callEntrypointResult
        }
        , cmd.none
      ]
    case 'ChangeForm':
      return [{ ...model, selectedEntrypoint: msg.value }, cmd.none]
    case 'ChangeNet':
      let tezosLink = msg.value === 'Mainnet' ? mainnetLink : testnetLink
      let tezos = new TezosToolkit(tezosLink)
      let currentNet = msg.value;
      return [{ ...model, tezos, currentNet }, cmd.none]

    case 'FormHelperMsg':
      const formModel = getModelFromEntrypoint(model, msg.entrypoint)
      const [newFormModel, itsMsg] = FH.update(msg.value, formModel)
      let newModel = updateModelFromEntrypoint(model, msg.entrypoint, newFormModel)
      if (itsMsg.type === "FormComplete") {
        return [
          { ...newModel, callEntrypointResult: RD.pending },
          attempt
            ((result: E.Either<string, TransactionWalletOperation>) => ({ type: 'CallEntrypointResult', result }))
            (callEntrypoint(newModel, itsMsg.contractInfo, msg.entrypoint, formModel.formMap))
        ]
      } else return [newModel, cmd.none]
  }
}

// ------------------------------------------------------
// Helper
// ------------------------------------------------------

const getModelFromEntrypoint = (model: Model, ep: EntrypointTypes): FH.Model => {
  if (ep.type === "x_to_y")
    return model.xToYForm
  else if (ep.type === "y_to_x")
    return model.yToXForm
  else if (ep.type === "set_position")
    return model.setPositionForm
  else if (ep.type === "update_position")
    return model.updatePositionForm
  else if (ep.type === "transfer")
    return model.transferForm
  else if (ep.type === "update_operators")
    return model.updateOperatorsForm
  else if (ep.type === "x_to_x_prime")
    return model.xToXPrimeForm
  else if (ep.type === "increase_observation_count")
    return model.increaseObservationCountForm
  else throw new Error("getModelFromEntrypoint: Entrypoint does not exist.")
}
const updateModelFromEntrypoint = (model: Model, ep: EntrypointTypes, newForm: FH.Model): Model => {
  if (ep.type === "x_to_y")
    return { ...model, xToYForm: newForm }
  else if (ep.type === "y_to_x")
    return { ...model, yToXForm: newForm }
  else if (ep.type === "set_position")
    return { ...model, setPositionForm: newForm }
  else if (ep.type === "update_position")
    return { ...model, updatePositionForm: newForm }
  else if (ep.type === "transfer")
    return { ...model, transferForm: newForm }
  else if (ep.type === "update_operators")
    return { ...model, updateOperatorsForm: newForm }
  else if (ep.type === "x_to_x_prime")
    return { ...model, xToXPrimeForm: newForm }
  else if (ep.type === "increase_observation_count")
    return { ...model, increaseObservationCountForm: newForm }
  else throw new Error("getModelFromEntrypoint: Entrypoint does not exist.")
}

const getStorageIfContractExist = (model: Model) => {
  if (model.contract._tag === "RemoteSuccess")
    return getStorageCmd(model, model.contract.value)
  else return cmd.none
}

// ------------------------------------------------------
// Cmd
// ------------------------------------------------------

function connectWalletCmd(model: Model): cmd.Cmd<Msg> {
  return attempt
    ((result: E.Either<string, TezosToolkit>) => ({ type: 'ConnectWalletResult', result }))
    (connectWallet(model))
}

function getWalletAddressCmd(model: Model): cmd.Cmd<Msg> {
  return perform
    ((result: string) => ({ type: 'GetUserAddressResult', result }))
    (getWalletAddress(model))
}

function findContractCmd(model: Model, addr: string): cmd.Cmd<Msg> {
  return attempt
    ((result: E.Either<string, ContractAbstraction<ContractProvider>>) => ({ type: 'FindContractResult', result }))
    (findContract(model, addr))
}

function getStorageCmd(model: Model, contractInfo: ContractInfo) {
  return attempt
    ((result: E.Either<string, TransactionWalletOperation>) => ({ type: 'GetStorageResult', result }))
    (getStorage(model, contractInfo))
}

// ------------------------------------------------------
// View
// ------------------------------------------------------

const Navbar = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {

  const nets: NetworkTypes[] = ['Mainnet', 'Granada']

  const NetworkItem = (props: { a: NetworkTypes, model: Model, dispatch: React.Dispatch<Msg> }) => {
    let isCurrent = props.model.currentNet === props.a ? "bg-blue-200 rounded" : ""
    return <div className={"cursor-pointer p-2 " + isCurrent} onClick={() => props.dispatch({ type: 'ChangeNet', value: props.a })}>{props.a}</div>
  }

  return <div className="bg-white flex justify-between items-center w-full">
    <div className="flex items-center">
      <div className="px-3 py-2"><i className="text-2xl fa fa-credit-card" aria-hidden="true"></i></div>
      <div>Segmented CFMM</div>
    </div>
    <div className="flex items-center">
      <div className="text-sm text-blue-600 flex">

        {pipe(nets, A.mapWithIndex((i, a) => <NetworkItem key={i} a={a} dispatch={props.dispatch} model={props.model} />))}
      </div>

      <div onClick={() => props.dispatch({ type: 'ConnectWallet' })} className="px-4 text-sm">Connect to wallet</div>
    </div>

  </div>
}

const ContractFinder = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {
  let isDisabled = props.model.isWalletConnected ? 'opacity-100 pointer-events-auto' : 'opacity-50 pointer-events-none';
  let SubmitButton = () => {
    if (props.model.contract._tag === "RemotePending")
      return <button
        className="block rounded-xl bg-blue-500 text-white text-sm px-3 py-2">
        Loading
      </button>

    else return <button
      onClick={() => props.dispatch({ type: 'FindContract' })}
      className="block rounded-xl bg-blue-500 text-white text-sm px-3 py-2">
      Submit
      </button>
  }
  let ErrorText = () => {
    if (props.model.contract._tag === "RemoteFailure")
      return <div className="py-2">
        <p className="text-sm text-red-600">{JSON.stringify(props.model.contract.error)}</p>
      </div>
    else return <div></div>
  }
  return <div className={"bg-white rounded-2xl mt-10 p-6 m-auto " + isDisabled} style={{ "width": "500px" }}>
    <div className="text-sm">Enter an address of a Segmented CFMM contract</div>
    <p className="text-xs pt-2 text-gray-500"> For example: KT1XMaGzMc7iGVkbnCC9gZyjs2b85A3NxJTz</p>
    <div className="mt-4">
      <input
        onChange={e => props.dispatch({ type: 'ContractAddressInput', input: e.target.value })}
        className="bg-gray-100 w-full rounded-lg p-4"
        type="text" value={props.model.addressInput} />
    </div>
    <ErrorText />
    <div className="mt-2 flex w-full">
      <div style={{ flexGrow: 1 }}></div>

      <SubmitButton />
    </div>
  </div>
}

export function view(model: Model): Html.Html<Msg> {
  return dispatch => (
    <div className="max-w-screen min-h-screen flex flex-col bg-blue-100 items-center text-gray-700 pb-60">
      <Navbar model={model} dispatch={dispatch} />
      <div className="w-8/12">
        <ContractFinder model={model} dispatch={dispatch} />
        <EntrypointForms model={model} dispatch={dispatch} />
      </div>

    </div>

  )
}


const MkFormView = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {

  const formView: Html.Html<Msg> = pipe(
    FH.view(getModelFromEntrypoint(props.model, props.model.selectedEntrypoint), props.model.contract, props.model.callEntrypointResult),
    Html.map(value => ({ type: 'FormHelperMsg', entrypoint: props.model.selectedEntrypoint, value }))
  )
  return formView(props.dispatch)
}

const Form = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {
  return <MkFormView model={props.model} dispatch={props.dispatch} />

}

let ResultText = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {
  if (props.model.callEntrypointResult._tag === "RemoteFailure")
    return <div className="py-2">
      <p className="text-sm text-red-600">{JSON.stringify(props.model.callEntrypointResult.error)}</p>
    </div>
  else if (props.model.callEntrypointResult._tag === "RemoteSuccess")
    return <div className="py-2">
      <p className="text-sm text-green-600">Success! Operation hash: {props.model.callEntrypointResult.value.opHash}</p>
    </div>
  else return <div></div>
}


export const EntrypointForms = (props: { model: Model, dispatch: React.Dispatch<Msg> }) => {

  const isDisabled = (): string => {
    if (props.model.contract._tag === "RemoteSuccess") return ' opacity-100 pointer-events-auto'
    else return ' opacity-50 pointer-events-none'
  }

  const allEntrypoints: Array<[string, EntrypointTypes]> = [
    ["Swap X for Y", { type: 'x_to_y' }],
    ["Swap Y for X", { type: 'y_to_x' }],
    ["Set position", { type: 'set_position' }],
    ["Update position", { type: 'update_position' }],
    ["Transfer position", { type: 'transfer' }],
    ["Update operators", { type: 'update_operators' }],
    ["Swap X for X'", { type: 'x_to_x_prime' }],
    ["Increase observation count", { type: 'increase_observation_count' }],
  ]

  let ContractStorageView = () => {
    if (props.model.storage._tag === "RemoteSuccess")
      return <div>
        <p className="pb-1"><span className="font-bold">Price:</span> {Math.pow(props.model.storage.value.sqrt_price.toString(), 2)}</p>
        <p className="pb-1"><span className="font-bold">Liquidity:</span> {props.model.storage.value.liquidity.toString()}</p>
        <p className="pb-1"><span className="font-bold">Token X ID:</span> {props.model.storage.value.constants.x_token_id.toString()}</p>
        <p className="pb-1"><span className="font-bold">X Address:</span> {props.model.storage.value.constants.x_token_address.toString()}</p>
        <p className="pb-1"><span className="font-bold">Token Y ID:</span> {props.model.storage.value.constants.y_token_id.toString()}</p>
        <p className="pb-1"><span className="font-bold">Y Address:</span> {props.model.storage.value.constants.y_token_address.toString()}</p>
        <p className="pb-1"><span className="font-bold">Swap Fee BPS:</span> {props.model.storage.value.constants.fee_bps.toString()}</p>
        <p className="pb-1"><span className="font-bold">CTEZ Burn Fee BPS:</span> {props.model.storage.value.constants.ctez_burn_fee_bps.toString()}</p>
      </div>
    else return <div></div>
  }

  let RefreshButtonView = () => {
    if (props.model.storage._tag === "RemotePending")
      return <button
        className="block rounded-xl bg-blue-500 text-white text-sm px-3 py-2">
        Loading
      </button>
    else if (props.model.storage._tag === "RemoteInitial")
      return <div></div>
    else
      return <button
        className="block rounded-xl bg-blue-500 text-white text-sm px-3 py-2"
        onClick={() => props.dispatch({ type: 'GetStorage' })}>
        Refresh
      </button>
  }

  return <div>
    <div className={"bg-white rounded-2xl mt-10 m-auto p-6 " + isDisabled()} style={{ width: "500px" }}>
      <p className="font-bold pb-1 text-lg">Current Contract Info</p>
      <div className="py-2">
        <ContractStorageView />
      </div>
      <div className="py-2">
        <RefreshButtonView />
      </div>
    </div>

    <div className={"bg-white rounded-2xl mt-10 m-auto " + isDisabled()} style={{ width: "500px" }}>

      <div className="relative">
        <div className="absolute top-0 bg-white py-4 rounded-lg" style={{ "left": "-260px" }}>
          {pipe(allEntrypoints, A.mapWithIndex((i, a) => <FormSelect key={i} a={a} dispatch={props.dispatch} model={props.model} />))}
        </div>
      </div>
      <div className="p-6">
        <Form model={props.model} dispatch={props.dispatch} />
        <ResultText model={props.model} dispatch={props.dispatch} />
      </div>

    </div>
  </div>


}

export const FormSelect = (props: { a: [string, EntrypointTypes], model: Model, dispatch: React.Dispatch<Msg> }) => {
  const [label, value] = props.a
  return <div
    className="px-4 py-2 hover:bg-gray-200 cursor-pointer"
    onClick={() => props.dispatch({ type: 'ChangeForm', value })}
  >
    {label}</div>
}

