// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import * as RD from "@devexperts/remote-data-ts"
import { TransactionWalletOperation } from '@taquito/taquito'
import { Html } from 'elm-ts/lib/React'
import * as A from "fp-ts/lib/Array"
import * as M from "fp-ts/lib/Map"
import * as O from "fp-ts/lib/Option"
import { pipe } from "fp-ts/lib/pipeable"
import { contramap } from "fp-ts/lib/Ord"
import * as S from "fp-ts/lib/string"
import * as N from 'fp-ts/number'
import * as React from 'react'
import { FormItem } from '../Data/Form'
import { ContractInfo } from "../Type"

export type Model = {
  formMap: Map<string, FormItem>
}

export const init: Model = { formMap: new Map([]) }

export type Msg
  = { type: 'ValidateForm', value: ContractInfo }
  | { type: 'InputForm', key: string, value: string }

export type OutputMsg
  = { type: 'None' }
  | { type: 'FormComplete', contractInfo: ContractInfo }


export const update = (msg: Msg, model: Model): [Model, OutputMsg] => {
  console.log(msg)
  switch (msg.type) {
    case 'ValidateForm':
      const newFormMap: Map<string, FormItem> = pipe(
        model.formMap,
        M.map(val => {
          const validatedResult = pipe(
            val.validationFuncs,
            A.reduce([], (init: string[], f) => {
              const result = f(val.value)
              if (result._tag === "Some") return init.concat([result.value])
              else return init
            })
          )
          return { ...val, validatedState: { type: "ValidateComplete", errors: validatedResult } }
        }
        ))

      const isFormValid = pipe(
        newFormMap,
        M.reduce(S.Ord)([], (init: string[], val) => {
          if (val.validatedState.type === "ValidateComplete") {
            return init.concat(val.validatedState.errors)
          } else return []
        }),
        (errAmt) => errAmt.length === 0
      )
      if (isFormValid) return [{ ...model, formMap: newFormMap }, { type: 'FormComplete', contractInfo: msg.value }]
      else return [{ ...model, formMap: newFormMap }, { type: 'None' }]
    case 'InputForm':
      let formMap = pipe(
        model.formMap,
        M.modifyAt(S.Ord)(msg.key, n => ({ ...n, value: msg.value })),
        O.fold(() => { throw new Error("impossbile") }, a => a)
      )
      return [{ ...model, formMap }, { type: 'None' }]

  }
}

let InputType = (props: { item: [string, FormItem], dispatch: React.Dispatch<Msg> }) => {
  let [key, val] = props.item
  if (val.formType.type === "RadioForm")
    return <div>{
      pipe(
        val.formType.values, A.mapWithIndex((i, [label, value]) =>
          <div key={i} onClick={() => props.dispatch({ type: 'InputForm', key, value })} >
            <input type="radio"
              checked={val.value === value}
              value={value}
              onChange={_ => _}
            />
            <label className="pl-2">{label}</label>
          </div>
        ))}
    </div>

  else return <input
    className="w-full rounded p-2 bg-gray-100 border-2 border-gray-300" placeholder={val.placeholder}
    onChange={e => props.dispatch({ type: 'InputForm', key, value: e.target.value })}
    value={val.value}
  />

}

const FormItemView = (props: { item: [string, FormItem], dispatch: React.Dispatch<Msg> }) => {
  let [key, val] = props.item
  let ErrorText = () => {
    if (val.validatedState.type === "ValidateComplete")
      return <p>
        {pipe(
          val.validatedState.errors,
          A.mapWithIndex((i, item) =>
            <span key={i} className="text-sm text-red-500 pr-2">{item}</span>
          ))
        }
      </p>

    else return <div></div>
  }


  return <div>
    <p className="py-2 tooltip">
      {val.label}
      <span className="tooltip_text text-xs py-1 px-2">{val.desc}</span>
    </p>
    <InputType item={props.item} dispatch={props.dispatch} />
    <ErrorText />
  </div>
}

export function view(model: Model, contractInfo: RD.RemoteData<string, ContractInfo>, callEntrypointResult: RD.RemoteData<string, TransactionWalletOperation>): Html<Msg> {
  const byOrder = pipe(
    N.Ord,
    contramap((i: [string, FormItem]) => i[1].order)
  )
  return dispatch => (
    <div>
      {pipe(
        model.formMap,
        M.toArray(S.Ord),
        A.sortBy([byOrder]),
        A.mapWithIndex((i, item) =>
          <FormItemView key={i} item={item} dispatch={dispatch} />
        ))
      }
      <div className="pt-4">
        <CallEntrypointButton contractInfo={contractInfo} model={model} dispatch={dispatch} callEntrypointResult={callEntrypointResult} />
      </div>
    </div>
  )
}

const CallEntrypointButton = (props: { model: Model, contractInfo: RD.RemoteData<string, ContractInfo>, dispatch: React.Dispatch<Msg>, callEntrypointResult: RD.RemoteData<string, TransactionWalletOperation> }) => {

  if (props.contractInfo._tag === "RemoteSuccess") {
    const value = props.contractInfo.value
    if (props.callEntrypointResult._tag === "RemotePending")
      return <button
        className="mt-2 w-full block rounded-xl bg-blue-500 text-white px-3 py-2"
      >
        Loading...</button>

    else
      return <div>
        <button
          className="mt-2 w-full block rounded-xl bg-blue-500 text-white px-3 py-2"
          onClick={() => props.dispatch({ type: 'ValidateForm', value })}>
          Call Entrypoint</button>
      </div>

  }
  else return <div></div>
}
