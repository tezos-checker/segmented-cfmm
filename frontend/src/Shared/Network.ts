// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import { TezosToolkit, TransactionWalletOperation } from '@taquito/taquito';
import { TempleWallet } from '@temple-wallet/dapp';
import { pipe } from 'fp-ts/lib/function';
import * as M from "fp-ts/lib/Map";
import * as O from "fp-ts/lib/Option";
import * as S from "fp-ts/lib/string";
import * as T from "fp-ts/lib/Task";
import * as TE from "fp-ts/lib/TaskEither";
import { FormItem } from '../Data/Form';
import { Increase_observation_count } from '../Generated/generated/Increase_observation_count';
import { Parameter } from "../Generated/generated/Parameter";
import { Set_position } from "../Generated/generated/Set_position";
import { Transfer } from "../Generated/generated/Transfer";
import { Update_operators } from "../Generated/generated/Update_operators";
import { Update_position } from "../Generated/generated/Update_position";
import { X_to_x_prime } from "../Generated/generated/X_to_x_prime";
import { X_to_y } from "../Generated/generated/X_to_y";
import { Y_to_x } from "../Generated/generated/Y_to_x";
import { ContractInfo, EntrypointTypes, Model } from '../Type';
import { getSomeUnsafe, toNumber } from './Helper';


export const findContract = (model: Model, addr: string): TE.TaskEither<any, ContractInfo> => {
  return pipe(
    TE.tryCatch(
      () => model.tezos.contract.at(addr),
      reason => reason,
    ))
}

const extractParam = (model: Model, ep: EntrypointTypes, formMap: Map<string, FormItem>): Parameter => {
  if (ep.type === "x_to_y") {
    const dx = pipe(formMap, M.lookup(S.Ord)("dx"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const min_dy = pipe(formMap, M.lookup(S.Ord)("min_dy"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const deadline = pipe(formMap, M.lookup(S.Ord)("deadline"), O.map(a => a.value), getSomeUnsafe)
    const to_dy = pipe(formMap, M.lookup(S.Ord)("to_dy"), O.map(a => a.value), getSomeUnsafe)
    const params: X_to_y = {
      deadline,
      dx,
      min_dy,
      to_dy
    }
    return params;
  }
  else if (ep.type === "y_to_x") {
    const dy = pipe(formMap, M.lookup(S.Ord)("dy"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const min_dx = pipe(formMap, M.lookup(S.Ord)("min_dx"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const deadline = pipe(formMap, M.lookup(S.Ord)("deadline"), O.map(a => a.value), getSomeUnsafe)
    const to_dx = pipe(formMap, M.lookup(S.Ord)("to_dx"), O.map(a => a.value), getSomeUnsafe)

    const params: Y_to_x = {
      deadline,
      dy,
      min_dx,
      to_dx
    }
    return params;
  }
  else if (ep.type === "set_position") {
    const liquidity = pipe(formMap, M.lookup(S.Ord)("liquidity"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const lower_tick_index = pipe(formMap, M.lookup(S.Ord)("lower_tick_index"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const lower_tick_witness = pipe(formMap, M.lookup(S.Ord)("lower_tick_witness"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const upper_tick_index = pipe(formMap, M.lookup(S.Ord)("upper_tick_index"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const upper_tick_witness = pipe(formMap, M.lookup(S.Ord)("upper_tick_witness"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const maximum_tokens_contributed_x = pipe(formMap, M.lookup(S.Ord)("maximum_tokens_contributed_x"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const maximum_tokens_contributed_y = pipe(formMap, M.lookup(S.Ord)("maximum_tokens_contributed_y"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const deadline = pipe(formMap, M.lookup(S.Ord)("deadline"), O.map(a => a.value), getSomeUnsafe)
    const params: Set_position = {
      deadline,
      liquidity,
      lower_tick_index,
      lower_tick_witness,
      maximum_tokens_contributed: { x: maximum_tokens_contributed_x, y: maximum_tokens_contributed_y },
      upper_tick_index,
      upper_tick_witness,
    }
    return params;

  }
  else if (ep.type === "update_position") {
    const liquidity_delta = pipe(formMap, M.lookup(S.Ord)("liquidity_delta"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const maximum_tokens_contributed_x = pipe(formMap, M.lookup(S.Ord)("maximum_tokens_contributed_x"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const maximum_tokens_contributed_y = pipe(formMap, M.lookup(S.Ord)("maximum_tokens_contributed_y"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const deadline = pipe(formMap, M.lookup(S.Ord)("deadline"), O.map(a => a.value), getSomeUnsafe)
    const to_x = pipe(formMap, M.lookup(S.Ord)("to_x"), O.map(a => a.value), getSomeUnsafe)
    const to_y = pipe(formMap, M.lookup(S.Ord)("to_y"), O.map(a => a.value), getSomeUnsafe)
    const position_id = pipe(formMap, M.lookup(S.Ord)("position_id"), O.chain(a => toNumber(a.value)), getSomeUnsafe)

    const params: Update_position = {
      deadline,
      liquidity_delta: liquidity_delta,
      maximum_tokens_contributed: { x: maximum_tokens_contributed_x, y: maximum_tokens_contributed_y },
      position_id,
      to_x,
      to_y
    }

    return params;

  }
  else if (ep.type === "x_to_x_prime") {
    const dx = pipe(formMap, M.lookup(S.Ord)("dx"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const min_dx_prime = pipe(formMap, M.lookup(S.Ord)("min_dx_prime"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const deadline = pipe(formMap, M.lookup(S.Ord)("deadline"), O.map(a => a.value), getSomeUnsafe)
    const to_dx_prime = pipe(formMap, M.lookup(S.Ord)("to_dx_prime"), O.map(a => a.value), getSomeUnsafe)
    const xprime_contract = pipe(formMap, M.lookup(S.Ord)("xprime_contract"), O.map(a => a.value), getSomeUnsafe)
    const params: X_to_x_prime = {
      deadline,
      dx,
      min_dx_prime,
      to_dx_prime,
      x_prime_contract: xprime_contract
    }
    return params;
  }
  else if (ep.type === "transfer") {
    const from_ = pipe(formMap, M.lookup(S.Ord)("from_"), O.map(a => a.value), getSomeUnsafe)
    const to_ = pipe(formMap, M.lookup(S.Ord)("to_"), O.map(a => a.value), getSomeUnsafe)
    const token_id = pipe(formMap, M.lookup(S.Ord)("token_id"), O.chain(a => toNumber(a.value)), getSomeUnsafe)

    const params: Transfer = [
      {
        from_: from_,
        txs: [{
          to_: to_,
          token_id: token_id,
          amount: 1
        }
        ]
      }
    ]
    return params;
  }
  else if (ep.type === "update_operators") {
    const is_add = pipe(formMap, M.lookup(S.Ord)("is_add"), O.map(a => a.value), getSomeUnsafe)
    const owner_address = pipe(model.userAddress, getSomeUnsafe)
    const operator_address = pipe(formMap, M.lookup(S.Ord)("operator_address"), O.map(a => a.value), getSomeUnsafe)
    const token_id = pipe(formMap, M.lookup(S.Ord)("token_id"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    const param = () => {
      if (is_add === "true") {
        return {
          "add_operator": {
            "owner": owner_address,
            "operator": operator_address,
            "token_id": token_id,
          }
        }
      } else return {
        "remove_operator": {
          "owner": owner_address,
          "operator": operator_address,
          "token_id": token_id,
        }
      }
    }
    return ([param()] as Update_operators)
  }
  else if (ep.type === "increase_observation_count") {
    const added_observation_count = pipe(formMap, M.lookup(S.Ord)("added_observation_count"), O.chain(a => toNumber(a.value)), getSomeUnsafe)
    return (added_observation_count as Increase_observation_count)

  }
  else throw new Error("extractParam: Entrypoint does not exist.")
}

export const callEntrypoint = (model: Model, contractInfo: ContractInfo, entrypoint: EntrypointTypes, formMap: Map<string, FormItem>): TE.TaskEither<any, TransactionWalletOperation> => {
  const params = extractParam(model, entrypoint, formMap)
  return pipe(
    TE.tryCatch(
      () => model.tezos.wallet.at(contractInfo.address),
      reason => reason,
    ),
    TE.chain(w => TE.tryCatch(
      () => w.methodsObject[entrypoint.type](params).send(),
      reason => reason
    ))
  )
}

export const getStorage = (model: Model, contractInfo: ContractInfo): TE.TaskEither<any, TransactionWalletOperation> => {
  return pipe(
    TE.tryCatch(
      () => model.tezos.wallet.at(contractInfo.address),
      reason => reason,
    ),
    TE.chain(w => TE.tryCatch(
      () => w.storage(),
      reason => reason
    ))
  )
}

export const getWalletAddress = (model: Model): T.Task<string> => {
  return () => model.tezos.wallet.pkh()
}


export const connectWallet = (model: Model): TE.TaskEither<string, TezosToolkit> => {
  return pipe(
    TE.tryCatch(() => TempleWallet.isAvailable(), _ => "impossible"), // Have to used impossible since `fromTask` cannot infer type correctlya
    TE.chain(status => {
      const myWallet = new TempleWallet('CFMM Finder')
      if (status === true)
        return pipe(
          TE.tryCatch(() => myWallet.connect('granadanet'), err => "Unable to connect to network" + (err as any).toString),
          TE.chain(() => TE.tryCatch(
            (() => {
              model.tezos.setWalletProvider(myWallet);
              return T.of(model.tezos);
            })(),
            _ => "impossible")
          )
        )
      else
        return TE.throwError("Wallet not awailable")
    }))
}
