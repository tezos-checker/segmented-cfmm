// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman


import { ContractMethod , Signer, TransactionWalletOperation,
         Contract, ContractAbstraction,
         ContractProvider, TezosToolkit, MichelsonMap
       } from '@taquito/taquito';
import { InMemorySigner, importKey } from '@taquito/signer';

import { Parameter } from './generated/Parameter';
import { Observe } from './generated/Observe';
import { Increase_observation_count } from './generated/Increase_observation_count';
import { Set_position } from "./generated/Set_position";
import { X_to_x_prime } from './generated/X_to_x_prime';
import { X_to_y } from './generated/X_to_y';
import { Y_to_x } from './generated/Y_to_x';

function println(s: string) {
  console.log(s);
}

type Callback = (Contract) => ContractMethod<ContractProvider>

type ExtractMapKey<MapType> = MapType extends Map<infer R, infer V> ? R : null;
type ExtractMapValue<MapType> = MapType extends Map<infer R, infer V> ? V : null;

const unit = ["Unit"]; //https://github.com/ecadlabs/taquito/issues/526

/**
 * Check equality of objects via JSON.stringify().
 */
function isSame(exp: any, got: any): void {
  let exp_str = JSON.stringify(exp);
  let got_str = JSON.stringify(got);
  if (exp_str !== got_str) {
    throw {expected: exp_str, got: got_str};
  }
}

/**
 * Wraps SegCFMM Contract methods
 */
export class SegCFMMContract {
  private contractAddr: string;
  private nodeAddr: string;
  private senderSk: string;
  private contract: undefined | Promise<ContractAbstraction<ContractProvider>>;

  public debug: boolean;

  public lastOperationHash: undefined | string;

  constructor(nodeAddr: string, senderSk: string, contractAddr: string) {
    this.contractAddr = contractAddr;
    this.nodeAddr = nodeAddr;
    this.senderSk = senderSk;
    this.debug = false;
  }

  public parameterSchema(): Promise<object> {
    return this.inspectContract(contract => contract.parameterSchema.ExtractSchema());
  }

  public storageSchema(): Promise<object> {
    return this.inspectContract(contract => contract.schema.ExtractSchema());
  }

  /**
   * Ensure the storage of the contract matches with provided the reference object
   * @param storageSchemaTargetJson - The reference schema as a plain object.
   * @returns A promise that yields nothing on success or throw on fail.
   */
  public ensureStorageSchema(storageSchemaTargetJson: Object): Promise<void> {
      let checker = function(contract: ContractAbstraction<ContractProvider>): void {
        isSame(storageSchemaTargetJson, contract.schema.ExtractSchema());
      }
      return this.inspectContract(checker);
  }

  /**
   * Ensure the parameter of the contract matches with provided the reference object
   * @param storageSchemaTargetJson - The reference schema as a plain object.
   * @returns A promise that yields nothing on success or throw on fail.
   */
  public ensureParamSchema(paramSchemaTargetJson: Object): Promise<void> {
      let checker = function(contract: ContractAbstraction<ContractProvider>): void {
        isSame(paramSchemaTargetJson, contract.parameterSchema.ExtractSchema());
      };
      return this.inspectContract(checker);
  }

  private initContract(): Promise<ContractAbstraction<ContractProvider>> {
    if (this.contract === undefined) {
      const Tezos = new TezosToolkit(this.nodeAddr);
      Tezos.setProvider({ signer: new InMemorySigner(this.senderSk) });
      this.contract = Tezos.contract.at(this.contractAddr);
    }
    return this.contract;
  }

  public setSender(senderSk: string) {
    this.senderSk = senderSk;
    return this;
  }

  public inspectContract(callback: (c: ContractAbstraction<ContractProvider>) => any): Promise<any> {
    return this.initContract().then(contract => {
      return callback(contract);
      })
  }

  private withContract(callback: Callback): Promise<string | void> {

    // Initialize contract if it hasn't been initialized
    return this.initContract().then(callback)
        .then((method:ContractMethod<ContractProvider>) => {
          if (this.debug) {
            let tp = method.toTransferParams();
            println(JSON.stringify(tp, null, 2));
          }
          return method.send();
        })
        .then((op: any): string => {
          println(`Waiting for ${op.hash} to be confirmed...`);
          return op.confirmation(1).then(() => {
            this.lastOperationHash = op.hash;
            return this;
          });
        });
  }

  // entrypoint methods

  observe(arg: Observe): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.observe(arg));
  }

  increase_observation_count(arg: Increase_observation_count): Promise<string|void> {
      return this.withContract(
          contract => contract.methods.Increase_observation_count(arg));
  }

  set_position(arg: Set_position): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.set_position(arg));
  }

  x_to_x_prime(arg: X_to_x_prime): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.x_to_x_prime(arg));
  }

  x_to_y(arg: X_to_y): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.x_to_y(arg));
  }

  y_to_x(arg: Y_to_x): Promise<string|void> {
    return this.withContract(
      contract => contract.methods.y_to_x(arg));
  }


}
