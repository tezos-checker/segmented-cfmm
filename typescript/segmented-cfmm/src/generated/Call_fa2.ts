import {Lambda} from '../common';
export type Call_fa2 =
  | Call_fa2Balance_of
  | Call_fa2Transfer
  | Call_fa2Update_operators
export interface Call_fa2Balance_ofItem {
  requests: Call_fa2Balance_ofRequests;
  callback: string;
};
export interface Call_fa2Balance_of {
  balance_of: Call_fa2Balance_ofItem;
};
export type Call_fa2Balance_ofRequests = Array<Call_fa2Balance_ofRequestsItem>;
export interface Call_fa2Balance_ofRequestsItem {
  owner: string;
  token_id: number;
};
export type Call_fa2Transfer = Array<Call_fa2TransferItem>;
export interface Call_fa2TransferItem {
  from_: string;
  txs: Call_fa2TransferItemTxs;
};
export type Call_fa2TransferItemTxs = Array<Call_fa2TransferItemTxsItem>;
export interface Call_fa2TransferItemTxsItem {
  to_: string;
  token_id: number;
  amount: number;
};
export type Call_fa2Update_operators = Array<Call_fa2Update_operatorsItem>;
export type Call_fa2Update_operatorsItem =
  | Call_fa2Update_operatorsItemAdd_operator
  | Call_fa2Update_operatorsItemRemove_operator
export interface Call_fa2Update_operatorsItemAdd_operatorItem {
  owner: string;
  operator: string;
  token_id: number;
};
export interface Call_fa2Update_operatorsItemAdd_operator {
  add_operator: Call_fa2Update_operatorsItemAdd_operatorItem;
};
export interface Call_fa2Update_operatorsItemRemove_operatorItem {
  owner: string;
  operator: string;
  token_id: number;
};
export interface Call_fa2Update_operatorsItemRemove_operator {
  remove_operator: Call_fa2Update_operatorsItemRemove_operatorItem;
};
