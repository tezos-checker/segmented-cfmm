import {Lambda} from '../common';
export interface Observe {
  callback: string;
  times: ObserveTimes;
};
export type ObserveTimes = Array<string>;
