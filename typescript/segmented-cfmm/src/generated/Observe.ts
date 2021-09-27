import {Lambda} from '../common';
export interface Observe {
  viewParam: ObserveViewParam;
  viewCallbackTo: string;
};
export type ObserveViewParam = Array<string>;
