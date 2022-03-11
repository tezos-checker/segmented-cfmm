import {Lambda} from '../common';
export interface Set_position {
  deadline: string;
  liquidity: number;
  lower_tick_index: number;
  lower_tick_witness: number;
  maximum_tokens_contributed: Set_positionMaximum_tokens_contributed;
  upper_tick_index: number;
  upper_tick_witness: number;
};
export interface Set_positionMaximum_tokens_contributed {
  x: number;
  y: number;
};
