import {Lambda} from '../common';
export interface Update_position {
  deadline: string;
  liquidity_delta: number;
  maximum_tokens_contributed: Update_positionMaximum_tokens_contributed;
  position_id: number;
  to_x: string;
  to_y: string;
};
export interface Update_positionMaximum_tokens_contributed {
  x: number;
  y: number;
};
