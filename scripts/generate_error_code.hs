#!/usr/bin/env stack
-- stack --resolver lts-17.3 script --package string-interpolate --package universum --package text-manipulate

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Script for adding new error to ligo and also update the docs.
module GenerateErrorCode where

import Data.String.Interpolate (i)
import Data.Text.Manipulate (toCamel)
import Prelude ()
import Universum

data ErrorItem = ErrorItem
  { eiLabel :: Text
  , eiCode :: Integer
  , eiDesc :: Text
  }

data ShortErrorItem = Text :? Text
  deriving (Eq)

removedError :: ShortErrorItem
removedError = "" :? ""

errorsEnumerate :: Integer -> [ShortErrorItem] -> [ErrorItem]
errorsEnumerate start =
  map (\(eiCode, eiLabel :? eiDesc) -> ErrorItem{..}) .
  filter ((/= removedError) . snd) .
  zip [start..]

invalidInputErrors :: [ErrorItem]
invalidInputErrors = errorsEnumerate 100
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "invalid_witness_err"
      :? "Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick."

  , "too_big_price_change_err"
      :? "The action would apply too big of a change to the price, which is not allowed. \
         \We assume that the amount of X or Y tokens in the contract should not change by more than 30% at once \
         \(in some circumstances, a larger change may be allowed)."
         -- See the restrictions of `floor_log_half_bps`

  , "price_out_of_bounds_err"
      :? "The action would put the price out of bounds. \
         \Used tick indices should remain within `[-1048575; 1048575]` range, \
         \and, respectively, amount of one token type in the pair should not exceed \
         \`exp(0.0001)^1048575 ≈ 3.46 * 10^45` times the amount in the other token."

  , "past_deadline_err"
      :? "Swap has expired: now > deadline."

  , "smaller_than_min_asset_err"
      :? "Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`."

  , "tick_not_exist_err"
      :? "User provided tick is not initialized."

    -- Note: instead of "position_id_not_exists" we use FA2-specific error

  , "high_tokens_err"
      :? "The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`."

  , "invalid_timestamp_err"
      :? "Some of the timestamps passed to the `observe` entrypoint are too far back in the past."

  , "invalid_x_prime_contract_err"
      :? "The X prime contract address provided is not a segmented-cfmm contract."

  , "observe_outdated_timestamp_err"
      :? "Some of the timestamps passed to the `observe` entrypoint are too far back in the past."

  , "observe_future_timestamp_err"
      :? "Some of the timestamps passed to the `observe` entrypoint are yet in the future."

  , "tick_order_err"
      :? "When setting a new position, `upper_tick_index` must be strictly greater than `lower_tick_index`. \
         \When observing cumulative values at range, `upper_tick_index` must be greater or equal than `lower_tick_index`."

  , "position_liquidity_below_zero_err"
      :? "Liquidity of a position went below zero."

  , "incorrect_tick_spacing_err"
      :? "Tick indexes must be a multiple of the tick spacing."

  ]

invalidConfigErrors :: [ErrorItem]
invalidConfigErrors = errorsEnumerate 200
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "asset_transfer_invalid_entrypoints_err"
      :? "The `x_token_address` or `y_token_address` has no transfer entrypoint."
  , "asset_update_operator_invalid_entrypoints_err"
      :? "The `x_token_address` or `y_token_address` has no `update_operator` entrypoint."
  , "asset_approve_invalid_entrypoints_err"
      :? "The `x_token_address` or `y_token_address` has no `approve` entrypoint."

  ]

internalErrors :: [ErrorItem]
internalErrors = errorsEnumerate 300
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "internal_impossible_err"
      :? "Generic impossible error."

  , "internal_tick_not_exist_err"
      :? "Tick is not initialized."

  , "internal_epoch_bigger_than_now_err"
      :? "Time now is smaller than epoch time."

  , "internal_fee_more_than_100_percent_err"
      :? "The `fee_bps` is initialized to be higher than 10000 (100%)."

  , "internal_bad_sqrt_price_move_x_direction"
      :? "Unexpected price direction movement after sqrt_price_move_x."

  , "internal_bad_sqrt_price_move_y_direction"
      :? "Unexpected price direction movement after sqrt_price_move_y."

  , "internal_flip_fee_growth_outside_err"
      :? "Flip for `fee_growth_outside` failed. (This is an invariant of the contract)."

  , "internal_307"
      :? "Thrown when `(p.dx - dx_consumed)` or `(p.dy - dy_consumed)` is not nat."

  , "internal_tick_liquidity_below_zero_err"
      :? "Liquidity of a tick went below zero."

  , "internal_309"
      :? "Thrown when `(p.dx - r.dx)` is not nat."

  , removedError

  , "internal_311"
      :? "Thrown when `s.cur_tick_index.i >= upper_tick_index.i` and `(s.fee_growth.x - upper_tick.fee_growth_outside.x)` (or `y`) is not nat."

  , "internal_312"
      :? "Thrown when `s.cur_tick_index.i < lower_tick_index.i` and `(s.fee_growth.x - lower_tick.fee_growth_outside.x)` (or `y`) is not nat."

  , "internal_position_underflow_err"
      :? "Number of positions underflow."

  , removedError

  , removedError

  , "internal_316"
      :? "Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat."

  , "internal_317"
      :? "Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat."

  , "internal_sqrt_price_grow_err_1"
      :? "Thrown when `s.cur_tick_index.i < p.lower_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."

  , "internal_sqrt_price_grow_err_2"
      :? "Thrown when `p.lower_tick_index.i <= s.cur_tick_index.i && s.cur_tick_index.i < p.upper_tick_index.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."

  , "internal_negative_seconds_outside_err"
      :? "Thrown when `seconds_outside` is negative."

  , "internal_bad_access_to_observation_buffer"
      :? "Failed to access a value in time-weighted i_c cumulative sums buffer."

  , "internal_observe_bin_search_failed"
      :? "Some issue with binary search in `observe` entrypoint."

  , "internal_non_empty_position_gc_err"
      :? "Attempt to garbade collect a tick with non-zero liquidity net."

  , "internal_flip_seconds_per_liquidity_outside_err"
      :? "Flip of `seconds_per_liquidity_outside` failed. (This is an invariant of the contract)."

  , "internal_unexpected_income_err"
      :? "Position creation/change unexpectedly transferred tokens to someone"

  , "internal_negative_price"
      :? "Price became negative when crossing a tick"

  ]


---------------------------------------------------------------------------------
-- Ligo
---------------------------------------------------------------------------------

errorItemToLigoText :: ErrorItem -> Text
errorItemToLigoText ErrorItem{..} =
  [i|(* #{eiDesc} *)
[@inline] let #{eiLabel} = #{eiCode}n
|]

ligoErrorsTemplate :: Text
ligoErrorsTemplate =
  [i|// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

// NOTE: This file should not be modified directly.
// Use `stack scripts/generate_error_code.hs` instead.

\#if ERRORS_MLIGO
\#else
\#define ERRORS_MLIGO

\#include "types.mligo"

// ---------------------------------------------------------------------------
// -- Invalid input error codes
// ---------------------------------------------------------------------------

#{unlines $ errorItemToLigoText <$> invalidInputErrors}

// ---------------------------------------------------------------------------
// -- Contract configuration error codes
// ---------------------------------------------------------------------------

#{unlines $ errorItemToLigoText <$> invalidConfigErrors}

// ---------------------------------------------------------------------------
// -- Internal error codes
// ---------------------------------------------------------------------------

#{unlines $ errorItemToLigoText <$> internalErrors}

\#endif
|]

---------------------------------------------------------------------------------
-- Markdown
---------------------------------------------------------------------------------

errorItemToMdText :: ErrorItem -> Text
errorItemToMdText ErrorItem{..} =
  "| " <> (show @Text eiCode) <> " | `" <> eiLabel <> "` | " <> eiDesc <> " |"

mdErrorsTemplate :: Text
mdErrorsTemplate =
  [i|<!--
- SPDX-FileCopyrightText: 2021 Arthur Breitman
-
- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
-->

<!--
NOTE: This file should not be modified directly.
Use `stack scripts/generate_error_code.hs` instead.
-->

\#\# Error Codes

Here is a summary of all the error codes thrown by the contract.
(The list of errors may be inaccurate and incomplete, it will be updated during the implementation.)


\#\#\#\# Invalid Input Error Codes

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
#{unlines $ errorItemToMdText <$> invalidInputErrors}

\#\#\#\# Contract Configuration Error Codes

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
#{unlines $ errorItemToMdText <$> invalidConfigErrors}


\#\#\#\# Internal Error Codes

| Error Code       | Error Label      | Description                                           |
|------------------|------------------|-------------------------------------------------------|
#{unlines $ errorItemToMdText <$> internalErrors}

|]

---------------------------------------------------------------------------------
-- Haskell
---------------------------------------------------------------------------------

haskellFnName :: ErrorItem -> Text
haskellFnName = toCamel . eiLabel

errorItemToHaskellText :: ErrorItem -> Text
errorItemToHaskellText err@ErrorItem{eiDesc, eiCode} =
  [i|
-- | #{eiDesc}
#{fnName} :: Natural
#{fnName} = #{eiCode}|]
  where
    fnName = haskellFnName err

haskellErrorsTemplate :: Text
haskellErrorsTemplate =
  [i|-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- NOTE: This file should not be modified directly.
-- Use @stack scripts/generate_error_code.hs@ instead.

{-\# OPTIONS_GHC -Wno-missing-export-lists \#-}

module SegCFMM.Errors where

import Universum

----------------------------------------------------------------------------
-- Invalid Input Error Codes
----------------------------------------------------------------------------
#{unlines $ errorItemToHaskellText <$> invalidInputErrors}

----------------------------------------------------------------------------
-- Contract Configuration Error Codes
----------------------------------------------------------------------------
#{unlines $ errorItemToHaskellText <$> invalidConfigErrors}

----------------------------------------------------------------------------
-- Internal Error Codes
----------------------------------------------------------------------------
#{unlines $ errorItemToHaskellText <$> internalErrors}|]

main :: IO ()
main = do
  writeFile "ligo/errors.mligo" ligoErrorsTemplate
  writeFile "docs/error-codes.md" mdErrorsTemplate
  writeFile "haskell/src/SegCFMM/Errors.hs" haskellErrorsTemplate
