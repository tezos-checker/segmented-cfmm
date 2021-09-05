#!/usr/bin/env stack
-- stack --resolver lts-17.3 script --package string-interpolate --package universum

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Script for adding new error to ligo and also update the docs.
module GenerateErrorCode where

import Data.String.Interpolate (i)
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

  , "log_out_of_bounds_err"
      :? "Log out of bounds."

  , "end_ladder_reached_err"
      :? "Should not reach end of ladder." -- TODO [TCFMM-19]: improve this error message

  , "past_deadline_err"
      :? "Swap has expired: now > deadline."

  , "smaller_than_min_asset_err"
      :? "Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`."

  , "tick_not_exist_err"
      :? "User provided tick is not initialized."

  , "high_tokens_err"
      :? "The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`."

  , "invalid_timestamp_err"
      :? "Some of the timestamps passed to the `observe` entrypoint are too far back in the past."

  ]

invalidConfigErrors :: [ErrorItem]
invalidConfigErrors = errorsEnumerate 200
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "asset_transfer_invalid_entrypoints_err"
      :? "The `const_x_token_entrypoint` or `const_y_token_entrypoint` has no transfer entrypoint."

  ]

internalErrors :: [ErrorItem]
internalErrors = errorsEnumerate 300
  -- New errors must be added to the _end_ of this list.
  -- To remove an error, replace it with 'removedError'.

  [ "internal_tick_not_exist_err"
      :? "Tick is not initialized."

  , "internal_epoch_bigger_than_now_err"
      :? "Time now is smaller than epoch time."

  , "internal_fee_more_than_100_percent_err"
      :? "The `const_fee_bps` is initialized to be higher than 10000 (100%)."

  , "internal_303"
      :? "Thrown when `(p.s.sqrt_price - sqrt_price_new)` is not nat."

  , "internal_304"
      :? "Thrown when `(sqrt_price_new - p.s.sqrt_price)` is not nat."

  , "flip_fee_growth_outside_err"
      :? "Flip for `fee_growth_outside` failed. (This is an invariant of the contract)."

  , "internal_306"
      :? "Thrown when `(p.dx - dx_consummed)` is not nat."

  , "internal_liquidity_below_zero_err"
      :? "Liquidity went below zero."

  , "internal_309"
      :? "Thrown when `(p.dx - r.dx)` is not nat."

  , "internal_insufficient_balance_err"
      :? "Contract does not have enough liquidity to execute the swap."

  , "internal_311"
      :? "Thrown when `s.i_c >= key.hi.i` and `(s.fee_growth.x - tick_hi.fee_growth_outside.x)` (or `y`) is not nat."

  , "internal_312"
      :? "Thrown when `s.i_c < key.hi.i` and `(s.fee_growth.x - tick_lo.fee_growth_outside.x)` (or `y`) is not nat."

  , "internal_position_underflow_err"
      :? "Number of positions underflow."

  , "internal_314"
      :? "Thrown when `(s.fee_growth.x - f_a.x - f_b.x)` is not nat."

  , "internal_315"
      :? "Thrown when `(s.fee_growth.y - f_a.y - f_b.y)` is not nat."

  , "internal_316"
      :? "Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat."

  , "internal_317"
      :? "Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat."

  , "internal_sqrt_price_grow_err_1"
      :? "Thrown when `s.i_c < i_l.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."

  , "internal_sqrt_price_grow_err_2"
      :? "Thrown when `i_l.i <= s.i_c && s.i_c < i_u.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."

  , "internal_impossible_err"
      :? "Generic impossible error."

  , "internal_negative_seconds_outside_err"
      :? "Thrown when `seconds_outside` is negative."

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

main :: IO ()
main = do
  writeFile "ligo/errors.mligo" ligoErrorsTemplate
  writeFile "docs/error-codes.md" mdErrorsTemplate
