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

invalidInputErrors :: [ErrorItem]
invalidInputErrors =
  [ ErrorItem
      { eiLabel = "invalid_witness_err"
      , eiCode = 100
      , eiDesc = "Invalid witness. The witness must refer to an initialized tick that is below or equal to the supplied tick."
      }
  , ErrorItem
      { eiLabel = "log_out_of_bounds_err"
      , eiCode = 101
      , eiDesc = "Log out of bounds."
      }
  , ErrorItem
      { eiLabel = "end_ladder_reached_err"
      , eiCode = 102
      , eiDesc = "Should not reach end of ladder." -- TODO [TCFMM-19]: improve this error message
      }
  , ErrorItem
      { eiLabel = "past_deadline_err"
      , eiCode = 103
      , eiDesc = "Swap has expired: now > deadline."
      }
  , ErrorItem
      { eiLabel = "smaller_than_min_asset_err"
      , eiCode = 104
      , eiDesc = "Threshold on amount of bought tokens violated: `dx` received < `min_dx` or `dy` received < `min_dy`."
      }
  , ErrorItem
      { eiLabel = "tick_not_exist_err"
      , eiCode = 105
      , eiDesc = "User provided tick is not initialized."
      }
  , ErrorItem
      { eiLabel = "high_tokens_err"
      , eiCode = 106
      , eiDesc = "The amount of tokens that needs to be transferred to the contract is higher than `maximum_tokens_contributed`."
      }
  , ErrorItem
      { eiLabel = "invalid_timestamp_err"
      , eiCode = 107
      , eiDesc = "Some of the timestamps passed to the `observe` entrypoint are too far back in the past."
      }
  ]

invalidConfigErrors :: [ErrorItem]
invalidConfigErrors =
  [ ErrorItem
      { eiLabel = "asset_transfer_invalid_entrypoints_err"
      , eiCode = 200
      , eiDesc = "The `const_x_token_entrypoint` or `const_y_token_entrypoint` has no transfer entrypoint."
      }
  ]

internalErrors :: [ErrorItem]
internalErrors =
  [ ErrorItem
      { eiLabel = "internal_tick_not_exist_err"
      , eiCode = 300
      , eiDesc = "Tick is not initialized."
      }
  , ErrorItem
      { eiLabel = "internal_epoch_bigger_than_now_err"
      , eiCode = 301
      , eiDesc = "Time now is smaller than epoch time."
      }
  , ErrorItem
      { eiLabel = "internal_fee_more_than_100_percent_err"
      , eiCode = 302
      , eiDesc = "The `const_fee_bps` is initialized to be higher than 10000 (100%)."
      }
  , ErrorItem
      { eiLabel = "internal_303"
      , eiCode = 303
      , eiDesc = "Thrown when `(p.s.sqrt_price - sqrt_price_new)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_304"
      , eiCode = 304
      , eiDesc = "Thrown when `(sqrt_price_new - p.s.sqrt_price)` is not nat."
      }
  , ErrorItem
      { eiLabel = "flip_fee_growth_outside_err"
      , eiCode = 305
      , eiDesc = "Flip for `fee_growth_outside` failed. (This is an invariant of the contract)."
      }
  , ErrorItem
      { eiLabel = "internal_306"
      , eiCode = 306
      , eiDesc = "Thrown when `(p.dx - dx_consummed)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_liquidity_below_zero_err"
      , eiCode = 308
      , eiDesc = "Liquidity went below zero."
      }
  , ErrorItem
      { eiLabel = "internal_309"
      , eiCode = 309
      , eiDesc = "Thrown when `(p.dx - r.dx)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_insufficient_balance_err"
      , eiCode = 310
      , eiDesc = "Contract does not have enough liquidity to execute the swap."
      }
  , ErrorItem
      { eiLabel = "internal_311"
      , eiCode = 311
      , eiDesc = "Thrown when `s.i_c >= key.hi.i` and `(s.fee_growth.x - tick_hi.fee_growth_outside.x)` (or `y`) is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_312"
      , eiCode = 312
      , eiDesc = "Thrown when `s.i_c < key.hi.i` and `(s.fee_growth.x - tick_lo.fee_growth_outside.x)` (or `y`) is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_position_underflow_err"
      , eiCode = 313
      , eiDesc = "Number of positions underflow."
      }
  , ErrorItem
      { eiLabel = "internal_314"
      , eiCode = 314
      , eiDesc = "Thrown when `(s.fee_growth.x - f_a.x - f_b.x)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_315"
      , eiCode = 315
      , eiDesc = "Thrown when `(s.fee_growth.y - f_a.y - f_b.y)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_316"
      , eiCode = 316
      , eiDesc = "Thrown when `(fee_growth_inside.x - position.fee_growth_inside_last.x)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_317"
      , eiCode = 317
      , eiDesc = "Thrown when `(fee_growth_inside.y - position.fee_growth_inside_last.y)` is not nat."
      }
  , ErrorItem
      { eiLabel = "internal_sqrt_price_grow_err_1"
      , eiCode = 318
      , eiDesc = "Thrown when `s.i_c < i_l.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."
      }
  , ErrorItem
      { eiLabel = "internal_sqrt_price_grow_err_2"
      , eiCode = 319
      , eiDesc = "Thrown when `i_l.i <= s.i_c && s.i_c < i_u.i` and the `sqrt_price` happened not to grow monotonically with tick indices (This is an invariant of the contract)."
      }
  , ErrorItem
      { eiLabel = "internal_impossible_err"
      , eiCode = 320
      , eiDesc = "Generic impossible error."
      }
  , ErrorItem
      { eiLabel = "internal_negative_seconds_outside_err"
      , eiCode = 321
      , eiDesc = "Thrown when `seconds_outside` is negative."
      }
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
