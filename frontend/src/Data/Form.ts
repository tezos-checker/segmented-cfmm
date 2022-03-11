// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import * as O from "fp-ts/lib/Option"
import { Moment } from "moment";
import { toNumber, toNat } from '../Shared/Helper';
import moment from 'moment'

export type ValidateState
  = { type: "ValidateInitial" }
  | { type: "ValidateComplete", errors: string[] }

export type FormType
  = { type: "TextForm" }
  | { type: "CalendarForm", momentVal: Moment }
  | { type: "RadioForm", values: [string, string][], selected: string }

export type ValidationFunc = (value: string) => O.Option<string>


export type FormItem = {
  order: number, // How the form is ordered
  label: string,
  desc: string,
  value: string,
  placeholder: string,
  validatedState: ValidateState,
  validationFuncs: ValidationFunc[],
  formType: FormType,
}

// ----------------------------------------------------------------------------------------
// Validation functions
// ----------------------------------------------------------------------------------------

export const required: ValidationFunc = (value: string) => {
  if (value == "") return O.some("Input cannot be empty.")
  else return O.none
}

export const isNumber: ValidationFunc = (value: string) => {
  const result = toNumber(value)
  if (result._tag === "None") return O.some("Input must be a number")
  else return O.none
}

export const isNat: ValidationFunc = (value: string) => {
  const result = toNat(value)
  if (result._tag === "None") return O.some("Number cannot be negative")
  else return O.none
}


// ----------------------------------------------------------------------------------------
// Helper
// ----------------------------------------------------------------------------------------

const getNowPlusOneMinute = () => moment().add(1, 'minutes')

// ----------------------------------------------------------------------------------------
// Form Datas
// ----------------------------------------------------------------------------------------

export const xToYFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["dx",
        {
          order: 1,
          label: "X Amount",
          desc: "X tokens to sell.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["min_dy",
        {
          order: 2,
          label: "Minimum Y Amount",
          desc: "The transaction won't be executed if buying less than the given amount of Y tokens.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["deadline",
        {
          order: 3,
          label: "Deadline:",
          desc: "The transaction won't be executed past this point.",
          value: getNowPlusOneMinute().format("YYYY-MM-DD[T]HH:mm:ss[Z]"),
          placeholder: "2022-01-01T00:01:40Z",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "CalendarForm", momentVal: getNowPlusOneMinute()}
        }
      ],
      ["to_dy",
        {
          order: 4,
          label: "To Address",
          desc: "Recipient of dy.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ]
    ]
  )

export const yToXFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["dy",
        {
          order: 1,
          label: "Y Amount",
          desc: "Y tokens to sell.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["min_dx",
        {
          order: 2,
          label: "Minimum X Amount",
          desc: "The transaction won't be executed if buying less than the given amount of X tokens.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["deadline",
        {
          order: 3,
          label: "Deadline:",
          desc: "The transaction won't be executed past this point.",
          value: getNowPlusOneMinute().format("YYYY-MM-DD[T]HH:mm:ss[Z]"),
          placeholder: "2022-01-01T00:01:40Z",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "CalendarForm", momentVal: getNowPlusOneMinute()}
        }
      ],
      ["to_dx",
        {
          order: 4,
          label: "To Address",
          desc: "Recipient of dx.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ]
    ]
  )

export const setPositionFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["liquidity",
        {
          order: 1,
          label: "Liquidity ",
          desc: "Liquidity of the new position",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["lower_tick_index",
        {
          order: 2,
          label: "Lower Tick Index",
          desc: "Lower tick.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNumber],
          formType: { type: "TextForm" }
        }
      ],
      ["lower_tick_witness",
        {
          order: 3,
          label: "Lower Tick Witness",
          desc: "Lower tick's witness calculated offchain.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNumber],
          formType: { type: "TextForm" }
        }
      ],
      ["upper_tick_index",
        {
          order: 4,
          label: "Upper Tick Index",
          desc: "Upper tick.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNumber],
          formType: { type: "TextForm" }
        }
      ],
      ["upper_tick_witness",
        {
          order: 5,
          label: "Upper Tick Witness",
          desc: "Upper tick's witness calculated offchain.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNumber],
          formType: { type: "TextForm" }
        }
      ],
      ["maximum_tokens_contributed_x",
        {
          order: 6,
          label: "Maximum X Tokens Contributed",
          desc: "The maximum number of X token to contribute.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["maximum_tokens_contributed_y",
        {
          order: 7,
          label: "Maximum Y Tokens Contributed",
          desc: "The maximum number of Y token to contribute.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["deadline",
        {
          order: 8,
          label: "Deadline:",
          desc: "The transaction won't be executed past this point.",
          value: getNowPlusOneMinute().format("YYYY-MM-DD[T]HH:mm:ss[Z]"),
          placeholder: "2022-01-01T00:01:40Z",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "CalendarForm", momentVal: getNowPlusOneMinute()}
        }
      ],
    ]
  )


export const transferFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["from_",
        {
          order: 1,
          label: "From Address",
          desc: "Address of the user to remove the position from.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
      ["to_",
        {
          order: 2,
          label: "To Address",
          desc: "Address of the user to transfer the position to.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
      ["token_id",
        {
          order: 3,
          label: "Position ID",
          desc: "ID of the position to be transfered.",
          value: "",
          placeholder: "1",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
    ]
  )

export const updateOperatorsFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["is_add",
        {
          order: 1,
          label: "Add or update",
          desc: "Choose add or update operation.",
          value: "true",
          placeholder: "",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [],
          formType: { type: "RadioForm", values: [["Add Operator", "true"], ["Remove Operator", "false"]], selected: "true" }
        }
      ],
      ["operator_address",
        {
          order: 2,
          label: "Operator Address",
          desc: "Address of the user to be made/removed as operator.",
          value: "",
          placeholder: "KT1XMaGzMc7iGVkbnCC9gZyjs2b85A3NxJTz",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
      ["token_id",
        {
          order: 3,
          label: "Position ID",
          desc: "ID of the position associated with the operator.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ]
    ]
  )


export const xToXPrimeFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["dx",
        {
          order: 1,
          label: "X Amount",
          desc: "Amount of X tokens to sell.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["min_dx_prime",
        {
          order: 2,
          label: "Minimum X' Amount",
          desc: "The transaction won't be executed if buying less than the given amount of X' tokens.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["deadline",
        {
          order: 3,
          label: "Deadline:",
          desc: "The transaction won't be executed past this point.",
          value: getNowPlusOneMinute().format("YYYY-MM-DD[T]HH:mm:ss[Z]"),
          placeholder: "2022-01-01T00:01:40Z",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "CalendarForm", momentVal: getNowPlusOneMinute()}
        }
      ],
      ["to_dx_prime",
        {
          order: 4,
          label: "To Address",
          desc: "Recipient of dx'.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
      ["xprime_contract",
        {
          order: 5,
          label: "X' Contract",
          desc: "Address of another segmented-cfmm contract.",
          value: "",
          placeholder: "KT1XMaGzMc7iGVkbnCC9gZyjs2b85A3NxJTz",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ]
    ]
  )

export const increaseObservationCountFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["added_observation_count",
        {
          order: 1,
          label: "Add observation count",
          desc: "Increase the number of observations of `tick_cumulative` and `seconds_per_liquidity_cumulative` taken and stored in the contract by the given number.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
    ]
  )


export const updatePositionFormInit: () => Map<string, FormItem> = () =>
  new Map(
    [
      ["position_id",
        {
          order: 1,
          label: "Position ID",
          desc: "ID of the position to be updated.",
          value: "",
          placeholder: "1",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["liquidity_delta",
        {
          order: 2,
          label: "Liquidity Delta",
          desc: " How to change the liquidity of the existing position.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNumber],
          formType: { type: "TextForm" }
        }
      ],

      ["maximum_tokens_contributed_x",
        {
          order: 3,
          label: "Maximum X Tokens Contributed",
          desc: "The maximum number of X token to contribute.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["maximum_tokens_contributed_y",
        {
          order: 4,
          label: "Maximum Y Tokens Contributed",
          desc: "The maximum number of Y token to contribute.",
          value: "",
          placeholder: "0",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required, isNat],
          formType: { type: "TextForm" }
        }
      ],
      ["deadline",
        {
          order: 5,
          label: "Deadline:",
          desc: "The transaction won't be executed past this point.",
          value: getNowPlusOneMinute().format("YYYY-MM-DD[T]HH:mm:ss[Z]"),
          placeholder: "2022-01-01T00:01:40Z",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "CalendarForm", momentVal: getNowPlusOneMinute()}
        }
      ],
      ["to_x",
        {
          order: 6,
          label: "To X Address",
          desc: "Where to send the freed X tokens, if any.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
      ["to_y",
        {
          order: 7,
          label: "To Y Address",
          desc: "Where to send the freed Y tokens, if any.",
          value: "",
          placeholder: "tz1ZCQP68ybWVzAuA5KidbWgwXz1Eh6ioMyo",
          validatedState: { type: "ValidateInitial" },
          validationFuncs: [required],
          formType: { type: "TextForm" }
        }
      ],
    ]
  )
