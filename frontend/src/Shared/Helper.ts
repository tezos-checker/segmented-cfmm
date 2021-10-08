// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import * as O from "fp-ts/lib/Option"
import { identity, pipe } from 'fp-ts/lib/function';

export const toNumber = (value: string | number) => {
  if (typeof value === 'number') return O.some(value)
  else {
    let result = parseInt(value, 10)
    console.log("result", result)
    if (!isNaN(result)) return O.some(result)
    else return O.none
  }
}

export const getSomeUnsafe = <T>(value: O.Option<T>): T =>
  pipe(
    value,
    O.fold(
      () => { throw new Error("getSomeUnsafe: Expect value to be some, but get none.") },
      identity
    )
  )
