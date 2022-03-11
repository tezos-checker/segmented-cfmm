// SPDX-FileCopyrightText: 2021 Arthur Breitman
// SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

import { programWithDebugger } from 'elm-ts/lib/Debug/Html'
import * as React from 'elm-ts/lib/React'
import { render } from 'react-dom'
import * as component from './App'

const program = process.env.NODE_ENV === 'production' ? React.program : programWithDebugger

const main = program(component.init(), component.update, component.view)

React.run(main, dom => render(dom, document.getElementById('app')!))
