<!--
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-->

# Mirth

![](https://github.com/mirth-lang/mirth/workflows/tests/badge.svg)
![](https://github.com/mirth-lang/mirth/workflows/self-build/badge.svg)

**Mirth** is a new type-safe concatenative functional programming language. Mirth was inspired by **Forth**, **Joy**, **Haskell**, **Idris**, **Rust**, **Lisp**, and symmetric monoidal category theory. This repo is the WIP implementation of these ideas. If you are interested, please support the development of Mirth on [Patreon](https://patreon.com/typeswitch)! Thank you for your time and generosity.

Directory structure:

- `src`: Source code for compiler, written in Mirth.
- `bootstrap`: Bootstrap for compiler, i.e. a minimal Mirth interpreter.
- `formal`: Formal models of mirth.
- `tools`: Development tools.

  - `tools/mirth-vim`: Mirth syntax highlighting for Vim. To use, install Pathogen, then run `make install-vim`.
  - `tools/mirth-code`: Mirth syntax highlighting for VS Code. The extension is not yet published in the Marketplace, but you can install it locally. To do so, run `make install-code`. (You will need `code` in your `PATH`. That should set up automatically by VS Code on Windows and Linux. To set that up on Mac, [please follow these official instructions](https://code.visualstudio.com/docs/setup/mac).)

## Issues and Contribution

Please use [GitHub issues](https://github.com/mirth-lang/mirth/issues) for bug reports and requests.

To contribute directly, open a pull request on [the GitHub repo](https://github.com/mirth-lang/mirth). Files must be contributed under the MPL2.0 license (see below). Add the following license notice at the top of any new files:

> This Source Code Form is subject to the terms of the Mozilla Public
> License, v. 2.0. If a copy of the MPL was not distributed with this
> file, You can obtain one at https://mozilla.org/MPL/2.0/.

If you see something that can be improved, please contribute!

## License

The entire code base is licensed under the Mozilla Public License 2.0.

This license gives anyone the right to view, share, modify, and contribute source code, as long as the license notice is preserved at the top of every source code file. The code base (or portions and/or modifications thereof) can be used or distributed as part of a larger project, as long as the license notice is preserved, or in binary form, as long as source code is made available to the user. For the avoidance of doubt, this paragraph is merely informative and only the license text matters.

Please read the license text under `LICENSE.txt` or [here](https://mozilla.org/MPL/2.0/) or [here](https://choosealicense.com/licenses/mpl-2.0/).
