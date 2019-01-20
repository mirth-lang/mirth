<!--
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-->

# Mirth

**Mirth** is a new type-safe concatenative functional programming languague. Mirth was inspired by **Forth**, **Joy**, **Haskell**, **Idris**, **Rust**, **Lisp**, and symmetric monoidal category theory. This repo contains all the interpreters, compilers, (in)formal models, libraries, docs and examples for Mirth.

This is very much an early-stage WIP at this point. Mirth exists conceptually, but not as a working language yet. In my spare time, I'm working to change this. If you are interested, please support the development of Mirth on [Patreon](https://patreon.com/mirth_lang)! Thank you for your time and generosity.

Directory structure:

- `mirth`: The mirth compiler / interpreter.
- `bootstrap`: Bootstrap for compiler.
- `base`: Base package for mirth.
- `docs`: Documentation and tutorials.
- `examples`: Examples of Mirth (aspirational -- don't try to run this).
- `tests`: Language tests.
- `formal`: (In)formal models of mirth and related systems.

## License

The entire code base is licensed under the Mozilla Public License 2.0.

This license gives anyone the right to view, share, modify, and contribute source code, as long as the license notice is preserved at the top of every source code file. The code base (or portions and/or modifications thereof) can be used or distributed as part of a larger project, as long as the license notice is preserved, or in binary form, as long as source code is made available to the user. For the avoidance of doubt, this paragraph is merely informative and only the license text matters.

Please read the license text under `LICENSE.txt` or [here](https://mozilla.org/MPL/2.0/) or [here](https://choosealicense.com/licenses/mpl-2.0/).

## Issue Reports

If you find a bug or a paper cut, please use the [GitHub issue tracker](https://github.com/mirth-lang/mirth/issues) to search for an existing issue. If you find your issue already, please add a comment so we know it affects you as well. If not, please submit a new issue with a descriptive title. Please do not use the issue tracker for feature requests.

## Contributing

To contribute, open a pull request on [the GitHub repo](https://github.com/mirth-lang/mirth). Anyone may contribute to this code base as long as they act in a respectful manner, are responsive to feedback, and the code is contributed under the Mozilla Public License 2.0. Add the following license notice verbatim at the top of any new files:

> This Source Code Form is subject to the terms of the Mozilla Public
> License, v. 2.0. If a copy of the MPL was not distributed with this
> file, You can obtain one at http://mozilla.org/MPL/2.0/.

If you see something that can be improved, please contribute!
