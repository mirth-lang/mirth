# Mirth

![build](https://github.com/mirth-lang/mirth/workflows/build/badge.svg)

**Mirth** is a new concatenative programming language. Mirth is inspired by **Forth**, **Joy**, **Haskell**, **Lisp**, and monoidal category theory.

This repository is a **work-in-progress** implementation of Mirth. If you are interested, please support the development of Mirth on [Patreon](https://patreon.com/typeswitch). Thank you for your time and generosity.

### Hello, world!

```mirth
||| A simple hello world program to show off the mirth language.
module(examples.hello-world)
import(prelude)
import(platform.posix)

target-c99("hello-world.c",
    "Hello, world!" str-print-ln!)
```

## Tools

| Editor       | Source | Install                                                                                                                    | Syntax highlighting |
| ------------ | ------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------- |
| Vim          | `tools/mirth-vim` | To install via Pathogen,           </br> run `make install-vim`.                                                           | :heavy_check_mark:  |
| VS Code      | `tools/mirth-code` | Ensuring `code` is in your `PATH`, </br> run `make install-code`. </br> Not yet published in the Marketplace.              | :heavy_check_mark:  |
| Atom         | `tools/mirth-atom` | Ensuring `apm` is in your `PATH`,  </br> run `make install-atom`. </br> Not yet published in the atom.io package registry. | :heavy_check_mark:  |

## License

This software is licensed under a [BSD Zero Clause License](https://en.wikipedia.org/wiki/BSD_licenses#0-clause_license_(%22BSD_Zero_Clause_License%22)). This is a public domain equivalent license. You can use, copy, modify, and/or distribute, with or without fee. You can use it freely as part of larger projects, commercial or otherwise. No warranty is implied. See `LICENSE` for the full text of the license.
