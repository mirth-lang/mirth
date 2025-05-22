# Mirth

**Mirth** is a strongly-typed concatenative programming language that is currently in development.

Mirth is inspired by **Forth**, **Joy**, **Haskell**, **Lisp**, and monoidal category theory.

Useful Links:

- Main repository: [git.sr.ht/~typeswitch/mirth](https://git.sr.ht/~typeswitch/mirth)
- GitHub mirror: [github.com/mirth-lang/mirth](https://github.com/mirth-lang/mirth)
- Issue tracker: [todo.sr.ht/~typeswitch/mirth](https://todo.sr.ht/~typeswitch/mirth)

If you are interested in Mirth, please support the development of Mirth on [Patreon](https://patreon.com/typeswitch). Thank you for your time and generosity.

### Hello, world!

```mirth
||| A simple hello world program to show off the Mirth language.
module examples.hello-world
import std.prelude
import std.world

def main [ +World -- +World ] {
    "Hello, world!" print
}
```

## Tools

| Editor       | Source | Install                                                                                                                    | Syntax highlighting |
| ------------ | ------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------- |
| Vim          | `tools/mirth-vim` | To install via Pathogen,           </br> run `make install-vim`.                                                           | ✔️  |
| VS Code      | `tools/mirth-code` | Ensuring `code` is in your `PATH`, </br> run `make install-code`. </br> Not yet published in the Marketplace.              | ✔️ |
| Atom         | `tools/mirth-atom` | Ensuring `apm` is in your `PATH`,  </br> run `make install-atom`. </br> Not yet published in the atom.io package registry. | ✔️ |

## License

This software is licensed under a [BSD Zero Clause License](https://en.wikipedia.org/wiki/BSD_licenses#0-clause_license_(%22BSD_Zero_Clause_License%22)). This is a public domain equivalent license. You can use, copy, modify, and/or distribute, with or without fee. You can use it freely as part of larger projects, commercial or otherwise. No warranty is implied. See `LICENSE` for the full text of the license.
