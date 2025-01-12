# Hello World!

In this chapter we build and run our first Mirth program. To achieve this, we must first create a Mirth project, which we will use throughout the rest of the tutorial, and then we can start building programs.

## Modules, Packages, and Projects

A Mirth module is a Mirth source code file. These are text files with a `.mth` file extension. Mirth modules use the UTF-8 unicode text encoding.

A Mirth package is a folder containing many Mirth modules. (In the future, it may also contain subpackages.)

A Mirth project is a folder organized in such a way that the Mirth compiler knows where to find all of the relevant modules and packages. Typically, a Mirth project will contain at least three subfolders:

- A `src` subfolder containing the current project's source files.
- A `bin` subfolder containing the current project's output files.
- A `lib` subfolder containing the source code for other Mirth packages that this project depends on.

Of these, the most important is `lib`, because that is where the Mirth compiler will look for other packages by default.

## Creating A Mirth Project

Let's create our mirth project! Currently, we have to do this ourselves, there is no automation. (In the future, we may have some automation.)

Create a folder for the mirth project. You can name this anything you like, but we will call it `learning-mirth` in this tutorial:

    mkdir learning-mirth
    cd learning-mirth

In this folder, create `src`, `bin` and `lib` folders:

    mkdir src
    mkdir bin
    mkdir lib

We are going to need the `std` package, so let's copy it over from our mirth installation. (In the future, this step will be unnecessary, and you will be able to use the `std` package directly from your mirth installation.)

    cp -r ~/.mirth/lib/std lib/std

And we're done! Our mirth project is ready to use, and we can start developing programs in mirth.

## Building & Running Your First Program

Now that we are done setting up, let's create a "Hello world!" program in mirth.

Create a file `src/hello.mth` and add the following code:

```
module learning-mirth.hello

import std.prelude
import std.world

def main {
    "Hello world!" print
}
```

Now let's build and run this code in a terminal:

- First compile the Mirth code into C: `mirth src/hello.mth -o bin/hello.c`
- Then compile the C code into an executable: `gcc bin/hello.c -o bin/hello`
- Then run the executable: `bin/hello`

If all succeeded, you should see the text `Hello world!` in your terminal.

Let's analyze the program line by line.

- `module learning-mirth.hello`: This is the module header. Every mirth module must have a header like this, it tells the compiler two things:

  - What package this module is part of: `learning-mirth`
  - What the name of the module is: `hello`

  The module name needs to match the file name (`hello.mth`).
  The package name does not, because this is the `src` folder, but if you have other modules in `src` they need to share the same package name.

- `import std.prelude`: This line will import the `prelude` module from the `std` package, which is in `lib/std/prelude.mth`, so you can use the types and definitions defined in the `std.prelude` module. Although this is technically optional for this example, almost every mirth module will import `std.prelude` because it exposes a lot of useful definitions. [In the future, this module might be imported automatically by default.]

- `import std.world`: This line will import the `world` module from the `std` package. This module exposes definitions for communicating with the world outside of the program, for example, for printing things to the terminal. We need this for our "Hello world!" program.

- `def main {`: This begins a definition of the `main` word (AKA the `main` function, the `main` procedure). This `main` is the entry point for the entire program.

- `"Hello world!" print`: This pushes the string `"Hello world!"` onto the stack, and then calls `print` which pops the string and prints it to standard output, along with a newline.

- `}`: This ends the definition for the `main` word.

When we compile this module using `mirth src/hello.mth -o bin/hello.c`, the Mirth compiler determines that `src/hello.mth` is the main module source file, so it will invoke the corresponding `main` word as its entry point. We could override this by passing the `-e` compiler flag.

## Footnote: Mirth Compiler Flags

For reference, the mirth compiler accepts the following flags:

- `-o PATH` or `--output-path PATH`: Set the path for the C output file. Either this or `-c` are required. For example: `-o bin/hello.c`

- `-c` or `--compile-only`: Typecheck the code without generating C code. Either this or `-o` are required. [This flag name is misleading, it will be changed.]

- `-e ENTRY_POINT` or `--entry-point ENTRY_POINT`: Override the default entry point. For example: `-e main`

- `-p pkg:PATH` or `--package PACKAGE:PATH`: Override the path to a single mirth package. For example: `-p std:lib/std`

- `-P PATH` or `--package-search-path PATH`: Override the default package search path (the default is `lib`), which determines where mirth looks for other packages. You can pass multiple of these to search in multiple folders. For example: `-P lib -P ~/.mirth/lib`

Besides requiring either the `-o PATH` or `-c` flag, the path to a mirth module containing the entry point must also be passed. For example, `mirth src/hello.mth -o bin/hello.c`


**Next:** [Values and Types](03-values-and-types.md)
