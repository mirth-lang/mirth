
# An Introduction to Mirth

This tutorial is an introduction to Mirth as it currently exists. It is written with the target audience of people who have some prior experience with concatenative programming languages, and some prior experience with statically typed programming languages. It is written with the intent of getting you set up to use Mirth as a programming language, and teaching you the language through a series of simple programs.

Mirth is a work in progress and is unstable. So although the information in this file will get you set up in the current version of mirth, it might not be necessarily true or accurate in the future. To mitigate this somewhat, I will include little notes in square brackets [like this] to let you know about changes that are planned for the future. Do take these with a grain of salt, like everything else, those plans may change as we continue to work on mirth.

If something in this file does not work in the current version of mirth in this repository, please [raise an issue](https://github.com/mirth-lang/mirth/issues) so we can fix this tutorial. Thank you!

## First Steps

First we need to download, build, and install mirth.

On Mac and Linux, assuming you have a C compiler, `make`, and `git`, the following instructions should work:

- Download the repository into a `mirth` folder: `git clone https://github.com/mirth-lang/mirth`

- Go to that directory: `cd mirth`

- Build the compiler: `make`

- (Optional) Install the compiler into `$HOME/.mirth`: `make install`

    - (Optional) Add `$HOME/.mirth/bin` to your `$PATH` variable (and add this line to your bash profile): `export PATH="$HOME/.mirth/bin:$PATH"`

- (Optional) Install an editor plugin that provides syntax highlighting:

    - Vim: Make sure you have Pathogen installed and run: `make install-vim`
    - VSCode: Make sure you have `code` in PATH and run: `make install-code`
    - Atom: Make sure you have `apm` in PATH and run: `make install-atom`

  These editor plugins are located in `tools`. If your preferred editor is not listed, you could try making a syntax highlighting plugin based on the existing ones, and we can add it to the repository -- contributions are always welcome!

On Windows, you can install MSYS2 and try the above instructions inside of bash (after installing `gcc`, `make`, `git` packages). Alternatively, you can compile Mirth using the Visual C compiler. The C file for the Mirth compiler is located at `bin/mirth0.c` in the repository (see next section). You can see how we automate Visual C builds using `cl` in the batch scripts `tools/build.bat`, `tools/build64.bat`. You will probably need to adjust the build setup if you are on Windows, but you may find those scripts useful as a reference. If you can help us improve the build situation on Windows, please contribute!

The rest of the tutorial is written with the assumption that:

- You can run programs in a terminal.
- That the mirth compiler can be evoked from the terminal using the `mirth` command.
- That the C compiler can be evoked from the terminal using the `gcc` command, and behaves like gcc or clang would.

If you have a different setup, you may have to adjust the instructions accordingly.

### Aside: Some Background Information About Building The Mirth Compiler

Mirth is a self-hosting compiler written in Mirth. There is no stable version of Mirth currently, so when we work on Mirth we are also modifying the compiler that we use to compile Mirth itself. Normally, this cyclic build dependency would be an issue, but Mirth solves it by compiling down to C, and then we save the C compiler output in the git repository. This is `bin/mirth0.c`.

So when we work on the Mirth compiler, we build Mirth in several stages (helpfully automated via `make`):

- We build the `bin/mirth0` compiler from the C source file `bin/mirth0.c`
- We use `bin/mirth0` to build the Mirth compiler from mirth code (in `src` and `lib`), producing `bin/mirth1.c`.
- We build the `bin/mirth1` compiler from the C source file `bin/mirth1.c`.
- We use `bin/mirth1` to build Mirth again, producing `bin/mirth2.c`.
- We build `bin/mirth2` from the C source file `bin/mirth2.c`.
- We use `bin/mirth2` as the main Mirth compiler for testing purposes.

When a change is made to the Mirth compiler source, this change is going to affect both the `bin/mirth1` and `bin/mirth2` compilers. In order to commit back into the repository, we run a test that verifies that `bin/mirth0.c` and `bin/mirth1.c` and `bin/mirth2.c` are exactly the same. As part of running `make` it will also output an abbreviated `diff` of the C outputs so you can also see discrepancies there. When we have a change we want to make part of mirth, we use `make update` to copy the latest `bin/mirth2.c` over to `bin/mirth0.c`, and then we verify that a fixed point is reached by running `make` again. Thus we have handled the cyclic build dependency.

Although this process introduces some complexity, it does mean we can iterate quickly on the Mirth compiler. Once we release a stable version of Mirth, we will revisit this approach.

## Mirth Projects and Packages

A mirth project consists of some mirth code organized in such a way that the mirth compiler knows where to find all of the packages.
Typically, a Mirth project is a folder will contain at least three subfolders:

- A `src` folder containing the current project's source files.
- A `bin` folder containing the current project's output files (both executable binaries and intermediate C files generated by the Mirth compiler).
- A `lib` folder containing the source code for other Mirth packages that this project depends on.

Of these, the most important is `lib`, because that is where the Mirth compiler will look for other packages by default.

A mirth package is a folder containing mirth modules (and possibly other files). A mirth module is a mirth source file, which uses a `.mth` file extension. The mirth compiler expects all source code files to be text encoded via UTF-8, although this is not strictly enforced.

Let's create a mirth project! We have to do this manually.

1. Create a folder for your mirth project. For example, `learning-mirth`.
2. In that folder, create the `src`, `bin`, and `lib` folders.
3. We are going to need the `std` package, so copy that package over from the mirth compiler repository (`lib/std`) into your `lib` folder.

(If you installed mirth via `make install`, a copy of the `std` package is also available in `$HOME/.mirth/lib`. But for now you still have to copy the `std` package into your repository manually, to follow this tutorial.)

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

### Aside: Mirth Compiler Flags

For reference, the mirth compiler accepts the following flags:

- `-o PATH` or `--output-path PATH`: Set the path for the C output file. Either this or `-c` are required. For example: `-o bin/hello.c`

- `-c` or `--compile-only`: Typecheck the code without generating C code. Either this or `-o` are required. [This flag name is misleading, it will be changed.]

- `-e ENTRY_POINT` or `--entry-point ENTRY_POINT`: Override the default entry point. For example: `-e main`

- `-p pkg:PATH` or `--package PACKAGE:PATH`: Override the path to a single mirth package. For example: `-p std:lib/std`

- `-P PATH` or `--package-search-path PATH`: Override the default package search path (the default is `lib`), which determines where mirth looks for other packages. You can pass multiple of these to search in multiple folders. For example: `-P lib -P ~/.mirth/lib`

Besides requiring either the `-o PATH` or `-c` flag, the path to a mirth module containing the entry point must also be passed. For example, `mirth src/hello.mth -o bin/hello.c`

## Next Steps

[More to come.]
