# Getting Started

Before we can use Mirth, we must build and install the Mirth compiler.
This chapter covers how to do that, depending on your operating system.

## Mac and Linux

On Mac and Linux, first ensure you have a C compiler (`gcc` or `clang`), `make`, and `git`.

The following instructions should work:

**Step 1.** Download the repository into a `mirth` folder, and open that folder:

    git clone https://github.com/mirth-lang/mirth
    cd mirth

**Step 2.** Build the compiler:

    make

**Step 3.** Install the compiler into `~/.mirth`:

    make install

**Step 4.** Add `$HOME/.mirth/bin` into your `PATH` variable. Use this line in your shell profile:

    export PATH="$HOME/.mirth/bin:$PATH"

Then restart your shell. If this was done correctly, you will have access to the `mirth` executable.

**Step 5.** (Optional) Install an editor plugin that provides syntax highlighting:

- **Vim:** Make sure you have Pathogen installed and run: `make install-vim`
- **VSCode:** Make sure you have `code` in your `PATH` and run: `make install-code`
- **Atom:** Make sure you have `apm` in your `PATH` and run: `make install-atom`

The source for these editor plugins is located in the `tools` folder in the Mirth repo. If your preferred editor is not listed, you could also try making your own syntax highlighting rules based on the existing ones. [If you do, consider contributing it back into the Mirth repo so others can use it and so we can help maintain it as the language continues to evolve.]

Mirth is now installed and ready to use. Eventually, you may wish to update mirth, or to remove mirth from your system. To update mirth, download the latest version of the repo (`git pull`), and then do steps 2 and 3 again (`make && make install`). To remove mirth, delete the repo and delete the `~/.mirth` folder (`rm -r ~/.mirth`).

## Windows

On Windows, you can install MSYS2 or another Unix-like environment for Windows. Make sure you have `gcc`, `make`, and `git`, and then follow the same instructions above as for Mac and Linux.

Alternatively, you can compile Mirth natively using the Visual C compiler. The C file for the Mirth compiler is located at `bin/mirth0.c` in the Mirth repo. You can see how we automate Visual C builds using `cl` in the batch scripts `tools/build.bat`, `tools/build64.bat`. You may need to adjust it to your setup, but you may find those scripts useful as a reference. [If you can help us improve the build situation on Windows, please contribute!]

The rest of the tutorial is written with the assumption that:

- You can run programs in a terminal.
- That the mirth compiler can be evoked from the terminal using the `mirth` command.
- That the C compiler can be evoked from the terminal using the `gcc` command, and behaves like gcc or clang would.

If you have a different setup, you may have to adjust the instructions accordingly.

## Footnote: How The Mirth Compiler is Built

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

**Next:** [Hello World!](02-hello-world.md)
