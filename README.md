# hu.dwim.tablegen-to-assembler

## What

An experiment to parse the json output of LLVM's TableGen, with the
ultimate goal of generating an x86_64 assembler.

## Status

**It's an alpha nothingburger.**

It can parse the json version of X86.tb as a stream, and can
walk/filter the instructions. Nothing is generated for now.

## Why

My ultimate goal is an x86_64 assembler for
[Maru](https://github.com/attila-lendvai/maru).

But it's easier to work in a mature language with Slime, so I'm hoping
to finish this CL lib and then either:

  - port this to Maru (ideal outcome),

  - or simply generate the assembler for Maru and check it in into its
    repo (a less pleasing but more pragmatic shortcut).

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.tablegen-to-assembler).
