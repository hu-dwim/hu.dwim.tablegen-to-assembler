# hu.dwim.genassem

## What

A suite to parse the json output of LLVM's TableGen, and use it to
generate an assemblers.

## Status

**It's work in progress.**

It can parse the json version of X86.tb as a stream, and can
walk/filter the instructions. Work is underway to generate the x86_64
assembler.

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
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.genassem).
