# hu.dwim.tablegen-to-assembler

## What

An experiment to parse LLVM's TableGen file format, with the ultimate
goal of generating an assembler for x86_64.

## Status

**It's a pre-alpha nothingburger.** It can almost parse the X86.tb file,
but I have abandoned it because I'm now looking into doing the same
but from
[Sleigh](https://ghidra.re/ghidra_docs/languages/html/sleigh.html)
(part of [Ghidra](https://github.com/NationalSecurityAgency/ghidra)).

LLVM's TableGen DSL seems to be the wrong choice in 2025. It also
smells too much like C++ (i.e. too much accidental complexity).

## Why

I need an x86_64 assembler for
[Maru](https://github.com/attila-lendvai/maru), and I'm hoping to
later port this experiment to Maru.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.tablegen-to-assembler).
