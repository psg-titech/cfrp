CFRP Compiler
======

CFRP is a typed functional reactive programming language aimed at small-scale embedded systems.
This compiler translates CFRP programs into C++.

## Installation

Use the attached shell script as follows. It can avoid the dependency problems that may arise when installing the peggy package (thanks to [Shohei Yasutake](https://github.com/amutake)).

    $ sh install.sh

## Usage

The current version of the compiler produces its output to stdout.

    $ cfrp your_program.cfrp > your_program.cpp

## Running sample applications

    $ cd sample
    $ make
    $ ./sample1

## Documents

Documents (user's manual, language specification, etc.) are under preparation.

## History

CFRP was originally designed and developed by [Kohei Suzuki](https://github.com/eagletmt) for his Master's thesis at the Department of Computer Science, Tokyo Institute of Technology in 2014.
