CFRP Compiler
======

CFRP is a typed functional reactive programming language aimed at small-scale embedded systems.
This compiler translates CFRP programs into C++.


## Installation

Use the attached script as follows. It can avoid the dependency problems that may arise when installing the peggy package (thanks to Shohei Yasutake).

    $ sh install.sh


## Usage

The current version of the compiler produces its output to stdout.

    $ cfrp your_program.cfrp > your_program.cpp


## Running the sample application

    $ cd sample
    $ make
    $ ./sample1


## Documents

Documents (user's manual, language specification, etc.) are under preparation.


## History

CFRP was initially developed by Kohei Suzuki for his Master's thesis
at Tokyo Institute of Technology (Department of Computer Science) in 2014.
