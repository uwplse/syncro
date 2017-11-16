# Syncro

Syncro uses program synthesis to automatically generate incremental update rules that can speed up programs by orders of magnitude.

## Installation

* Install [Racket](https://download.racket-lang.org/)
* Install Rosette 2

      $ raco pkg install rosette

* Install the `graph` package

      $ raco pkg install graph

* Clone this repository and install it using `raco`:

      $ cd incremental
      $ ls
      README.md	example		info.rkt	src
      direct-example	incremental	scratch
      $ raco pkg install

## Using Syncro

To use Syncro, you first write a program using the incremental language, which means your file must start with `#lang incremental`. Once you have written such a program, it can be run by Racket directly. For example, to run the provided examples:

    $ racket example/test/sketch.rkt

Each such program automatically supports various command line flags, which you can learn about through `--help`:

    $ racket example/test/sketch.rkt --help

## Current Limitations

* The `basic` grammar (`-g` option) only supports a small subset of the language that other grammars support.
* The `#:initialize` and `algorithm` parts of the program do nothing. They are placeholders that will eventually be used in code generation.
* The `#:sketches` construct and SSA grammar (`-g "(ssa n)"` option) do not play well together. The code will work but will likely be slow.
* There is experimental support for metasketches, using the Synapse library. This has been commented out so that no dependencies are introduced, but any references to metasketches in the code are dead code and can be ignored.