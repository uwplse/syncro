To run the code:

- Install Rosette 2 and Graph using `raco`
- Run `racket grades.rkt` under `example/test`, or any other example you want.

Currently command line options are not working properly -- instead, you need to change the values in `src/racket/cmd-parse.rkt`. Descriptions of what each parameter does can be found in that file.

In particular, there are multiple kinds of grammars that can be used:
- Basic grammar: This only has support for a subset of data structures, primarily vectors and arithmetic. In particular, anything involving sets or graphs will not work.
- General grammar: A grammar that can be easily extended with new operators. All examples should work with the general grammar.
- Caching grammar: The same as the general grammar, but with a caching optimization. All examples should work with the caching grammar, and they should be slightly faster than the general grammar. Use this by default.
- SSA grammar: An experimental grammar that tries to generate programs in something like SSA form in order to make synthesis more tractable. It is currently still under development and examples may not work with it; however it is typically faster than the other grammars when it does work.

In addition, the basic and general grammars can use the "sharing" option, which allows them to share boolean variables among different subtrees of the AST. This option is not compatible with the caching grammar or the SSA grammar.

There is experimental support for metasketches, using the Synapse library. The default version of the Synapse library only works with Rosette 1, but there is a branch that has a partial port to Rosette 2, and that's what this repository requires. As of March 24, 2017 the parts of the code that use Synapse have been commented out so you should not need to install them.

