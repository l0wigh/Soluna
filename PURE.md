# Internal Pure Functions

This is a short explaination of what I call an Internal Pure Functions.

Adding a primitive (internal -> OCaml) function can be a pain sometimes. That's the reason IPF exists.

The idea is simple, since creating a function in Soluna modify the Global Env, why not do it myself in the code.

The benefits are that if a primitive is easier to write in IPF (Soluna dialect), I can just implement it and load it inside of the Soluna core.

This has a cost and should never be abused. The cost is performance. If I add too much IPF, it will slow down Soluna at startup. If I add unoptimized IPF, it will be slow to use them.

## List of IPF

- reduce
