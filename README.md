# Soluna 0.7.0

<p align="center" width="100%">
    <img src="https://raw.githubusercontent.com/l0wigh/Soluna/refs/heads/master/logo_alchemy.gif">
</p>
<!-- ![Alchemy illustration](./logo_alchemy.gif) -->

Soluna is a Lisp dialect interpreted in OCaml.

It does not aim to be perfect, to be accurate, to be easy to use. It aims to be my own Alchemy lab to invoke Gods and Space creatures.

Originaly made as a challenge in a challenge. I wanted to do a Lisp dialect in which I would then solve at least one AoC (2025) problem.

The documentation can be found [here](https://github.com/l0wigh/Soluna/blob/master/DOCUMENTATION.md)

## What's builtin ?

- lambda and function: functions is basically syntactic sugar for lambdas
- do: let you do multiple operations and yet return only the last expression ! (trust me it's useful)
- list: let you create a list
- if: simple if else block
- map: apply a function to a list to get a new list
- filter: apply a function that returns a boolean and create a function only with elements that output as true.
- macros: let you create code template to extend language capacities
- ...: other stuff can be found in the [Language Reference](https://github.com/l0wigh/Soluna/blob/master/LANGUAGE.md)

You need to build the missing tools by yourself, that's the fun part.

## Oracle REPL

<p align="center" width="100%">
    <img src="https://raw.githubusercontent.com/l0wigh/Soluna/refs/heads/master/oracle_2.png">
</p>

You can use the builtin REPL called Oracle when starting Soluna without argument.

The code for Oracle is present [here](https://github.com/l0wigh/Soluna/blob/master/examples/Oracle.luna)

Also protips, use **rlwrap** to expand the capabilities of Oracle.

## Soluna Bundler

Soluna is equipped with a bundler. The way it works is simply by injecting your source code inside Soluna's one, then compile everything.

The executable is in fact the Soluna interpreter running only your code source. It's far from perfect but will do the trick to add some portability.

For the bundler to work you need to have OCaml tools installed (look at the "Build and run" section to know how to install everything).

## What's to come ?

I don't really know exactly what I'll want and what I'll do. Here is a list that I will follow (or not):

- Fix the slow 'cons' function
- More high order functions (fold, ...)
- Sexp runtime reading functions
- Step by step debugger

Again, most of this will never be made since it doesn't feel necessary to me.

## Neovim configuration

### Plugin to cast spell faster

I built a Neovim plugin to help you do more faster using Soluna. Take a look [here](https://github.com/l0wigh/soluna.nvim)

### Automatic coloring (by cheating)
```lua
vim.cmd [[
	autocmd BufNewFile,BufRead *.luna :set filetype=lisp
]]
```

For vim, just takes what is inside the vim.cmd block

## The OCaml code is... interesting

Yep, might be dirty. I'm not the best in this language, but it works really nice.

I might want to clean the code, at some point. For now it works nice and I want to add some more stuff before.

## Build and run

You need to install OCaml and Dune. Look at the [official website](https://ocaml.org/install) to know how to install all this.

You can then do :
- build: `dune build`
- run: `dune exec soluna`
- install: `dune install`

After a dune install, you'll be able to use `soluna <filename>`.

Binary file will be present in the `_build` directory.

## Other informations

There is some code example. File extension is luna. Good luck and have fun.
