# Soluna 0.1.2

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
- do: let you do multiple operations and yet return nothing ! (trust me it's useful)
- list: let you create a list
- fst: gives you the first item of a list
- rst: gives you the list without the first element
- if: simple if else block
- map: apply a function to a list to get a new function

You need to build the missing tools by yourself

## What's to come ?

I don't really know exactly what I'll want and what I'll do. Here is a list that I will follow (or not):

- Fix the slow 'cons' function
- REPL
- More high order functions (filter, fold, ...)
- Sexp runtime reading functions
- File reader (This would be specifically to help solving AoC problems)
- Step by step debugger

Again, most of this will never be made since it doesn't feel necessary to me.

## Neovim automatic filetype on luna files

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

You need to install ocaml and dune. Look at the [official website](https://ocaml.org/install) to know how to install all this.

You can then do :
- build: `dune build`
- run: `dune exec soluna`

Binary file will be present in the `_build` directory.

## Other informations

There is some code example. File extension is luna. Good luck and have fun.
