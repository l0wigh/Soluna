# Soluna Language Reference

Version described: Soluna `0.10.4` (`soluna_version` in `bin/main.ml`).

This reference describes the interpreter in `bin/main.ml`. Behaviour was checked against that source and against the files in `examples/` and `examples/tests/`. The examples were not executed for this revision. Points marked **to confirm** are read from the source but not yet checked by a run.

---

## 1. Lexical Elements

### 1.1 Comments

A `;` starts a comment that runs to the end of the line.

```lisp
; This whole line is a comment.
(writeln "visible") ; So is this part.
```

If the first line of a file starts with `#!`, it is ignored. This allows executable scripts.

### 1.2 Atoms

| Kind | Syntax | Notes |
| ---- | ------ | ----- |
| Integer | `42`, `-7` | OCaml native `int`. |
| Float | `3.14`, `-0.5`, `.5` | A token is read as a Float only if it contains a `.`. `1e3` is read as a Symbol. |
| Boolean | `true`, `false` | |
| String | `"hello"` | Escapes recognised: `\n`, `\t`, `\\`, `\"`. Any other backslash sequence keeps its backslash. |
| Keyword | `:red`, `:overwrite` | A symbol starting with `:` evaluates to the String with the same text, colon included. Used for options such as colours and write modes. |
| Empty list | `()` or `nil` | Both read as the same empty list. Its type is `"Nil"`. It is false in `if`. |
| Symbol | `x`, `my-list`, `even?`, `+` | Any token that is not one of the atoms above. |

### 1.3 Lists

Lists are written with parentheses:

```lisp
(expr1 expr2 expr3)
```

The first element decides how the list is evaluated (see section 2). Lists are also data: `quote` and macros manipulate them without evaluating them.

### 1.4 Reader Shorthands

| Written | Read as |
| ------- | ------- |
| `'x` | `(quote x)` |
| `` `x `` | `(quasiquote x)` |
| `,x` | `(unquote x)` |
| `,@x` | `(unquote-splicing x)` |

The single quote is only a reader prefix. It does not start a character literal. `'h'` is not a character.

---

## 2. Evaluation Model

### 2.1 Values

* Numbers, booleans, strings and keywords evaluate to themselves.
* The empty list evaluates to itself.
* A symbol evaluates to the value bound to it in the current environment. An unbound symbol is an error: `Unbound symbol '...'`.

### 2.2 Lists

A list whose first element is:

1. **a special form name** (`if`, `defvar`, `let`, ...): the operands are not evaluated before the form decides what to do with them;
2. **a macro**: the operands are passed unevaluated; the macro body runs, and its result is evaluated in the calling environment;
3. **anything else**: the first element is evaluated, then the remaining elements are evaluated left to right. The result is applied as a built-in or as a function. A non-function value in head position is an error.

Special form names are recognised by name. Calls written with a special form name always use the special form, even if a function of that name has been defined. The special forms are listed in section 3.

Built-in functions are ordinary values bound in the global environment. A program can rebind them. `examples/functions.luna` defines its own `filter`, and calls to `filter` then use that definition.

### 2.3 Environments and Scope

`defvar` adds a binding to the current environment (see section 3.1). The environment changes according to the form that encloses the code:

| Form | Scope of bindings created inside it |
| ---- | ----------------------------------- |
| top level, `do`, `while`, `each`, `if` branches | Same environment as the enclosing code. A `defvar` here stays visible after the form ends. |
| `function`, `lambda` call | A new scope whose parent is the environment where the function was defined. Bindings made inside stay local; a function sees the current state of the scope it was defined in, and `set!` can change the enclosing bindings. |
| `let`, `let*` | A new scope, child of the current one. Bindings made inside stay local; `set!` can change the enclosing bindings. |

`examples/mutation.luna` shows the function case: a `defvar x` inside a function does not change the global `x`.

`examples/while.luna` shows the loop case: a `defvar` inside the body of a `while` changes the variable used by the loop condition.

---

## 3. Special Forms

### 3.1 Definitions

#### `defvar`

```lisp
(defvar name value)
```

Binds `name` to the evaluated `value` in the current environment, and returns the value.

Redefining a name that already exists in the same environment does not raise an error. The new binding replaces the previous one (see section 8).

#### `set!`

```lisp
(set! name value)
```

Changes the binding of `name` to the evaluated `value`, in the nearest scope where `name` is already bound — the current scope, an enclosing one, or the top level. Returns the value. If `name` is not bound anywhere, it is an error: `set!` only changes an existing binding, use `defvar` to create one.

#### `function`

```lisp
(function name (param1 param2 ...) body)
```

Creates a function, binds it to `name` in the current environment, and returns it. `body` is a single expression. Use `do` for several expressions.

Recursion works, because the name is bound before any call is made.

```lisp
(function square (x) (* x x))
(square 4) ; 16
```

#### `lambda`

```lisp
(lambda (param1 param2 ...) body)
```

Creates an anonymous function value. The value is not bound to a name.

```lisp
(defvar double (lambda (x) (* x 2)))
(double 21) ; 42
```

#### Parameters and `&rest`

The parameter list holds symbols. `&rest name`, when it is the last entry, collects all remaining arguments into a list bound to `name`.

Calling a function with too many or too few arguments is an error: `Too much arguments` or `Not enough arguments`.

#### `defmacro`

```lisp
(defmacro name (param1 param2 ...) body)
```

Defines a macro. See section 4. The form returns the macro name as a symbol.

---

### 3.2 Control Flow

#### `if`

```lisp
(if condition then-expr else-expr)
```

Exactly three operands are required. Only `false` and the empty list (`()`, `nil`) are false. Everything else is true, including `0` and `""`.

```lisp
(if (> x 5) (* x 2) x)
```

#### `do`

```lisp
(do expr1 expr2 expr3)
```

Evaluates the expressions in order and returns the value of the last one. `(do)` returns the empty list.

#### `while`

```lisp
(while condition body)
```

Repeats `body` while `condition` is not `false`. The loop does not stop on the empty list (see section 8). Returns the empty list.

`examples/while.luna`:

```lisp
(defvar start 1)
(defvar end 11)
(while (< start end) (do (writeln start) (defvar start (+ start 1))))
```

#### `case`

```lisp
(case
  (test1 expr1)
  (test2 expr2)
  (default default-expr))
```

Each clause is a `(test expression)` pair. The tests are evaluated in order. The first test that is not `false` selects its expression. `default` must be the last clause, and is optional. If no clause is selected, the form is an error.

`examples/cases.luna`:

```lisp
(function check_sign (x) (case
	((> x 0) "Positive")
	((< x 0) "Negative")
	(default "Zero")
))
```

#### `match`

```lisp
(match value
  (pattern expr)
  (pattern (when guard) expr)
  (default expr))
```

Each clause is a list: a pattern, an optional guard, and one body expression. The clauses are tried in order. The first pattern that matches runs its body. If no pattern matches, the form is an error (`No pattern matched the value`).

Pattern rules:

| Pattern | Matches |
| ------- | ------- |
| `42`, `true`, `"text"` | The same Integer, Boolean or String. |
| `()` | The empty list, or the empty string `""`. |
| `(quote name)` | The Symbol whose name is `name`. `'name` reads as the same pattern. |
| `(p1 p2 ...)` | A list of the same length whose elements match `p1`, `p2`, ... |
| `(head :: tail)` | A non-empty list (`head` is the first element, `tail` is the rest) or a non-empty string (`head` is its first character as a String, `tail` the rest of the string). |
| `name` | Any value. The value is bound to `name` in the clause body. |
| `_` | Any value. Nothing is bound. |
| `default` | Any value. Nothing is bound. Use it as the last clause. |
| `(any p1 p2 ...)` | Any value that matches one of `p1`, `p2`, ... |
| `(p as name)` | A value that matches `p`. The whole value is also bound to `name`. |

A guard `(when condition)` must be evaluated to `true` for the clause to be taken. If the guard is not `true`, matching continues with the next clause.

`examples/match.luna`, function `analyse-move`:

```lisp
(function analyse-move (move)
	(match move
		(("expense" :: value) (when (> (fst value) 100))
			"Excessive Expense !")
		(("income" :: value) (when (= (null value) false)) "More Money !")
		(_ "Suspicious Transaction")
	)
)
```

`examples/match-as-any.luna`:

```lisp
(match "NORTH"
  ((any "NORTH" "UP" "N") (writeln "Direction: Nord")))
```

#### `try`

```lisp
(try expr (error-name handler-expr))
```

Exactly this shape is required: one protected expression, and one clause with a name and a handler. If `expr` fails, the error message is bound to `error-name` as a String, and `handler-expr` is evaluated.

`examples/bind.luna` and `examples/defvar_macro.luna` use `try` around code that fails on purpose, and print the message with `(writeln e)`.

The message contains terminal colour escape codes (section 8).

---

### 3.3 Local Bindings

#### `let`

```lisp
(let ((name1 expr1) (name2 expr2)) body...)
```

Parallel bindings. Every value is evaluated in the outer environment, then the names are bound together. The body is a sequence (like `do`), and the value of the last expression is returned. An empty body returns the empty list.

```lisp
(let ((x 5) (y 15))
  (writeln x)
  (writeln y))
```

#### `let*`

```lisp
(let* ((name1 expr1) (name2 expr2)) body...)
```

Sequential bindings. Each value is evaluated in an environment that contains the previous bindings.

```lisp
(let* ((a 10) (b (+ a 5))) (- b a)) ; 5
```

---

### 3.4 Iteration

#### `each`

```lisp
(each name list-expr body)
```

Evaluates `list-expr`, which must be a List. For each element, binds `name` to it and evaluates `body`. Returns the empty list.

`name` stays bound in the current environment after the loop, with the last element as its value. The body is a single expression. Use `do` for several.

```lisp
(each c (explode "Soluna") (do (write c) (write " ")))
```

---

### 3.5 Threading

#### `bind`

```lisp
(bind name initial-expr step1 step2 ...)
```

Threads one accumulated value through several expressions.

* `initial-expr` is evaluated first. Its value is the starting accumulator.
* Before each step, `name` is bound to the current accumulator. The step is then evaluated, and its value becomes the new accumulator.
* The value of the last step is returned.
* `name` exists only inside `bind`. The outer `name`, if any, is not changed.

`examples/bind.luna`:

```lisp
(defvar x "Shadowed variable. Still the same")
(writeln (bind x (explode "Soluna") (reverse x) (implode x)))
```

The first step reverses the list of characters, the second joins it back into a String. The result is `anuloS`. The outer `x` is unchanged.

---

### 3.6 Strings

#### `split`

```lisp
(split delimiter string)
(split delimiter string :keep-empty)
```

Special form. Both arguments are evaluated. `delimiter` must evaluate to a String. Only its first character is used. An empty delimiter splits on a space.

Returns a List of Strings. Empty pieces are removed, unless `:keep-empty` is given.

```lisp
(split "l" "Hello" :keep-empty) ; keeps the empty piece between the two l's
(split "\n" heroes)             ; one String per line
```

`examples/split.luna` shows these calls with `writeln`.

---

### 3.7 Quoting and Templates

#### `quote`

```lisp
(quote expr)
'expr
```

Returns `expr` as data, without evaluating it.

#### `quasiquote`, `unquote`, `unquote-splicing`

```lisp
`(a ,b ,@c)
```

A quasiquote is a template. Everything inside it is data, except:

* `,x` (unquote) is replaced by the value of `x`;
* `,@x` (unquote-splicing) is replaced by the elements of the list `x`, spliced into the surrounding list.

`examples/defvar_macro.luna`:

```lisp
(defvar my-list (list 2 3))
(writeln `(1 ,@my-list 4)) ; (1 2 3 4)
```

Templates are used mainly inside macros (section 4).

---

### 3.8 Modules

#### `include`

```lisp
(include "file.luna")
```

Reads, parses and evaluates another Soluna file, in the current environment. The path is relative to the directory of the file that contains the `include`. The argument must be a String literal.

The bundler (`soluna bundle` and `soluna build`) finds includes by reading the source line by line. It inlines an include only when the line, with all spaces removed, starts with `(include"`. Write each `include` alone on its own line, at the start of the line.

Includes can loop forever if two files include each other.

`examples/include.luna` includes `_include_child.luna` and calls a function it defines.

---

### 3.9 Command-line Arguments

#### `args`

`args` is a global variable. It holds a List of Strings:

| How Soluna is started | Contents of `args` |
| --------------------- | ------------------ |
| `soluna file.luna a b` | `("file.luna" "a" "b")` |
| `soluna eval "code" a b` | `("a" "b")` |
| `soluna` (Oracle REPL) | `()` |
| A bundled executable | Every command-line word, starting with the program name. |

`examples/args.luna`:

```lisp
(if (= (length args) 1)
	(writeln (concat "Only one argument found (script name: " (fst args) ")\nTry adding command line arguments"))
	(writeln args))
```

---

## 4. Macros

A macro receives its arguments as **unevaluated syntax**. Its body runs at expansion time and returns new code. That code is then evaluated in the environment where the macro was called.

### 4.1 Defining a Macro

```lisp
(defmacro name (param1 param2 ...) body)
```

* Each parameter is bound to a piece of syntax (a symbol, a number, a list, ...), not to a value.
* The body must return a value that is valid Soluna code.
* `&rest name`, as the last parameter, binds all remaining arguments as one list of syntax.

`tests/general_tests.luna` defines and uses `unless`:

```lisp
(defmacro unless (cond body) `(if (not ,cond) ,body "SKIP"))

(defvar res6 (unless false "OK")) ; res6 is "OK"
```

Here `(unless false "OK")` expands to `(if (not false) "OK" "SKIP")`. The `not` returns `true`, so the form returns `"OK"`.

### 4.2 Quasiquotation

Macro bodies build their result with a quasiquote template (section 3.7):

```lisp
(defmacro sum-all (&rest num)
  `(+ ,@num))

(sum-all 1 2 3 4) ; expands to (+ 1 2 3 4)
```

Taken from `examples/rest.luna`.

### 4.3 Expansion Model

1. The macro is called with its arguments as unevaluated syntax.
2. The macro body is evaluated. Its parameters are bound to that syntax.
3. The result is evaluated in the calling environment.

A macro body can use `defvar`, `do`, `if` and any other form. A macro body can call other macros.

A macro exists only after its `defmacro` form has run. Calling a name before that is an `Unbound symbol` error.

---

## 5. Built-in Functions

All built-in functions receive their arguments already evaluated, except the special forms listed in section 3. The types given below are checked by the interpreter. A call with the wrong types is an error.

### 5.1 Arithmetic and Comparison

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `+` | one or more Numbers | Number | `(+ 1 2 3)` → `6` |
| `-` | one or more Numbers | Number | `(- 10 3)` → `7`. With one argument, `(- 5)` returns `5`. There is no negation. |
| `*` | one or more Numbers | Number | `(* 2 4)` → `8` |
| `/` | one or more Numbers | Number | Always computed as a Float. `(/ 8 2)` → `4`, `(/ 5 2)` → `2.5`. |
| `mod` | two Integers, or two Floats | Number | `(mod 10 3)` → `1`. The sign follows the first argument. Division by zero is an error. |
| `<` `>` `<=` `>=` | two Numbers, or two Strings | Boolean | Strings are compared by character codes. |
| `=` `!=` | two Numbers, two Strings, two Booleans, two Symbols, or two lists | Boolean | Numbers are equal when they differ by less than `1e-11`: `(= 1 1.0)` → `true`. Symbols are equal when their names match. Lists: see section 8. |

**Number results.** When all arguments are Integers, the result is an Integer. If any argument is a Float, the result is a Float. A Float result with no fractional part is returned as an Integer: `(+ 1.5 0.5)` → `2`, and `(+ 10 0.5)` → `10.5`.

### 5.2 Logic

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `not` | one value | Boolean | `false` and `()` give `true`. Every other value gives `false`, including `0`. |
| `and` | two Integers | Integer | Bitwise AND. `(and 5 3)` → `1` |
| `or` | two Integers | Integer | Bitwise OR. `(or 4 2)` → `6` |
| `xor` | two Integers | Integer | Bitwise XOR. `(xor 7 3)` → `4` |
| `nand` | two Integers | Integer | `(nand 1 1)` → `-2` |
| `nor` | two Integers | Integer | `(nor 0 0)` → `-1` |
| `xnor` | two Integers | Integer | `(xnor 7 3)` → `-5` |

`and`, `or`, `xor`, `nand`, `nor` and `xnor` work on Integers only. `(and true false)` is an error. Use `if` or `not` for Boolean logic. `examples/logical_adder.luna` uses these functions on the values 0 and 1.

### 5.3 Conversion and Types

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `int` | a String, Integer or Float | Integer | A String is parsed with OCaml `int_of_string`. A String that cannot be parsed gives `0`, without an error. A Float is truncated toward zero. |
| `float` | a String, Integer or Float | Float | A String is parsed with OCaml `float_of_string`. A String that cannot be parsed gives `0.0`, without an error. |
| `str` | an Integer or Float | String | Other types are an error. A Float is converted with OCaml `string_of_float`. |
| `symbol->string` | a Symbol | String | Other types are an error. |
| `string->symbol` | a String | Symbol | Other types are an error. |
| `type` | any value | String | See the list below. |

`type` returns one of: `"Integer"`, `"Float"`, `"String"`, `"Boolean"`, `"List"`, `"Nil"` (the empty list), `"Symbol"`, `"Lambda"`, `"Macro"`, `"Primitive"`, `"Dict"`.

```lisp
(type (int "123")) ; "Integer"
```

### 5.4 Lists

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `list` | any number of values | List | `(list 1 2 3)`. `(list)` is the empty list. |
| `cons` | a value, a List | List | `(cons 0 (list 1 2))` → `(0 1 2)` |
| `fst` | a non-empty List | value | First element. Error on the empty list. |
| `rst` | a non-empty List | List | Every element but the first. Error on the empty list. |
| `get` | an index, a List or String | value | Index starts at 0. `(get 1 (list 8 20 3))` → `20`. On a String, returns a one-character String. An index out of range is an error. |
| `set` | an index, a value, a List or String | List or String | Returns a new value. The original is not changed. `(set 1 13 (list 8 20 3))` → `(8 13 3)`. On a String, the value must be a String and only its first character is used. An index out of range does not change the value (see section 8). |
| `null` | a List or Dict | Boolean | `true` when empty. Other types are an error. |
| `length` | a List, String or Dict | Integer | For a String, the number of bytes. |
| `range` | two Integers | List | Inclusive at both ends. `(range 1 4)` → `(1 2 3 4)`. `(range 5 4)` → `()`. |
| `concat` | Lists, or Strings | List or String | All arguments must be Lists, or all must be Strings. `(concat (list 1) (list 2 3))` → `(1 2 3)`. `(concat)` → `()`. |
| `reverse` | a List | List | |
| `map` | a function, a List | List | The function takes one argument: a Lambda with one parameter, or a built-in. |
| `filter` | a function, a List | List | The function must return a Boolean for each element. |
| `reduce` | a function, an initial value, a List | value | Calls the function as `(f accumulator element)`. A Lambda must take two parameters. `(reduce + 0 (list 1 2 3))` → `6` |

`examples/reduce.luna`, `examples/filter.luna` and `examples/get_set.luna` use these functions.

### 5.5 Strings

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `explode` | a String | List of Strings | One one-character String per character. `(explode "hi")` → `(h i)` |
| `implode` | a List of Strings | String | Joins the elements. Other element types are an error. `(implode (explode "hi"))` → `hi` |
| `split` | see section 3.6 | | Special form. |

Strings also work with `concat`, `get`, `set`, `length`, `=`, `!=` and the comparison operators (section 5.1).

### 5.6 Dictionaries

A Dict is a mutable table with String keys. Changes made by `dict-set`, `dict-remove` are visible to every reference to the same Dict.

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `dict` | an Integer | Dict | The Integer is the initial size of the table, not a limit. `(dict)` without an argument is an error. |
| `dict-set` | a Dict, a String key, a value | value | Stores the value and returns it. |
| `dict-get` | a Dict, a String key | value | Error if the key is missing. |
| `dict-ref` | a Dict, a String key, a default | value | Returns the default if the key is missing. |
| `dict-contains` | a Dict, a String key | Boolean | |
| `dict-remove` | a Dict, a String key | `()` | Removes the key. Returns the empty list, not the Dict. |
| `dict-keys` | a Dict | List of Strings | The order is not specified. |
| `dict-values` | a Dict | List | The order is not specified. |

`examples/dict.luna`:

```lisp
(defvar address-book (dict 10))
(dict-set address-book "Alice" "alice@wonderland.com")
(writeln (dict-ref address-book "Azucena" "azucena@ortizcoff.ee"))
```

### 5.7 Input, Output and System

| Function | Arguments | Returns | Notes |
| -------- | --------- | ------- | ----- |
| `write` | a value, optional colour keyword | the value | Writes without a newline. |
| `writeln` | a value, optional colour keyword | the value | Writes with a newline. |
| `input` | a String prompt | String | Prints the prompt, reads one line without its newline. At end of input, returns `""`. |
| `eval` | a String | value | Parses and evaluates the String in the current environment. Returns the value of the last expression, or `()` for an empty String. |
| `read-file` | a path String | String | Error if the file cannot be read. |
| `write-file` | a path String, a content String, a mode keyword | `()` | Mode `:overwrite` or `:append`. Creates the file if it does not exist. |
| `cmd` | a String | String | Runs the String as a shell command and returns its standard output. The exit status is not returned. Standard error is not captured. |
| `exit` | an Integer | does not return | Ends the program with that exit code. |

Colour keywords for `write` and `writeln`: `:red`, `:green`, `:yellow`, `:blue`. Any other keyword prints without colour.

```lisp
(writeln "Done" :green)
(write "splitted on spaces: " :blue)
```

#### How values are printed

| Value | Printed as |
| ----- | ---------- |
| Integer | Decimal digits, e.g. `42` |
| Float | Up to 10 significant digits. A Float that looks whole gets `.0`, e.g. `42.0`. |
| Boolean | `true` or `false` |
| String | Its text. In a List or a Dict, Strings are printed without quotes. |
| List | `(e1 e2 ...)` |
| Empty list | `Nil` |
| Dict | `{"key": value, ...}` |
| Symbol | Its name |
| Lambda, Macro, Primitive | `Lambda`, `Macro`, `Primitive` |

---

## 6. Examples

All examples below are excerpts from files in the repository. The file name is given with each one.

### Conditions with `case`

`examples/cases.luna`:

```lisp
(function check_sign (x) (case
	((> x 0) "Positive")
	((< x 0) "Negative")
	(default "Zero")
))

(writeln (check_sign -10)) ; Negative
```

### Pattern matching on lists

`examples/match.luna`:

```lisp
(function count (seq)
	(match seq
		("" 0)
		(() 0)
		((hd :: tl) (+ 1 (count tl)))
	)
)
(writeln (concat "String length using recursion: " (str (count "Soluna"))))
```

### Nested patterns

`examples/match.luna`:

```lisp
(function nested (val)
	(match val
		(("user" (id "admin")) (concat "Admin ID: " (str id)))
		(("user" (id _)) (concat "User ID: " (str id)))
		(_ "Invalid structure")
	)
)
(writeln (nested (list "user" (list 2 "admin")))) ; Admin ID: 2
```

### Dictionary

`examples/dict.luna`:

```lisp
(defvar address-book (dict 10))
(dict-set address-book "Neo" "neo@matrix.com")
(writeln (dict-get address-book "Neo"))
```

### Macro with `&rest`

`examples/rest.luna`:

```lisp
(defmacro sum-all (&rest num)
  `(+ ,@num))

(writeln (sum-all 1 2 3 4))
```

### Reading a file

`examples/files.luna` reads `my_heroes_raw.txt`, parses it character by character, and writes the result with `write-file`:

```lisp
(defvar data (read-file "my_heroes_raw.txt"))
(write-file "my_heroes.txt" "" :overwrite)
```

---

## 7. Grammar (Simplified)

```ebnf
Program  ::= Expr*
Expr     ::= Atom | List | ReaderPrefix Expr
ReaderPrefix ::= "'" | "`" | "," | ",@"
List     ::= "(" Expr* ")"
Atom     ::= Integer | Float | Boolean | String | Keyword | Symbol | "nil"
Comment  ::= ";" (any characters up to end of line)
```

A Float token contains a `.`. A Keyword token starts with `:`. The grammar does not restrict which symbols may appear in head position. Section 2 describes how each form is evaluated.

---

## 8. Notes and Known Limits

These points follow from the source in `bin/main.ml`.

* **`defvar` does not enforce immutability.** A second `defvar` for the same name in the same scope replaces the previous binding. No error is raised. Scopes form a chain: a function call, `let` and `let*` create a scope whose parent is the defining scope (function) or the current scope (`let`, `let*`). Lookups walk the chain, so a function sees the current state of the scope it was defined in; `set!` changes the nearest scope where the name is bound.
* **Truthiness differs between forms.** `if` treats `false` and `()` as false. `case` and `while` treat only `false` as false, so `()` is true in those forms. A `while` whose condition is `()` does not stop.
* **Numbers in `match` are compared by type.** `1` and `1.0` do not match each other in a pattern. Numbers in `=` are compared with a tolerance of `1e-11`. **To confirm.**
* **Equality of lists** is structural and ignores source positions: two lists with the same contents compare equal wherever they were written. Numbers inside a list are compared by exact value, with Integers and Floats distinct. Dicts and function values are compared by identity, not by their contents.
* **Escape sequences** in a String are processed when the file is read and again when the String is written. A String written as `"\\n"` may therefore print as a line break. **To confirm.**
* **Quote preprocessing.** Before tokenising, the reader inserts spaces around `(`, `)`, `'`, `` ` ``, `,` and `,@` outside strings. It treats a `"` as the end of a string even when the `"` follows a backslash. A `\"` inside a String may therefore change how later characters are read. **To confirm.**
* **Silent fallbacks.** `int` and `float` return `0` and `0.0` for a String that cannot be parsed. `set` with an index out of range returns the value unchanged.
* **`-` with one argument** returns the argument unchanged.
* **`cmd` discards the exit status and standard error.**
* **`each` leaves its variable bound** after the loop.
* **`dict-keys` and `dict-values` have no defined order.** A `writeln` of a Dict prints its entries in the same unspecified order.
* **Errors** are written to standard error as `[Soluna ERROR] file:line -> message`, and the program exits with status 1. The message includes terminal colour codes. `try` catches these errors. It does not catch internal errors.
* **Lists are immutable.** `set` and `cons` return new lists. The lists they were given are not changed.

---

## 9. Running Soluna

| Command | Action |
| ------- | ------ |
| `soluna` | Starts the Oracle REPL. |
| `soluna file.luna [args...]` | Runs a file. |
| `soluna eval "code" [args...]` | Runs a String of code. |
| `soluna bundle file.luna` | Builds a standalone executable with `ocamlopt`. |
| `soluna new name` | Creates a project folder with `main.luna` and `project.sol`. |
| `soluna add lib` | Downloads a library into `libs/` of the current project. |
| `soluna build` | Bundles the project named in `project.sol`. |
| `soluna version` | Prints the version. |
| `soluna help` | Prints the help. |
