# Soluna Language Reference

> **Version:** 0.6.0
> **Implementation reference:** `bin/main.ml`

---

## 1. Lexical Elements

### 1.1 Atoms

* **Numbers**: integers (e.g. `42`, `-7`)
* **Booleans**: `true`, `false`
* **Strings**: `"hello"`, supports escape sequences (`\n`, `\t`, etc.)
* **Symbols**: identifiers for variables, functions, and macros

### 1.2 Lists

Lists are written using parentheses:

```lisp
(expr1 expr2 expr3)
```

The first element determines how the list is evaluated.

---

## 2. Evaluation Model

* Atoms evaluate to themselves (except symbols).
* Symbols evaluate to their bound value.
* Lists are evaluated as:

  * **Special forms** (custom evaluation rules)
  * **Macros** (expanded before evaluation)
  * **Function calls** (arguments evaluated first)

---

## 3. Special Forms

### 3.1 Definitions

#### `defvar`

Defines an immutable variable.

```lisp
(defvar name value)
```

---

#### `function`

Defines a named function.

```lisp
(function name (arg1 arg2 ...) body)
```

---

#### `lambda`

Creates an anonymous function (closure).

```lisp
(lambda (arg1 arg2 ...) body)
```

---

### 3.2 Control Flow

#### `if`

```lisp
(if condition then-expr else-expr)
```

---

#### `while`

```lisp
(while condition body)
```

---

#### `do`

Evaluates expressions sequentially and returns the last value.

```lisp
(do expr1 expr2 expr3)
```

---

### 3.3 Local Bindings

#### `let`

Parallel bindings.

```lisp
(let ((a 1) (b 2)) body)
```

#### `let*`

Sequential bindings.

```lisp
(let* ((a 1) (b (+ a 1))) body)
```

---

### 3.4 Conditionals

#### `case`

```lisp
(case
  (test1) expr1
  (test2) expr2
  (default) default-expr)
```

---

#### `match`

Pattern matching on values.

```lisp
(match value
  ((pattern1) expr1)
  ((pattern2) expr2)
  (default expr))
```

Patterns may bind variables and optionally include guards using `(when condition)`.

---

### 3.5 Exceptions

#### `try`

```lisp
(try expr (errorVar handlerExpr))
```

Catches runtime failures and binds the error message to `errorVar`.

---

### 3.6 Iteration

#### `each`

Iterates over a list.

```lisp
(each item list body)
```

---

### 3.7 Threading

#### `bind`

Threads an accumulated value through expressions.

```lisp
(bind acc init expr1 expr2 ...)
```

---

### 3.8 Quoting

#### `quote`

Prevents evaluation.

```lisp
(quote expr)
```

#### `quasiquote`, `unquote`, `unquote-splicing`

Used for macro construction.

---

### 3.9 Modules

#### `include`

Loads and evaluates another Soluna file.

```lisp
(include "file.sol")
```

---

## 4. Macros

### `defmacro`

Defines a macro that operates on raw syntax.

```lisp
(defmacro name (args...) body)
```

Macros are expanded before evaluation.

---

## 5. Built-in Functions

This section lists all built-in functions available by default, with short usage examples.

---

### 5.1 Arithmetic & Logic

| Function    | Description    | Example               |
| ----------- | -------------- | --------------------- |
| `+`         | Addition       | `(+ 1 2 3)` → `6`     |
| `-`         | Subtraction    | `(- 10 3)` → `7`      |
| `*`         | Multiplication | `(* 2 4)` → `8`       |
| `/`         | Division       | `(/ 8 2)` → `4`       |
| `mod`       | Modulo         | `(mod 10 3)` → `1`    |
| `< > <= >=` | Comparisons    | `(< 1 2)` → `true`    |
| `=`         | Equality       | `(= 3 3)` → `true`    |
| `!=`        | Inequality     | `(!= 3 4)` → `true`   |
| `int`       | String → int   | `(int "42")` → `42`   |
| `str`       | Value → string | `(str 42)` → `"42"`   |
| `type`      | Type name      | `(type 42)` → `"int"` |

---

### 5.2 Lists

| Function  | Description     | Example                           |
| --------- | --------------- | --------------------------------- |
| `list`    | Create list     | `(list 1 2 3)`                    |
| `cons`    | Prepend element | `(cons 0 (list 1 2))` → `(0 1 2)` |
| `fst`     | First element   | `(fst (list 1 2 3))` → `1`        |
| `rst`     | Rest of list    | `(rst (list 1 2 3))` → `(2 3)`    |
| `null`    | Empty check     | `(null (list))` → `true`          |
| `range`   | Number range    | `(range 1 4)` → `(1 2 3)`         |
| `concat`  | Concatenate     | `(concat (list 1) (list 2 3))`    |
| `reverse` | Reverse list    | `(reverse (list 1 2))` → `(2 1)`  |
| `map`     | Map function    | `(map square (list 1 2 3))`       |
| `filter`  | Filter list     | `(filter even? (list 1 2 3 4))`   |
| `reduce`  | Fold list       | `(reduce + 0 (list 1 2 3))` → `6` |

---

### 5.3 Strings

| Function  | Description        | Example                    |
| --------- | ------------------ | -------------------------- |
| `explode` | String → char list | `(explode "hi")` → `(h i)` |
| `implode` | Char list → string | `(implode (list 'h' 'i'))` |
| `split`   | Split string       | `(split "," "a,b,c")`      |

---

### 5.4 Dictionaries

| Function        | Description       | Example                 |
| --------------- | ----------------- | ----------------------- |
| `dict`          | Create dictionary | `(dict)`                |
| `dict-set`      | Set value         | `(dict-set d "a" 1)`    |
| `dict-get`      | Get value         | `(dict-get d "a")`      |
| `dict-ref`      | Get with default  | `(dict-ref d "a" 0)`    |
| `dict-remove`   | Remove key        | `(dict-remove d "a")`   |
| `dict-contains` | Key exists        | `(dict-contains d "a")` |
| `dict-keys`     | All keys          | `(dict-keys d)`         |
| `dict-values`   | All values        | `(dict-values d)`       |

---

### 5.5 I/O

| Function     | Description           | Example                                |
| ------------ | --------------------- | -------------------------------------- |
| `write`      | Print without newline | `(write "Hello")`                      |
| `writeln`    | Print with newline    | `(writeln "Hello")`                    |
| `input`      | Read input            | `(input "> ")`                         |
| `eval`       | Evaluate code string  | `(eval "(+ 1 2)")`                     |
| `read-file`  | Read file             | `(read-file "a.txt")`                  |
| `write-file` | Write file            | `(write-file "a.txt" "hi" :overwrite)` |

---

## 6. Examples

### Function

```lisp
(function square (x) (* x x))
(square 4)
```

### Pattern Matching

```lisp
(match x
  ((1) "one")
  ((2) "two")
  (default "other"))
```

### Macro

```lisp
(defmacro unless (cond body)
  `(if (not ,cond) ,body null))
```

---

## 7. Grammar (Simplified)

```ebnf
Program ::= Expr*
Expr    ::= Atom | List
Atom    ::= Number | String | Boolean | Symbol
List    ::= '(' Expr* ')'
```

---

## 8. Notes

* Variables defined with `defvar` are immutable.
* Functions are first-class values.
* Macros operate on syntax, not values.
* Errors raise failures that can be caught with `try`.

---

**End of Soluna Language Reference**

