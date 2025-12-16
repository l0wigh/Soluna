# Soluna Language Documentation (v0.3.1)

---

## Available Keywords (Special Forms)

Special forms control how their arguments are evaluated and enable control flow constructs.

| Keyword | Description | Syntax |
|------|------------|--------|
| `defvar` | Defines and initializes an immutable variable | `(defvar name value)` |
| `function` | Defines a named reusable function | `(function name (param1 ...) body)` |
| `lambda` | Creates an anonymous function (closure) | `(lambda (param1 ...) body)` |
| `if` | Evaluates a condition and executes one of two branches | `(if condition then-expr else-expr)` |
| `case` | Multi-branch conditional expression | `(case (test1 result1) ... (default default-result))` |
| `do` | Executes expressions sequentially and returns the last result | `(do expr1 expr2 ... last-expr)` |
| `each` | Iterates over a list, binding each value | `(each var-name list-expr body)` |
| `while` | Executes body while condition is true | `(while condition-expr body)` |
| `include` | Loads and evaluates another Soluna file | `(include "filepath")` |
| `true` | Boolean constant | `true` |
| `false` | Boolean constant | `false` |
| `default` | Final fallback clause for `case` | `default` |
| `eval` | Evaluate an S-expression | `(eval "(+ 2 3)")` |
| `:overwrite` | File write mode | `:overwrite` |
| `:append` | File write mode | `:append` |

---

## Built-in Primitives (Functions)

Primitives always evaluate their arguments before execution.

---

## I. Core Types & Arithmetic

| Primitive | Description | Syntax | Types |
|---------|------------|--------|------|
| `+ - * /` | Standard arithmetic operations | `(+ n1 n2 ...)` | Number |
| `mod` | Remainder of division | `(mod n1 n2)` | Number |
| `< >` | Comparison operators | `(< a b)` | Number, String |
| `=` | Equality comparison | `(= a b)` | Number, String |
| `<= >=` | Inclusive comparison | `(<= a b)` | Number |
| `num` | Checks if value is a number | `(num value)` | Any |

---

## II. Lists & Sequences

| Primitive | Description | Syntax | Types |
|---------|------------|--------|------|
| `list` | Creates a list | `(list e1 e2 ...)` | Any |
| `cons` | Prepends an element to a list | `(cons element list)` | Any, List |
| `fst` | Returns the first element | `(fst list)` | List |
| `rst` | Returns the rest of the list | `(rst list)` | List |
| `null` | Checks for empty list or dict | `(null sequence)` | List, Dict |
| `reverse` | Reverses a list | `(reverse list)` | List |
| `map` | Applies a function to each element | `(map function list)` | Function, List |
| `filter` | Filters elements using a predicate | `(filter predicate list)` | Function, List |
| `length` | Returns sequence length | `(length sequence)` | List, String, Dict |
| `explode` | Converts a string to character list | `(explode "string")` | String |
| `implode` | Concatenates string list into one string | `(implode list-of-strings)` | List |

---

## III. Dictionaries (Hash Tables)

| Primitive | Description | Syntax | Types |
|---------|------------|--------|------|
| `dict` | Creates an empty dictionary | `(dict size)` | Number |
| `dict-set` | Associates a key with a value | `(dict-set dict key value)` | Dict |
| `dict-get` | Retrieves value for a key | `(dict-get dict key)` | Dict |
| `dict-ref` | Retrieves value or default | `(dict-ref dict key default)` | Dict |
| `dict-keys` | Returns all keys | `(dict-keys dict)` | Dict |
| `dict-values` | Returns all values | `(dict-values dict)` | Dict |
| `dict-remove` | Removes a key | `(dict-remove dict key)` | Dict |
| `dict-contains` | Checks if key exists | `(dict-contains dict key)` | Dict |

---

## IV. Input / Output & File Handling

| Primitive | Description | Syntax | Types |
|---------|------------|--------|------|
| `write` | Prints without newline (takes an optional color `:green/:red/:yellow/:blue`) | `(write expression)` | Any |
| `writeln` | Prints with newline (takes an optional color `:green/:red/:yellow/:blue`) | `(writeln expression)` | Any |
| `input` | Prompts user input | `(input "prompt")` | String |
| `read-file` | Reads entire file | `(read-file "filepath")` | String |
| `write-file` | Writes to a file | `(write-file "path" "content" mode)` | String |

---

