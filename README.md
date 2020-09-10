![CI](https://github.com/vzwGrey/yu/workflows/CI/badge.svg?branch=master)

# yu

An imperative toy programming language.

## Usage

**Requirements:** [GHC and Cabal](https://www.haskell.org/ghcup/#).

The project can be compiled using `cabal build`. It can also be ran using `cabal run`.
Currently the program will lex & parse the source code, report potential errors and if no errors
were found it will print the AST.
```
$ cabal run yu -- ./examples/simple.yu
```

## Examples

**Note:** Not fully functional, subject to change.

[`examples/simple.yu`](./examples/simple.yu)
```
module main;

/*
 * Add two integers, double result and return it.
 */
fun addTwice(x: int, y: int): int {
    return (x + y) * 2;
}

/*
 * Entry point of the program.
 */
fun main() {
    let x: int = 5;
    let y = 10;
    let z = addTwice(x, y);
    print(z); // -> 30
}
```

## Grammar

```
module        ::= (decl)* 

decl          ::= module_decl
                | function_decl
module_decl   ::= "module" IDENT ";"
function_decl ::= "fun" IDENT "(" param ("," param)* ")" "{" (statement ";")* "}"
param         ::= IDENT ":" IDENT

statement     ::= var_decl
                | return
var_decl      ::= "let" IDENT (":" IDENT) "=" expr
return        ::= "return" expr

expr          ::= expr_pre (call_tail)* (op_tail)*
expr_pre      ::= literal
                | IDENT
                | "(" expr ")"
call_tail     ::= "(" expr ("," expr)* ")"
literal       ::= INT_LITERAL
op_tail       ::= operator expr

OP_IDENT      ::= _one or more punctuation characters_
INT_LITERAL   ::= _one or more decimal digits_
IDENT         ::= _one or more alphanumeric characters or underscore_
```

## License

Licensed under the [MIT License](./LICENSE).
