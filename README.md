# aleph

An experimental language with algebraic effects

## Build and install
  - To build this project you will need `stack` - haskell package manager
  - Build and install with `stack install` - it should copy executable to `~/.local/bin`
  - Run tests with `stack test`

## Usage
  - To eval source files `files` run `aleph-exe eval <files>`
  - To only check types run `aleph-exe check <files>`

## Syntax

    toplevel ::= let <identifier> = <value>
               | run <expression>
               | eff <type-lit> = <operations>

    operations ::= { <identifier> : type-lit => type-lit ; }+
    
    type-lit ::= Unit | Int | Bool | List // List is a monomorphic list of ints

    value ::= fn <identifier> -> <expression>
            | <literal>
            | <variable>
    
    literal ::= <int> | true | false | ()

    expression ::= <expression> <expressiom>
                 | <value>
                 | handle <type-lit> in <expression> with {<handlers>}*
                 | lift <type-lit> in ( expression )
                 | <variable> <- <expression> , <expression>
                 | if <expression> then <expression> else <expression> end
    
    handlers ::= { <identifier> <identifier> , <identifier> -> expression ;}*
               | { return <identifier> -> expression ; }

## Builtins
### Integer and boolean operations
  - `add : Int -> a Int -> b Int`
  - `sub : Int -> a Int -> b Int`
  - `mul : Int -> a Int -> b Int`
  - `div : Int -> a Int -> b Int`
  - `isZero : Int -> a Bool`
### List operations
  - `cons : Int -> a List -> b List`
  - `nil  : List`
  - `head : List -> a Int`
  - `tail : List -> a List`
  - `null : List -> a Bool`
  - these operations are not safe, but one could define effect `EX` and wrap them

### Effects
  - `IO`
    - `print : Int -> [IO | a] Unit`

## Semantics
  - operation and effect names must be unique
  - operations have functional type with effect to which they belong
  - operation's behaviour is defined by its handler, handlers are deep

## Code structure
### Syntax
  - This module defines syntax, parser and lexer of the language, all important functionality is exported from `src/Syntax.hs`

### Inference
  - This module implements type inference for the program, file `src/Inference.hs` exports function wich deducts typing environment for a program (list of toplevel clauses)
  - file `src/Inference/Infer.hs` contains functions wich generate typing constraints
  - file `src/Inference/Solve.hs` contains functions which solve these constraints by unification

### Evaluation
  - This module implements evaluation of program, file 'src/Evaluation.hs` exports function `eval` which evaluates program
  - Evaluation uses a stack of continuations contained in `Ctx`