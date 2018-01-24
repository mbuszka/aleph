# aleph
An experimental language with algebraic effects

Necessary information:
  - Syntax
  - Intuitive semantics
  - Overview of code structure
  - How to run, compile, test
  

### Grammar

    toplevel ::= let <identifier> = <value>
               | run <expression>
               | eff <type-lit> = <operations>

    operations ::= { <identifier> : type-lit -> type-lit ; }+
    
    type-lit ::= Unit | Int

    value ::= fn <identifier> -> <expression>
            | <literal>
            | <variable>
    
    literal ::= int | ()

    variable ::= <identifier>

    expression ::= <expression> <expressiom>
                 | <variable>
                 | <value>
                 | handle <type-lit> in <expression> with {<handlers>}*
                 | lift <type-lit> in ( expression )
    
    handlers ::= { <identifier> <identifier> , <identifier> -> expression ;}*
               | { return <identifier> -> expression ; }