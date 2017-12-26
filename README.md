# aleph
An experimental language with algebraic effects

### Grammar

    toplevel ::= def <identifier> { : <type> } = <value>
               | <fixity> int
               | run <expression>
    
    type ::= type-lit | <type> -> <row> <type>

    type-lit ::= () | int | string | bool

    row ::= '<' <row-contents> '>'
          | row-var

    row-var ::= <identifier>

    row-label ::= <identifier>

    row-contents ::= <empty>
                   | <row-label>
                   | <row-label> '|' row-contents

    fixity ::= infix{l|r} <identifier> int
             | {pre|post}fix <identifier> int

    value ::= fun <identifier> => <expression>
            | <literal>
            | <variable>
    
    literal ::= "string" | int | true | false | ()

    variable ::= <identifier>

    expression ::= <expression> <expressiom>
                 | <variable>
                 | <value>
                 | handle <expression> with {<handlers>}*
    
    handlers ::= { | <identifier> <identifier> <identifier> => expression }*
               | { | return <identifier> => expression }?