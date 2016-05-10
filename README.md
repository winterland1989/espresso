Espresso
========

A minimal language compile to ecmascript(Not yet).

It's a toy by now, a lot of design choices need to be made, the parser is capable to recognize basic layout syntax, a pretty printer can be used for debugging purpose.

Quick start
-----------

```es
--------------------------------------------------------------------------------
-- line comment, so that you can use 80 char seperator like so
--------------------------------------------------------------------------------

-- define varible in current scope
let bar = 2

-- function
let foo = \a, b, c -> \curried -> \ -> bar

-- whitespace application
foo (3, 4, 5)

-- always use whitespace application
foo ()

-- assignment
foo = {}
foo.a = 3
```
