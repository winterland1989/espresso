Espresso
========

A minimal language compile to ecmascript(Not yet).

It's a toy by now, a lot of design choices need to be made, the parser is capable to recognize basic layout syntax, a pretty printer can be used for debugging purpose.

What's next?
-------------

After try ghcjs/purescript/elm, some decisions have been made:

+ **NO** HM style type checker, but i'll keep an eye on [infernu](https://github.com/sinelaw/infernu). 

Generally speaking it's not suitable for javascript's dynamic semantic, and i have no idea how to make javascript's prototype/argument... stuff fit HM type system.

+ better varible scope checking, not auto varible declaretion. 

This should help avoid lots of headache in coffee, but use extra keyword like `let`, maybe we can borrow `:=` from go? all varible declaration should have ES6 `let` semantic. we should prevent varible shadowing in scope checking?

Require a global varible before use, something like `{parseInt} := import window` should be fine.

+ support `class`, support `extend` but don't encourage use it. 

I never understand/use/like `extend` at all, but facebook/react use it and it seems everyone likes it a lot.

I only use `class` as a way to inject dependency, `this` behaves like a reader monad's environment. and `new Ctor (env)` just like `runReader env r`.

+ borrow syntax sugar like `"""`, `///` from coffee 

Can't say live without that, but it's nice to have, no idea how to lexer that 0_0...

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
