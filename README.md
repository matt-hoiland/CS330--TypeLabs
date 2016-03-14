# CS330--TypeLabs

## [1.15 Type Checker](http://hatch.cs.byu.edu/courses/cs330/w16/typec.html)

Complete this assignment with Team Three.

You must complete this assignment using a specific language. Choose Determine language from source in the DrRacket menu, then write
```scheme
#lang plai
```

as the first line of your program, overwritting the default #lang at the top of the file.
In this assignment, you will work with a typed language that includes numbers, booleans, conditionals, functions, and numeric lists. The concrete syntax for the language is given by the following BNF grammars:

```
  expr	 	=	 	num
 	 	|	 	true
 	 	|	 	false
 	 	|	 	(+ expr expr)
 	 	|	 	(- expr expr)
 	 	|	 	(* expr expr)
 	 	|	 	(iszero expr)
 	 	|	 	(bif expr expr expr)
 	 	|	 	id
 	 	|	 	(with (id expr) expr)
 	 	|	 	(fun (id : type) : type expr)
 	 	|	 	(expr expr)
 	 	|	 	nempty
 	 	|	 	(ncons expr expr)
 	 	|	 	(nempty? expr)
 	 	|	 	(nfirst expr)
 	 	|	 	(nrest expr)
 	 	 	 	 
  type	 	=	 	number
 	 	|	 	boolean
 	 	|	 	nlist
 	 	|	 	(type -> type)
```

where `num` is a Racket number and `id` is an identifier not otherwise mentioned in the grammar.
Warning: Many students incorrectly parse Racket booleans (`#t`, `true`, `#f`, `etc`) rather than the symbols `'true` and `'false` for the booleans of this language.

You have not implemented some of these constructs yet, but they should be familiar:
`iszero` consumes a number, and returns `true` if it is `0`, `false` otherwise

the test expression of `bif` ("boolean if") must evaluate to `true` or `false`

`ncons` consumes a number and a numeric list, and produces a numeric list

You must use the following template:
```racket
(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?)
       (arg-type Type?) (result-type Type?)
       (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [nempty]
  [ncons (first Expr?) (rest Expr?)]
  [nfirst (e Expr?)]
  [nrest (e Expr?)]
  [isnempty (e Expr?)])
 
(define-type Type
  [t-num]
  [t-bool]
  [t-nlist]
  [t-fun (arg Type?) (result Type?)])
```

```scheme
; parse : s-expression -> Expr
(define (parse sexp)
  (error 'parse "not implemented"))
```

```scheme
; type-of : Expr -> Type
(define (type-of e)
  (error 'type-of "not implemented"))
```

Define the function parse, which consumes the concrete representation of a program, and returns its abstract syntax tree. To be precise,

### procedure
```scheme
(parse sexp) → Expr?
  sexp : s-expression?
```

You may assume that s-expression provided to parse conforms to the grammar.
You should thoroughly test parse to ensure that it parses all valid syntaxes and in particular make sure that it parses things with type errors.

This parser will not be graded, but it may be convenient for you to write it.

Write down type judgments for the five numeric list constructs: `nempty`, `ncons`, `nempty?`, `nfirst`, and `nrest`. (These count as special test cases.)

Implement the function type-of, which consumes the abstract representation of a program (i.e. the result of parse) To be precise,

### procedure
```scheme
(type-of e) → Type?
  e : Expr?
```

This consumes the abstract representation of a program (i.e. the result of parse).
If the program has no type errors, type-of returns the type of the program.

If the program has a type error, type-of should invoke error with an appropriate error message. For example
```scheme
(type-of (parse '{+ 1 2}))
```

should produce `(t-num)`, while
```scheme
(type-of (parse '{3 4}))
```

should call `(error 'type-of "Number is not a function")`.
You should thoroughly test type-of to ensure that every kind of expression can be typed in as many ways as is reasonable.

Similarly, make sure type-of catches every kind of type error.

In particular, you should make sure type-of does not catch run-time errors, such as (nfirst nempty).

**Do not implement an evaluator, just a type checker.**
