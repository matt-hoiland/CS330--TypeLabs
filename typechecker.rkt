#lang plai

; Authors: Matthew Hoiland, Isaac Hartung
; CS 330 -- Winter 2016

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
 
; parse : s-expression -> Expr
(define (parse sexp)
  (error 'parse "not implemented"))
 
; type-of : Expr -> Type
(define (type-of e)
  (error 'type-of "not implemented"))