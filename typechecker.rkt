#lang plai

; Authors: Matthew Hoiland, Isaac Hartung
; CS 330 -- Winter 2016

#|
type judgements

nempty
  lon -> t-nlist
  '() -> (nempty)
ncons
  lon -> t-nlist
  '(1 2) -> (ncons 1 (ncons 2 (nempty)))
isnempty
  t-nlist -> t-bool
  (isnempty ()) -> true
nfirst
  t-nlist -> t-num
  (nfirst (1 2)) -> 1
nrest
  t-nlist -> t-nlist
  (nrest (1 2 3)) -> (ncons (2) (ncons (3) (nempty))) 
|#


(print-only-errors #t)
(halt-on-errors #t)

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

;op-table
; : (listof (list/c symbol? (number? number? . -> . number?)))
(define op-table
  (list
   (list '+ +)
   (list '- -)
   (list '* *)))
   
  
;(lookup-op op) → (or/c procedure? false/c)
;  op : symbol?
(define (look-up op)
  (if (boolean? (assoc op op-table))
      #f
      (second (assoc op op-table))))

(define (type-lookup t) (if (symbol? t)
                            (cond
                              [(equal? 't-num t) (t-num)]
                              [(equal? 't-bool t) (t-bool)]
                              [(equal? 't-nlist t) (t-nlist)]
                              [else (error 'parse "not a type")])
                            (if (and (list? t)
                                     (= (length t) 3)
                                     (equal? (first t) 't-fun))
                                (t-fun (type-lookup (second t)) (type-lookup (third t)))
                                (error 'parse "not a type"))))
                            

 
; parse : s-expression -> Expr
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(equal? sexp 'true) (bool #t)]
    [(equal? sexp 'false) (bool #f)]
    [(symbol? sexp) (if(or (equal? sexp '+)
                           (equal? sexp '*)
                           (equal? sexp '-)
                           (equal? sexp 'with)
                           (equal? sexp 'fun)
                           (equal? sexp 'bif)
                           (equal? sexp 'nempty)
                           (equal? sexp 'iszero))
                       (error 'parse (string-append "not an id: " (symbol->string sexp)))
                       (id sexp))]
    [(and (list? sexp) (not (empty? sexp)))
     (cond
       [(and (procedure? (look-up (first sexp))) (not (= (length (rest sexp)) 2))) (error 'parse "Illegal syntax")]
       [(and (not (number? (first sexp))) (> (length (rest sexp)) 1))
        (case (first sexp)
          [(+) (bin-num-op (look-up '+)
                           (parse (second sexp))
                           (parse (third sexp)))]
          [(-) (bin-num-op (look-up '-)
                           (parse (second sexp))
                           (parse (third sexp)))]
          [(*) (bin-num-op (look-up '*)
                           (parse (second sexp))
                           (parse (third sexp)))]
          [(with) (if (and (list? (second sexp))
                           (= (length (rest sexp)) 2)
                           ;(list? (first (second sexp)))
                           ;(not(empty? (second sexp)))
                           (= (length (second sexp)) 2)
                           ;(andmap (lambda (x) (= (length x) 2)) (second sexp))
                           ;(andmap (lambda (x) (symbol? (first x))) (second sexp))
                           (symbol? (first (second sexp)))
                           ;(not (ormap (lambda (x) (symbol? (second x))) (second sexp)))
                           ;(andmap (lambda (x) (= (count (lambda (y) (symbol=? (first x)
                           ;(first y)
                           ;)) (second sexp)) 1)) (second sexp))
                           ) 
                      (with (first (second sexp)) (parse (second (second sexp))) (parse (third sexp)))
                      (error 'parse "Illegal syntax")
                      )]
          [(fun) (if (and (list? (second sexp))
                          (= (length (second sexp)) 3)
                          (= (length (rest sexp)) 4))
                     (fun (first (second sexp)) (type-lookup (third (second sexp))) (type-lookup (fourth sexp)) (parse (fifth sexp)))
                     (error 'parse "Illegal syntax"))]
          [(bif) (if (and (= (length (rest sexp)) 3))
                     (bif (parse (second sexp))
                          (parse (third sexp))
                          (parse (fourth sexp)))
                     (error 'parse "Illegal syntax"))]
          [else (error 'parse "Illegal syntax")]        
          )]
       [(and (symbol? (first sexp)) (= (length (rest sexp)) 1))
        (cond
          [(equal? (first sexp) 'iszero) (iszero (parse (second sexp)))]
          [(equal? (first sexp) 'isnempty) (isnempty (parse (second sexp)))]
          [(equal? (first sexp) 'nfirst) (nfirst (parse (second sexp)))]
          [(equal? (first sexp) 'nrest) (nrest (parse (second sexp)))]
          [else 
                 (app (parse (first sexp))
                      (parse (second sexp)))]
          )];[else (error 'parse "Illegal syntax")])]
       [(number? (first sexp))
        (if (> (length sexp) 1)
            (ncons (parse (first sexp)) (parse (rest sexp)))
            (ncons (parse (first sexp)) (nempty)))]
       [(= (length sexp) 2)
                 (app (parse (first sexp))
                      (parse (second sexp)))]
       [else (error 'parse "Illegal syntax")]
       )]
    [(and (list? sexp) (empty? sexp)) (nempty)]
    [else (error 'parse "Illegal syntax")]))

;literals
(test (parse '5) (num 5))
(test/exn (parse "hello") "Illegal syntax")

;binary operators
(test (parse '(+ 1 2)) (bin-num-op + (num 1) (num 2)))
(test (parse '(- 1 2)) (bin-num-op - (num 1) (num 2)))
(test (parse '(* 1 2)) (bin-num-op * (num 1) (num 2)))
(test/exn (parse '(+ 1 2 3)) "Illegal syntax")
(test/exn (parse '(+ 1)) "Illegal syntax")

;id
(test (parse 'x) (id 'x))

;bif
(test (parse '(bif 0 x 1)) (bif (num 0) (id 'x) (num 1)))
(test/exn (parse '(bif 0 x)) "Illegal syntax")
(test/exn (parse '(bif 0 x 1 2)) "Illegal syntax")


;with
(test (parse '(with [x 1] x)) (with 'x (num 1) (id 'x)))

;fun and app not done

;nempty
(test (parse '()) (nempty))

;bif
(test (parse '(bif (iszero a) 3 4)) (bif (iszero (id 'a)) (num 3) (num 4)))
(test/exn (parse '(bif (iszero a) 3)) "Illegal syntax")
(test/exn (parse '(bif (iszero a) 3 5 6)) "Illegal syntax")

;iszero
(test (parse '(iszero 4)) (iszero (num 4)))
(test/exn (parse '(iszero 4 5)) "Illegal syntax")
(test/exn (parse '(iszero)) "Illegal syntax")

;ncons
(test (parse (list 1 2 3 4)) (ncons (num 1) (ncons (num 2) (ncons (num 3) (ncons (num 4) (nempty))))))

;fun
(test (parse '(fun (x : t-num) : (t-fun t-num t-bool) (+ x 3))) (fun 'x (t-num) (t-fun (t-num) (t-bool)) (bin-num-op + (id 'x) (num 3))))

;just an idea, prob doesnt work
(define (list-check first rest) (type-case Expr first
                                      [num (n) (if (equal? rest (nempty))
                                                   #t
                                                   (list-check (ncons-first rest) (ncons-rest rest)))]
                                      [else (error 'type-of "non-number in list")]))

;needs work

(define-type Tenv
  [mtenv]
  [tenv (i symbol?) (t Type?) (r Tenv?)])

; lookup-type : symbol? Tenv? -> Type?
; raises and error if the symbol is not associated with a type
(define (lookup-type id env)
  (type-case Tenv env
    [mtenv () (error 'lookup-type "Type-less symbol")]
    [tenv (i t r)
          (if (symbol=? id i)
              t
              (lookup-type id r))]))

(test/exn (lookup-type 'x (mtenv)) "Type-less")
(test (lookup-type 'x (tenv 'x (t-num) (mtenv))) (t-num))
(test (lookup-type 'x (tenv 'y (t-bool) (tenv 'x (t-num) (mtenv)))) (t-num))
(test/exn (lookup-type 'x (tenv 'y (t-bool) (tenv 'z (t-num) (mtenv)))) "Type-less")

; type-of : Expr -> Type
(define (type-of e env)
  (type-case Expr e
    [num (n) (t-num)]
    [id (v) (lookup-type v env)]
    [bool (b) (t-bool)]
    [bin-num-op (o l r)
                (local [(define tl (type-of l env))
                        (define tr (type-of r env))]
                  (if (and (equal? tl (t-num)) (equal? tr (t-num)))
                      (t-num)
                      (error 'type-of "binop: one of the parameters was not a number")))]
    [iszero (e) (if (t-num? (type-of e env))
                    (t-bool)
                    (error 'type-of "iszero: argument was not of type number"))]
    ;make sure 'then and 'else have the same return type and then return that type
    [bif (test then else)
         (if (and
              (t-bool? (type-of test env))
              (equal? (type-of then env) (type-of else env)))
             (type-of then env)
             (error 'type-of "bif: argument was not of type bool"))]
    [with (bound-id bound-body body)
          (type-of body (tenv bound-id (type-of bound-body env) env))]
    [fun (arg-id arg-type result-type body)
         (if (equal? result-type (type-of body (tenv arg-id arg-type env)))
           (t-fun arg-type result-type)
           (error 'type-of "fun: return type mismatch"))]
    [app (fun-expr arg-expr)
         (local [(define fun-type (type-of fun-expr env))]
           (if (equal? (t-fun-arg fun-type) (type-of arg-expr env))
               (t-fun-result fun-type)
               (error 'type-of "app: wrong arg type")))]
    [nempty () (t-nlist)]
    [ncons (first rest) 
         (if (and (equal? (t-num) (type-of first env)) (equal? (t-nlist) (type-of rest env)))
             (t-nlist)
             (error 'type-of "ncons: not a list"))]
    [nfirst (e)
            (if (t-nlist? (type-of e env))
                (t-num)
                (error 'type-of "nfirst: not a list"))]
    [nrest (e)
           (if (t-nlist? (type-of e env))
               (t-nlist)
               (error 'type-of "isnempty: not a list"))]
    [isnempty (e)
              (if (t-nlist? (type-of e env))
                  (t-bool)
                  (error 'type-of "isnempty: not a list"))]))

(define (run exp) (type-of (parse exp) (mtenv)))


;* Does type-of allow through runtime errors?
(test (run '(nfirst ())) (t-num))


;Expression: num
; * Is there an example of type-of on a correct num expression?
(test (run 5) (t-num))

;Expression: true
; * Is there an example of type-of on a correct true expression?
(test (run 'true) (t-bool))

;Expression: false
; * Is there an example of type-of on a correct false expression?
(test (run 'false) (t-bool))


;Expression: +
; * Is there an example of type-of on a correct + expression?
(test (run '(+ 4 5)) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (run '(+ true 5)) "not a number")
; * Is there a test case for the rhs not being a number?
(test/exn (run '(+ 4 false)) "not a number")

;Expression: -
; * Is there an example of type-of on a correct - expression?
(test (run '(- 4 5)) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (run '(- true 5)) "not a number")
; * Is there a test case for the rhs not being a number?
(test/exn (run '(- 4 false)) "not a number")

;Expression: *
; * Is there an example of type-of on a correct * expression?
(test (run '(* 4 5)) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (run '(* true 5)) "not a number")
; * Is there a test case for the rhs not being a number?
(test/exn (run '(* 4 false)) "not a number")

;Expression: iszero
; * Is there an example of type-of on a correct iszero expression?
(test (run '(iszero 5)) (t-bool))
; * Is there a test case for the input not being a number?
(test/exn (run '(iszero true)) "not of type number") 

;Expression: bif
; * Is there an example of type-of on a correct bif expression?
(test (run '(bif (iszero 2) 3 4)) (t-num))
; * Is there a test case for a non-boolean condition error?
(test/exn (run '(bif (+ 1 1) 3 4)) "not of type bool")
; * Is there a test case for a mismatch error?
(test/exn (run '(bif (iszero 0) 3 false)) "not of type")

;Expression: id
; * Is there an example of type-of on a correct id expression?
(test (run '(with (x 3) (+ x x))) (t-num))
; * Is there a test case for a unbound identifier?
(test/exn (run 'x) "Type-less")
  
;Expression: with
; * Is there an example of type-of on a correct with expression?
(test (run '(with (x 3) (+ 2 x))) (t-num))
; * Is there a test case for misuse of the identifier in the body?
(test/exn (run '(with (x 3) (+ 3 y))) "Type-less")

;Expression: fun
; * Is there an example of type-of on a correct fun expression?
(test (run '(fun (x : t-num) : t-bool (iszero x))) (t-fun (t-num) (t-bool)))
; * Is there a test case for misuse of the formal parameter in the body?
(test/exn (run '(fun (x : t-num) : t-bool (bif x true false))) "argument was not of type") 
; * Is there a test case for a return-type mismatch error?
(test/exn (run '(fun (x : t-num) : t-bool (+ x 3))) "return type mismatch")

;Expression: app
; * Is there an example of type-of on a correct app expression?
(test (run '((fun (x : t-num) : t-bool (iszero x)) 4)) (t-bool)) 
; * Is there a test case for an operator that isn't a function?
(test/exn (run '(/ 4)) "Type-less") 
; * Is there a test case for a wrong argument type?
(test/exn (run '((fun (x : t-num) : t-bool (iszero x)) true)) "wrong arg type")

;Expression: nempty
; * Is there an example of type-of on a correct nempty expression?
(test (run '()) (t-nlist))

;Expression: ncons
; * Is there an example of type-of on a correct ncons expression?
(test (run '(1 2 3 4)) (t-nlist))
; * Is there a test case for the first parameter not being a number?
(test/exn (run '(true 1 2 3)) "Illegal syntax")
; * Is there a test case for the second parameter not being an nlist?
(test/exn (run '(1 true 5 3)) "Illegal syntax")  

;Expression: nempty?
; * Is there an example of type-of on a correct isnempty expression?
(test (run '(isnempty (1 2))) (t-bool))
; * Is there a test case for the input not being an nlist?
(test/exn (run '(isnempty 2)) "not a list")

;Expression: nfirst
; * Is there an example of type-of on a correct nfirst expression?
(test (run '(nfirst (1 2 3 4))) (t-num))
; * Is there a test case for the input not being an nlist?
(test/exn (run '(nfirst 2)) "not a list")
  
;Expression: nrest
; * Is there an example of type-of on a correct nrest expression?
(test (run '(nrest (1 2 3 4))) (t-nlist))
; * Is there a test case for the input not being an nlist?
(test/exn (run '(nrest 2)) "not a list")
