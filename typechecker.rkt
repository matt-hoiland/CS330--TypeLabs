#lang plai

; Authors: Matthew Hoiland, Isaac Hartung
; CS 330 -- Winter 2016

(print-only-errors #t)

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
    [(or (equal? sexp 'true) (equal? sexp 'false)) (bool sexp)]
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
          [else (if (= (length sexp) 2)
                    (app (parse (first sexp))
                         (parse (second sexp)))
                    (error 'parse "Illegal syntax"))]
          
          )]
       [(and (symbol? (first sexp)) (= (length (rest sexp)) 1))
        (if (equal? (first sexp) 'iszero)
            (iszero (parse (second sexp)))
            (if (equal? (first sexp) 'isnempty)
                 (isnempty (parse (second sexp)))
                 (error 'parse "Illegal syntax")))]
       [(number? (first sexp))
        (if (> (length sexp) 1)
            (ncons (nfirst (parse (first sexp))) (nrest (parse (rest sexp))))
            (ncons (nfirst (parse (first sexp))) (nrest (nempty))))]
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
(test (parse (list 1 2 3 4)) (ncons
                              (nfirst (num 1))
                              (nrest (ncons (nfirst (num 2)) (nrest (ncons (nfirst (num 3)) (nrest (ncons (nfirst (num 4)) (nrest (nempty))))))))))

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
    [bin-num-op (op lhs rhs) (error 'type-of "not implemented")]
    [iszero (e) (t-bool)]
    ;make sure 'then and 'else have the same return type and then return that type
    [bif (test then else) (if (and (equal? (type-of test) t-bool) (equal? (type-of then) (type-of else)))
                              (type-of then)
                              (error 'type-of "not implemented"))]
    [with (bound-id bound-body body) (error 'type-of "not implemented")]
    [fun (arg-id arg-type result-type body) (error 'type-of "not implemented")]
    [app (fun-expr arg-expr) (error 'type-of "not implemented")]
    [nempty () (error 'type-of "not implemented")]
    [ncons (first rest) (list-check first rest)]
    [nfirst (e) (type-of e)]
    [nrest (e) (type-of e)]
    [isnempty (e) (t-bool)]))


