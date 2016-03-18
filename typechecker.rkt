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
  (cond
    [(number? sexp) (num sexp)]
    [(or (equal? sexp 'true) (equal? sexp 'false)) (bool sexp)]
    [(symbol? sexp) (if(or (equal? sexp '+)
                           (equal? sexp '/)
                           (equal? sexp '*)
                           (equal? sexp '-)
                           (equal? sexp 'with)
                           (equal? sexp 'fun)
                           (equal? sexp 'bif)
                           (equal? sexp 'nempty?)
                           (equal? sexp 'iszero))
                    (error 'parse (string-append "not an id: " (symbol->string sexp)))
                    (id sexp))]
    [(and (list? sexp) (not (empty? sexp)))
     (cond
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
          [(/) (bin-num-op (look-up '/)
                           (parse (second sexp))
                           (parse (third sexp)))]
          [(with) (if (and (list? (second sexp))
                           (list? (first (second sexp)))
                           (not(empty? (second sexp)))
                           (= (length sexp) 3)
                           (andmap (lambda (x) (= (length x) 2)) (second sexp))
                           (andmap (lambda (x) (symbol? (first x))) (second sexp))
                           ;(not (ormap (lambda (x) (symbol? (second x))) (second sexp)))
                           (andmap (lambda (x) (= (count (lambda (y) (symbol=? (first x)
                                                                               (first y)
                                                                               )) (second sexp)) 1)) (second sexp))
                           ) 
                      (with (map (lambda (x) (binding (first x) (parse (second x)))) (second sexp))
                            (parse (third sexp)))
                      (error 'parse "Illegal syntax")
                      )]
          [(fun) (if (and (list? (second sexp))
                          (andmap (lambda (x) (symbol? x)) (second sexp))
                          (andmap (lambda (x) (not (or (equal? x '+)
                                                       (equal? x '/)
                                                       (equal? x '*)
                                                       (equal? x '-)
                                                       (equal? x 'with)
                                                       (equal? x 'fun)
                                                       (equal? x 'bif))
                                                   )) (second sexp))
                          (andmap (lambda (x) (= (count (lambda (y) (symbol=? x y)) (second sexp)) 1)) (second sexp))
                          (= (length (rest sexp)) 2))
                     (fun (second sexp)
                          (parse (third sexp)))
                     (error 'parse "Illegal syntax"))]
          [(bif) (if (and (= (length (rest sexp)) 3))
                     (bif (parse (second sexp))
                          (parse (third sexp))
                          (parse (fourth sexp)))
                     (error 'parse "Illegal syntax"))]
          [else (if (> (length sexp) 1)
                    (app (parse (first sexp))
                         (map parse (rest sexp)))
                    (error 'parse "Illegal syntax"))]
          
          )]
       [(and (symbol? (first sexp)) (= (length (rest sexp)) 1))
        (if (equal? (first sexp) 'iszero)
            (iszero (parse (rest sexp)))
            (if? (equal? (first sexp) 'isnempty?)
                 (isnempty? )))
       [(number? (first sexp))
        (if (> (length (rest sexp)) 1)
            (ncons (nfirst (parse (first sexp))) (nrest (parse (rest sexp))))
            (ncons (nfirst (parse (first sexp))) (nrest (nempty))))]
       )]
[(and (list? sexp) (empty? sexp)) (nempty)]
[else (error 'parse "Illegal syntax")]))
 
; type-of : Expr -> Type
(define (type-of e)
  (error 'type-of "not implemented"))

(define (type-of e)
  (type-case Expr e
    [num (n) (t-num)]
    [id (v) ...]
    [bool (b) (t-bool)]
    [bin-num-op (op lhs rhs) (if (equal? (type))]
    [iszero (e) (t-bool)]
    ;make sure 'then and 'else have the same return type and then return that type
    [bif (test then else) (if (and (equal? (type-of test) t-bool) (equal? (type-of then) (type-of else)))
                              (type-of then)
                              (error 'type-of "..."))]
    [with (bound-id bound-body body) ...]
    [fun (arg-id arg-type result-type body) ...]
    [app (fun-expr arg-expr) ...]
    [nempty ...]
    [ncons (first rest) ...]
    [nfirst (e) ...]
    [nrest (e) ...]
    [isnempty (e) ...])