#lang plai-typed

(require "python-core-syntax.rkt")
(require "python-objects.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))
(require (typed-in racket/base [string-length : (string -> number)]))

(define (string-multiply (s : string) (n : number))
  (if (equal? n 0)
      ""
      (string-append s (string-multiply s (- n 1)))))

(define (pretty (arg  : PrimVal)) : string
  (type-case PrimVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "True"]
    [VFalse () "False"]
    [VNone () ""]
    [VList (mutable elts) (foldl string-append
                                 ""
                                 (list (if mutable "[" "(") 
                                       (foldl string-append 
                                              "" 
                                              (map (lambda (x) 
                                                     (string-append (obj-to-string x) 
                                                                    " ")) 
                                                   elts))
                                       (if mutable "]" ")")))]
    [VClosure (env args body) (error 'prim "Can't print closures yet")]
    [VClass (name) (foldl string-append ""
                          (list "<class '" name "'>"))]
    [VPass () (error 'prim "Can't print pass")]
    [VUnbound () (error 'prim "Can't print unbound")]))

(define (obj-to-string (obj : CVal)) : string
  (type-case CVal obj
    [VObject (primval fields) (pretty primval)]))

(define (python-print (arg : CVal))
  (begin (display (string-append (obj-to-string arg) "\n"))
         arg))

; would any but print need to yield a VResult? If not, special case print in interp
(define (python-prim1 (op : symbol) (arg : CVal)) : CExp
  (local [(define primval (VObject-val arg))]
  (case op
    [(truthy) (if (truthy? arg) (make-true) (make-false))]
    [(to-string) (make-str (obj-to-string arg))]
    [(str-len) (make-num (string-length (VStr-s primval)))]
    [else (error 'interp (string-append "NYI prim1 " (symbol->string op)))])))

(define (python-prim2 (op : symbol) (left : CVal) (right : CVal)) : CExp
  (local [(define lpval (VObject-val left))
          (define rpval (VObject-val right))
          (define (type-check class eval)
            (if (not (equal? class (get-class right)))
                (error 'interp "type-check failed") ; TODO raise exception here
                (eval)))] 
  (case op
    [(str-add) (type-check "str" (lambda () (make-str (string-append (VStr-s lpval) (VStr-s rpval)))))]
    [(str-mul) (type-check "int" (lambda () (make-str (string-multiply (VStr-s lpval) (VNum-n rpval)))))]
    [(str-eq) (if (not (equal? "str" (get-class right)))
                  (make-false)
                  (if (equal? (VStr-s lpval) (VStr-s rpval))
                      (make-true)
                      (make-false)))]
    [(int-add) (type-check "int" (lambda () (make-num (+ (VNum-n lpval) (VNum-n rpval)))))]
    [(int-sub) (type-check "int" (lambda () (make-num (- (VNum-n lpval) (VNum-n rpval)))))]
    [(int-mul) (type-check "int" (lambda () (make-num (* (VNum-n lpval) (VNum-n rpval)))))]
    [(int-div) (type-check "int" (lambda () (make-num (/ (VNum-n lpval) (VNum-n rpval)))))] ; TODO divide by zero
    [(int-lt) (type-check "int" (lambda () (make-bool (< (VNum-n lpval) (VNum-n rpval)))))]
    [(int-le) (type-check "int" (lambda () (make-bool (<= (VNum-n lpval) (VNum-n rpval)))))]
    [(int-eq) (type-check "int" (lambda () (make-bool (equal? (VNum-n lpval) (VNum-n rpval)))))]
    [(int-ge) (type-check "int" (lambda () (make-bool (>= (VNum-n lpval) (VNum-n rpval)))))]
    [(int-gt) (type-check "int" (lambda () (make-bool (> (VNum-n lpval) (VNum-n rpval)))))]
    [else (error 'interp (string-append "NYI prim2 " (symbol->string op)))])))

(define (truthy? (v : CVal)) : boolean
  (type-case CVal v
    [VObject (primval fields)
             (type-case PrimVal primval
               [VNum (n) (not (equal? n 0))]
               [VTrue () #t]
               (VFalse () #f)
               [else (error 'interp "Case for truthy NYI")])]))

(define (get-class (v : CVal)) : string
  (local [(define class-pval (VObject-val (some-v (hash-ref (VObject-fields v) "__class__"))))]
    (if (not (VClass? class-pval))
        (error 'interp "__class__ field contains non-class object!")
        (VClass-name class-pval))))
          
       

