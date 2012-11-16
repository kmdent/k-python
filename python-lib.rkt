#lang plai-typed

(require "python-core-syntax.rkt"
         "python-objects.rkt"
         "python-desugar.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))


(define print-lambda
  (CFunc (list 'to-print)
         (CPrim1 'print
                 (call-method (CId 'to-print)
                                     (make-object (VStr "__str__"))
                                     empty))))

(define assert-true-lambda
  (CFunc (list 'check-true)
         (CIf (CId 'check-true)
              (make-true)
              (CError (make-object (VStr "Assert failed"))))))

(define len-lambda
  (CFunc (list 'arg-id)
         (CGetField (CId 'arg-id)
                    (make-object (VStr "__len__")))))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True (make-true))
        (bind 'False (make-false))
        (bind 'None (make-none))
        (bind 'type type-class)
        (bind 'NoneType none-class)
        (bind 'int int-class)
        (bind 'str str-class)
        (bind 'bool bool-class)
        (bind '___assertTrue assert-true-lambda)))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name (Local) value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))

