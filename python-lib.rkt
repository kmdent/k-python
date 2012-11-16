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
                              empty)) (list (values (Local) 'to-print))))

(define assert-true-lambda
  (CFunc (list 'check-true)
         (CIf (CId 'check-true)
              (make-true)
              (CError (make-object (VStr "Assert failed"))))
         (list (values (Local) 'check-true))))

(define assert-equal-lambda
  (CFunc (list 'left 'right)
        (CIf (CApp (CGetField (CId 'left)
                        (get-prim-func 'eq))
              (list (CId 'left) (CId 'right)))
             (make-true)
             (CError (make-object (VStr "Assert failed"))))
        (list (values (Local) 'left) (values (Local) 'right))))

(define len-lambda
  (CFunc (list 'arg-id)
         (CGetField (CId 'arg-id)
                    (make-object (VStr "__len__")))
         (list (values (Local) 'arg-id))))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)        
        (bind 'type type-class)
        (bind 'NoneType none-class)
        (bind 'int int-class)
        (bind 'bool bool-class)
        (bind 'True (make-true))
        (bind 'False (make-false))
        (bind 'None (make-none))
        (bind 'str str-class)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertEqual assert-equal-lambda)))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name (Local) value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))

