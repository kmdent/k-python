#lang plai-typed

(require "python-core-syntax.rkt")

(define (make-object-of-class class-id primval)
  (CObject primval (make-hash (list (values "__class__" (CId class-id))))))

(define (make-object (val : PrimVal)) : CExp
  (type-case PrimVal val
    [VNum (n) (make-object-of-class 'int val)]
    [VTrue () (make-object-of-class 'bool val)]
    [VFalse () (make-object-of-class 'bool val)]
    [VStr (s) (make-object-of-class 'str val)]
    [VNone () (make-object-of-class 'NoneType val)]
    [else (error 'to-object "not implemented object yet")]))

(define (make-true) (make-object (VTrue)))
(define (make-false) (make-object (VFalse)))
(define (make-none) (make-object (VNone)))
(define (make-str s) (make-object (VStr s)))
(define (make-num n) (make-object (VNum n)))
(define (make-bool b) (make-object (if b (VTrue) (VFalse))))
(define (make-exception message) (make-object-of-class 'Exception (VStr message)))

(define (make-prim2-func name op)
  (values name
          (CFunc (list 'self 'right)
                 (CPrim2 op
                         (CId 'self)
                         (CId 'right)))))
(define (make-prim1-func name op)
  (values name
          (CFunc (list 'self)
                 (CPrim1 op (CId 'self)))))

(define type-class
  (CObject (VClass "type") 
           (make-hash (list 
                       (values "__class__" (make-none))
                       (make-prim1-func "__str__" 'to-str)
                       (values "__setattr__"
                               (CFunc (list 'self 'id 'val) (CSetField 
                                                             (CId 'self) 
                                                             (CId 'id)
                                                             (CId'val))))
                       (values "__getattr__"
                               (CFunc (list 'self 'id) (CGetField 
                                                        (CId 'self) 
                                                        (CId 'id))))
                       (values "__and__"
                               (CFunc (list 'self 'id)
                                      (CIf (CPrim1 'truthy (CId 'self))
                                           (CPrim1 'truthy (CId 'self))
                                           (make-false))))
                       
                       (values "__or__"
                               (CFunc (list 'self 'id)
                                      (CIf (CPrim1 'truthy (CId 'self))
                                           (make-true)
                                           (CPrim1 'truthy (CId 'id)))))
                       (values "__not__"
                               (CFunc (list 'self)
                                      (CIf (CPrim1 'truthy (CId 'self))
                                           (make-false)
                                           (make-true))))))))

#|
TODO
Exception
dict
tuple
list
float

classes are callable. How to deal with this? primval of class be a pair of name and callable?
or maybe but the function in __call__ in the class object, and look for classes or closures in CApp

|#

(define none-class
  (CObject (VClass "NoneType")
           (make-hash (list (values "__class__" (CId 'type))))))

(define str-class
  (CObject (VClass "str")
           (make-hash (list (values "__class__" (CId 'type))
                            (make-prim1-func "__len__" 'str-len)
                            (make-prim2-func "__add__" 'str-add)
                            (make-prim2-func "__mul__" 'str-mul)
                            (make-prim2-func "__le__" 'str-le)
                            (make-prim2-func "__lt__" 'str-lt)
                            (make-prim2-func "__ge__" 'str-ge)
                            (make-prim2-func "__gt__" 'str-gt)
                            (make-prim2-func "__eq__" 'str-eq)
                            (make-prim2-func "__is__" 'str-eq)
                            (values "__ne__"
                                    (CFunc (list 'self 'right)
                                           (CIf (CPrim2 'str-eq (CId 'self) (CId 'right))
                                                (make-false)
                                                (make-true))))))))

(define int-class
  (CObject (VClass "int") 
           (make-hash (list (values "__class__" (CId 'type))
                            (make-prim2-func "__add__" 'int-add)
                            (make-prim2-func "__sub__" 'int-sub)
                            (make-prim2-func "__mul__" 'int-mul)
                            (make-prim2-func "__div__" 'int-div)
                            (make-prim2-func "__floordiv__" 'int-floordiv)
                            (make-prim2-func "__mod__" 'int-mod)
                            (make-prim2-func "__le__" 'int-le)
                            (make-prim2-func "__lt__" 'int-lt)
                            (make-prim2-func "__ge__" 'int-ge)
                            (make-prim2-func "__gt__" 'int-gt)
                            (make-prim2-func "__eq__" 'int-eq)
                            (make-prim2-func "__is__" 'int-eq)
                            (make-prim2-func "__ne__" 'int-ne)))))

(define bool-class
  (CObject (VClass "bool")
           (make-hash (list (values "__class__" (CId 'int))))))
