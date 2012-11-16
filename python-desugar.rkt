#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-objects.rkt")

(define (cascade-lets (ids : (listof symbol))
                      (sts : (listof ScopeType))
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) (first sts) (first exprs) (cascade-lets (rest ids) (rest sts) (rest exprs) body))]))


(define (str->cval (str : string)) : CExp
  (make-object (VStr str)))

(define (call-method (obj : CExp) (name : CExp) (args : (listof CExp)))
  (CLet 'self (Local) obj
        (CApp (CGetField (CId 'self)
                         name)
              (append (list (CId 'self))
                      args))))

(define (desugar (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (make-object (VNum n))]
    
    [PyStr (s) (make-object (VStr s))]
    
    [PyRaise (exn) (CError (desugar exn))]
    
    [PyPass () (CPass)]
    
    [PyLambda (args body) (CFunc args (desugar body) (list))]
    
    [PyModule (exprs)
              (let ([global-vars (get-vars exprs)]) ;gets all of the assignments in the global scope
                (begin (if (hasGlobalScopeErrors global-vars) ;checks the existence of 'nonlocal' or 'global' declarations in the global scope
                           (error 'PyModule "Global or Nonlocal declaration in the global scope.")
                           (void))
                       (cascade-lets (get-ids global-vars) ;puts the variables in the environment as Globals
                                     (make-item-list (Global) (length global-vars) (list))
                                     (make-item-list (CUnbound) (length global-vars) (list))
                                     (desugar (PySeq (append
                                                      (list (PyGlobalEnv)) ;the first thing interpreter does is creating the
                                                      ;separate global environment
                                                      (list exprs)))))))] ;executes the program
    
    [PyDef (name args body)
           (begin (CSeq
                   (CSet! (CId name) (CFunc (list) (CError (CObject (VStr "dummy function was called!") (make-hash (list)))) (list)))
                   (CLet 'some-func (Local) (CFunc args (desugar body) (get-vars body)) ;;need (get-vars body) for scoping
                         (CSet! (CId name) (CId 'some-func)))))]    
    
    ;;[PyAssign (targets value)
    ;;      (CLet 'assign-value (Local) (desugar value)
    ;;            (desugar (PySeq (map (lambda (e) (PySet! e (PyId 'assign-value))) targets))))]
    
    ; XXX look into order of ops for assign
    [PyAssign (targets value) (CLet 'value-id (Local) (desugar value)
                                    (local [(define assign-exprs (map (lambda (target) (CSet! (desugar target) (CId 'value-id))) targets))]
                                      (foldl (lambda (e1 e2) (CSeq e2 e1)) (first assign-exprs) (rest assign-exprs))))]
    
    [PyIf (test body orelse) (CIf (desugar test)
                                  (desugar body)
                                  (desugar orelse))]
    [PyList (mutable elts) (CList mutable (map desugar elts))]
    [PyPrimOp (op arg) (CLet 'arg-val (Local) (desugar arg)
                             (CApp (CGetField (CId 'arg-val) 
                                              (get-prim-func op))
                                   (list (CId 'arg-val))))]
    
    [PyBinOp (op left right) (call-method (desugar left) (get-prim-func op) (list (desugar right)))]
    
    [PyBoolOp (op values) (cond
                            [(equal? op 'and) (and-op (map desugar values))]
                            [(equal? op 'or) (or-op (map desugar values))])]
    
    [PyComp (left ops comps) (CLet 'left-val (Local) (desugar left)
                                   (compare (CId 'left-val) ops (map desugar comps)))] ;;clueless for this one
    
    [PyGlobalEnv () (CGlobalEnv)]
    
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [else (error 'desugar (string-append "Haven't desugared yet: "
                                         (to-string expr)))]))

;;This is where we map out the comparison operations to properly short-circuit
;;Assumes the left value is an already-bound CId.
(define (compare (left : CExp) (ops : (listof symbol)) (rights : (listof CExp))) : CExp
  (cond
    [(equal? 1 (length rights)) (CApp (CGetField left
                                                 (get-prim-func (first ops)))
                                      (list left
                                            (first rights)))]
    [else (CLet 'right-val (Local) (first rights)
                (CIf (CApp (CGetField left
                                      (get-prim-func (first ops)))
                           (list left
                                 (CId 'right-val)))
                     (compare (CId 'right-val) (rest ops) (rest rights))
                     (CId 'False)))]))

(define (and-op (vals : (listof CExp))) : CExp
  (cond
    [(equal? 1 (length vals)) (first vals)]
    [(equal? 2 (length vals)) (CLet 'left (Local) (first vals)
                                    (CApp (CGetField (CId 'left)
                                                     (get-prim-func 'and))
                                          (list (CId 'left)
                                                (second vals))))]
    [else (CLet 'front (Local) (CLet 'left (Local) (first vals)
                                     (CApp (CGetField (CId 'left)
                                                      (get-prim-func 'and))
                                           (list (CId 'left)
                                                 (second vals))))
                (CIf (CId 'front)
                     (and-op (rest (rest vals)))
                     (CId 'False)))]))

(define (or-op (vals : (listof CExp))) : CExp
  (cond
    [(equal? 0 (length vals)) (error 'or-op "wtf mate?")]
    [(equal? 1 (length vals)) (first vals)]
    [(equal? 2 (length vals)) (CLet 'left (Local) (first vals)
                                    (CApp (CGetField (CId 'left)
                                                     (get-prim-func 'or))
                                          (list (CId 'left)
                                                (second vals))))]
    [else (CLet 'front (Local) (CLet 'left (Local) (first vals)
                                     (CApp (CGetField (CId 'left)
                                                      (get-prim-func 'and))
                                           (list (CId 'left)
                                                 (second vals))))
                (CIf (CId 'front)
                     (CId 'True)
                     (or-op (rest (rest vals)))))]))


(define (get-prim-func op)
  (make-object (VStr
                (cond
                  [(equal? op 'add) "__add__"]
                  [(equal? op 'mul) "__mul__"]
                  [(equal? op 'sub) "__sub__"]
                  [(equal? op 'div) "__div__"]
                  [(equal? op 'floordiv) "__floordiv__"]
                  
                  [(equal? op 'or) "__or__"]
                  [(equal? op 'and) "__and__"]
                  [(equal? op 'not) "__not__"]
                  
                  [(equal? op 'is) "__is__"]
                  
                  [(equal? op 'lt) "__lt__"]
                  [(equal? op 'lte) "__le__"]
                  [(equal? op 'gt) "__gt__"]
                  [(equal? op 'gte) "__ge__"]
                  [(equal? op 'neq) "__ne__"]
                  [(equal? op 'eq) "__eq__"]
                  [else (error 'get-prim-func 
                               (string-append "Unrecognized primitive operation: " 
                                              (symbol->string op)))]
                  
                  ))))

(define (get-vars [expr : PyExpr]) : (listof (ScopeType * symbol))
  (type-case PyExpr expr
    [PyNonlocal (ids) (map (lambda (id) (values (NonLocal) id)) ids)]
    [PyGlobal (ids) (map (lambda (id) (values (Global) id)) ids)]
    [PyGlobalEnv () (list)]
    [PySeq (es) (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (get-vars e)) es))]
    [PyNum (n) (list)]
    [PyApp (f args) (append (get-vars f)
                            (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) args)))]
    [PyReturn (value) (list)]
    [PyId (id) (list)]
    [PyStr (s) (list)]
    [PyBinOp (op left right)
             (append
              (get-vars left)
              (get-vars right))]
    [PyIf (test then orelse)
          (append
           (get-vars test)
           (append (get-vars then) (get-vars orelse))
           ;; (foldl (lambda (a b) (append b a))
           ;;        (list)
           ;;        (map (lambda (e) (get-vars e)) (list then)))
           ;; (foldl (lambda (a b) (append b a))
           ;;        (list)
           ;;        (map (lambda (e) (get-vars e)) (list orelse)))))]
           )]
    
    [PyBoolOp (op exprs)
              (foldl (lambda (a b) (append b a))
                     (list)
                     (map (lambda (e) (get-vars e)) exprs))]
    [PyComp (left ops comparators)
            (append
             (get-vars left)
             (foldl (lambda (a b) (append b a))
                    (list)
                    (map (lambda (e) (get-vars e)) comparators)))]
    [PyPass () (list)]
    ;;[PyNone () (list)]
    [PyVoid () (list)]
    [PyLambda (args body) (list)]
    [PyDef (name args body)
           (list (values (Local) name))]
    
    [PyRaise (exc) (get-vars exc)]
    ;;[Py-NotExist () (list)]
    [PyPrimOp (op arg) (get-vars arg)]
    ;;[PySet (lhs value) ;;PySet case may need to change, because it never actually appears since it only exists from use in PyAssign
    ;;       (append
    ;;           (get-vars value)
    ;;          (type-case PyExpr lhs
    ;;             [PyId (id) (list (values (Local) id))]
    ;;             [else (error 'get-vars-PySet "PySet should not be getting non-ids yet")]))]
    [PyAssign (targets value)
              (append
               (get-vars value)
               (foldl (lambda (a b) (append b a))
                      (list)
                      (map (lambda (e) (type-case PyExpr e
                                         [PyId (id) (list (values (Local) id))]
                                         [else (error 'get-vars-PyAssign "PyAssign should not be getting non-ids yet")]))
                           targets)))]
    [PyModule (exprs)
              (get-vars exprs)]
    
    [else (error 'get-vars "Case not implemented")]
    ))

;; hasGlobalScopeErrors checks the declarations in the global environment.
;; If we have something that is not a 'Local', we return true ('we have an error').
(define (hasGlobalScopeErrors [vars : (listof (ScopeType * symbol))]) : boolean
  (not (foldl (lambda (list-el result) (and list-el result))
              true
              (map (lambda (e) (local ([define-values (st id) e])
                                 (Local? st)))
                   vars))))

(define (get-ids [vars-list : (listof (ScopeType * symbol))]) : (listof symbol)
  (foldl (lambda (a b) (append b a))
         (list)
         (map (lambda (e) (local ([define-values (st id) e])
                            (list id)))
              vars-list)))

(define (make-item-list [item : 'a]
                        [size : number]
                        [newList : (listof 'a)]) : (listof 'a)
  (cond
    [(>= (length newList) size) newList]
    [else (make-item-list item size (append (list item) newList))]))




