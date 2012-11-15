#lang plai-typed

;; it's objects all the way down. Until you hit turtles

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt")

(require [typed-in racket (hash->list : ((hashof 'a 'b) -> (listof ('a * 'b))))])
(require [typed-in racket (car : (('a * 'b) -> 'a))])
(require [typed-in racket (cdr : (('a * 'b) -> 'b))])

(define (hash-has-key? (ht : (hashof 'a 'b)) (key : 'a)) : boolean
  (type-case (optionof 'b) (hash-ref ht key)
    [some (n) #t]
    [none () #f]))

(define (search-obj (obj : CVal) (field : string)) : CVal
  (type-case CVal obj
    [VObject (pval fields) (type-case (optionof CVal) (hash-ref fields field) 
                             [some (n) n]
                             [none () (type-case (optionof CVal) (hash-ref fields "%class")
                                        [some (n) (search-obj n field)]
                                        [none () (error 'search-obj (string-append "Sorry, dude. Not here: " field))])])]))

(define (extract-string (val : PrimVal)) : string
  (type-case PrimVal val
    [VStr (s) s]
    [else (error 'extract-string "Expected string. Blowing up")]))

(define none-obj (VObject (VNone) (make-hash empty)))

(define (extract-pval (obj : CVal)) : PrimVal
  (type-case CVal obj
    [VObject (primval fields) primval]))

(define (interp-env (expr : CExp) (env : Env) (sto : Store)) : AnswerC
  (type-case CExp expr
    [CError (e) (error 'interp (to-string (interp-env e env sto)))]
    [CUnbound () (ValueA (VObject (VUnbound) (make-hash (list))) sto)]
    
    [CIf (i t e)
         (type-case AnswerC (interp-env i env sto)
           [ValueA (v s)
                   (if (truthy? v)
                       (interp-env t env sto)
                       (interp-env e env sto))])]
    
    [CId (x) (ValueA (lookupStore (lookupVar x env) sto) sto)]
    [CLet (id scopeType bind body)
          (type-case AnswerC (interp-env bind env sto)
            [ValueA (v s)
                    (let ([loc (new-loc)])
                      (interp-env body (extendEnv id (values scopeType loc) env) (overrideStore loc v s)))])]
    [CList (mutable elts) 
           (local ([define sto-box (box sto)]
                   [define val-list (map (lambda (x)
                                           (type-case AnswerC (interp-env x env (unbox sto-box))
                                             [ValueA (v s)
                                                     (begin (set-box! sto-box s)
                                                            v)])) elts)])
             (interp-env (make-object (VList mutable val-list)) env (unbox sto-box)))]
    [CSeq (e1 e2)
          (type-case AnswerC (interp-env e1 env sto)
            [ValueA (v s) (interp-env e2 env s)])]
    
    [CPass () (ValueA none-obj sto)]
    [CApp (fun arges)
          (type-case AnswerC (interp-env fun env sto)
            [ValueA (v s) (type-case CVal v
                            [VObject (primval fields) (type-case PrimVal primval
                                                        [VClosure (cenv argxs body)
                                                                  (interp-args-app cenv argxs body arges env sto (list))]
                                                        [else (error 'interp "Not a closure")])])])]
    [CFunc (args body) (ValueA (VObject (VClosure (newEnvScope env (list) args) args body) (make-hash empty)) sto)] ;;need to add vlist
    [CSet! (a b) (error 'CSet! "Not yet implemented")]
    [CPrim1 (prim arg)
            (type-case AnswerC (interp-env arg env sto)
              [ValueA (v s) (if (equal? prim 'print)
                                (ValueA (python-print v) s)
                                (interp-env (python-prim1 prim v) env s))])]
    [CPrim2 (op left right)
            (type-case AnswerC (interp-env left env sto)
              [ValueA (v s) (type-case AnswerC (interp-env right env s)
                              [ValueA (v2 s2) (interp-env (python-prim2 op v v2) env s2)])])]
    [CObject (pval fields) (interp-obj pval (hash->list fields) env sto (make-hash (list)))] ; TODO
    [CSetField (obj field value) (type-case AnswerC (interp-env obj env sto)
                                   (ValueA (objV objS)
                                           (type-case CVal objV
                                             [VObject (primval fields) 
                                                      (type-case AnswerC (interp-env field env objS)
                                                        (ValueA (fieldV fieldS)
                                                                (type-case AnswerC (interp-env value env fieldS)
                                                                  (ValueA (valV valS)
                                                                          (if (equal? (get-class fieldV) "str")
                                                                              (begin (hash-set! fields (VStr-s (extract-pval fieldV)) valV)
                                                                                     (interp-env (make-none) env valS))
                                                                              (interp-env (make-exception "Non-string field") env valS))))))])))]
    [CGetField (obj field) (type-case AnswerC (interp-env obj env sto)
                             (ValueA (objV objS)
                                     (type-case AnswerC (interp-env field env objS)
                                       (ValueA (fieldV fieldS)
                                               (if (equal? (get-class fieldV) "str")
                                                   (type-case (optionof CVal) (hash-ref (VObject-fields objV) (VStr-s (extract-pval fieldV)))
                                                     [some (v) (ValueA v fieldS)]
                                                     [none () (interp-env (make-exception "No such field") env fieldS)])
                                                   (interp-env (make-exception "Non-string field") env fieldS))))))]

[CGlobalEnv ()
            (begin
              (set! globalEnv (createGlobalEnv env))
              (ValueA none-obj sto))]))


(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (interp expr) : CVal
  (type-case AnswerC (interp-env expr (hash (list)) (hash (list)))
    [ValueA (v s) v]))

(define (interp-obj pval fields env store interped-fields) : AnswerC
  (cond
    [(empty? fields) (ValueA (VObject pval interped-fields) store)]
    [else
     (local ([define f (first fields)])
       (type-case AnswerC (interp-env (cdr f) env store)
         [ValueA (v s) (begin (hash-set! interped-fields (car f) v)
                              (interp-obj pval (rest fields) env s interped-fields))]))]))


(define (interp-args-app closEnv closArgs closBody args env store interped-args) : AnswerC
  (cond
    [(empty? args) (interp-app closBody (allocateLocals closEnv) store closArgs (reverse interped-args))]
    [else
     (type-case AnswerC (interp-env (first args) env store)
       (ValueA (v s)
               (interp-args-app closEnv closArgs closBody (rest args) env s (cons v interped-args))))]))

(define (interp-app body closEnv store argsIds args) : AnswerC
  (cond
    [(not (equal? (length argsIds) (length args)))
     (error 'interp-app "Application failed with arity mismatch")]
    [(empty? args) (interp-env body closEnv store)]
    [else
     (interp-app body closEnv (overrideStore (lookupEnv (first argsIds) closEnv) (first args) store) (rest argsIds) (rest args))]))




;; Scoping shit is below here.  Don't look at it.  Seriously.

;;this is the global variable with the global environment
(define globalEnv
  (hash (list)))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define (extendEnv [id : symbol]
                   [sltuple : SLTuple]
                   [env : Env]) : Env
  (hash-set env id sltuple))

(define (overrideStore [location : Location]
                       [value : CVal]
                       [store : Store]) : Store
  (hash-set store location value))

(define (createGlobalEnv [env : Env]) : Env
  (foldl (lambda (key newEnv)
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'createGlobalScope "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (extendEnv key (values (Global) l) newEnv))]))
         (hash (list))
         (hash-keys env)))

;;lookupEnv searches the environment for some identifier
(define (lookupEnv [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'lookupEnv (string-append "Unbound indentifier error: " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                l)]))

;;lookupStore searches the store for an specific location
(define (lookupStore [loc : Location]
                     [store : Store]) : CVal
  (type-case (optionof CVal) (hash-ref store loc)
    [none () (error 'lookupStore "Unbound location error.")]
    [some (v) (type-case PrimVal (VObject-val v)
                [VUnbound () (error 'lookupStore "Unbound Identifier: using identifier before assignment")]
                [else v])]))


;;look into this: aren't we putting everything from the global env into the current env? Why do we need to look at the global env?
;;lookupVar searches for the identifier first at the given environment, then at the globalEnv. 
(define (lookupVar [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (lookupEnv id globalEnv)]
    [some (v) (local [(define-values (t l) v)]
                l)]))

;;helper method that allocates a new position for all of the local variables in the environment. Used when applying a function, because
;;each time we apply we are using new arguments/locals, not the old ones.
(define (allocateLocals [env : Env]) : Env
  (foldl (lambda (key newEnv)
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'allocateLocals "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (extendEnv key (values t (new-loc)) newEnv)]
                           [else (extendEnv key v newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;newEnvScope returns an environment with the changes needed for a new scope.
;;It basically changes the local tags to nonlocal ones.
(define (newEnvScope [env : Env]
                     [vlist : (listof (ScopeType * symbol))]
                     [args : (listof symbol)]) : Env
  (addLocals (addNonLocals (addGlobalVars (keepOldEnv env)
                                          vlist)
                           vlist)
             (addArgs (filter (lambda (x) (local [(define-values (t id) x)]
                                            (if (Local? t)
                                                true
                                                false)))
                              vlist)
                      args)
             (filter (lambda (x) (local [(define-values (t id) x)]
                                   (if (Local? t)
                                       false
                                       true)))
                     vlist)))

;;addLocals receives a list with Local variables candidates and a list with the variables that are
;;already declared as global or nonlocal in this scope. It returns an environment with the
;;appended correct Local variables
(define (addLocals [env : Env]
                   [localList : (listof (ScopeType * symbol))]
                   [othersList : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? localList) env]
    [else (local [(define-values (t id) (first localList))]
            (if (foldl (lambda (l result) (or l result))
                       false
                       (map (lambda (st-id) (local [(define-values (t2 id2) st-id)]
                                              (if (equal? id id2)
                                                  true
                                                  false)))
                            othersList))
                (addLocals env (rest localList) othersList)
                (addLocals (extendEnv id
                                      (values (Local) (new-loc))
                                      env)
                           (rest localList)
                           othersList)))]))

;;addNonLocals checks for errors that may be raised by the 'nonlocal' expression and,
;;if there are no errors, returns the same environment
(define (addNonLocals [env : Env]
                      [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (NonLocal? t)
                (if (inEnv? id env)
                    (if (Global? (getScopeType id env))
                        (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found")))
                        (addNonLocals env (rest vlist)))
                    (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found"))))
                (addNonLocals env (rest vlist))))]))

;;addGlobalVars will use the vlist (list of variables and ScopeTypes) to insert the variables declared
;;as Global in the environment
(define (addGlobalVars [env : Env]
                       [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (Global? t)
                (if (inEnv? id globalEnv)
                    (addGlobalVars (extendEnv id (values (Global) (lookupEnv id globalEnv)) env)
                                   (rest vlist))
                    (let ([newLocation (new-loc)])
                      (begin
                        (set! globalEnv (extendEnv id
                                                   (values (Global) newLocation)
                                                   env))
                        (addGlobalVars (extendEnv id
                                                  (values (Global) newLocation)
                                                  env)
                                       (rest vlist)))))
                (addGlobalVars env (rest vlist))))]))

;;keepOldEnv is a helper function that will keep all of the non-global variables of the older scope,
;;remembering to change 'Local' variables into 'NonLocal' ones
(define (keepOldEnv [env : Env]) : Env
  (foldl (lambda (key newEnv)
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'keepOldEnv "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (extendEnv key (values (NonLocal) l) newEnv)]
                           [Global () newEnv]
                           [NonLocal () (extendEnv key (values (NonLocal) l) newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;addArgs just appends the args to a list of (ScopeType * symbol),
;;with the ScopeType 'Local'
(define (addArgs [lst : (listof (ScopeType * symbol))]
                 [args : (listof symbol)]) : (listof (ScopeType * symbol))
  (cond
    [(empty? args) lst]
    [else (addArgs (append (list (values (Local) (first args))) lst)
                   (rest args))]))

;;inEnv? searches the environment for some identifier, returning true if
;;the identifier is already there and false otherwise
(define (inEnv? [id : symbol]
                [env : Env]) : boolean
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () false]
    [some (v) true]))

;;getScopeType gets the ScopeType of 'id' in the environment 'env'
(define (getScopeType [id : symbol]
                      [env : Env]) : ScopeType
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'getScopeType (string-append "Unbound Identifier : " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                t)]))
