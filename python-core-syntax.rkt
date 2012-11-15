#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type PrimVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VNone]
  [VTrue]
  [VFalse]
  [VList (mutable : boolean) (data : (listof CVal))]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)]
  [VPass]
  [VUnbound]
)

(define-type CExp
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (x : symbol)]
  [CLet (x : symbol) (scopeType : ScopeType) (bind : CExp) (body : CExp)]
  [CList (mutable : boolean) (elts : (listof CExp))]
  ;; Should we have a CSet! case? Or just desugar into something
  ;; that adds a value to a global hashmap?
  [CSet! (x : CExp) (bind : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)] ;;need to add vlist for scoping
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)]
  [CPass]
  [CObject (type : PrimVal) (val : PrimVal) (fields : (hashof string CExp))]
  [CSetField (obj : CExp) (field : CExp) (value : CExp)]
  [CGetField (obj : CExp) (field : CExp)]

  ;; experimental
  [CUnbound]
  [CGlobalEnv]
  ;;[C-NotExist (a : number)]
  )

(define-type CVal
  [VObject (val : PrimVal) (fields : (hashof string CVal))])

(define-type-alias Location number)
(define-type ScopeType
  [Local]
  [NonLocal]
  [Global])

(define-type-alias SLTuple (ScopeType * number))
(define-type-alias Env (hashof symbol SLTuple))
(define-type-alias Store (hashof Location CVal))

(define-type AnswerC
  [ValueA (value : CVal) (store : Store)])
  ;;[ReturnA (value : CVal) (store : Store)] ;;we need these, I just don't want to type-case eveything yet
  ;;[ExceptionA (value : CVal) (store : Store)])