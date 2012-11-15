#lang plai-typed

(define-type PyExpr
  ;; Scoping issues
  [PyGlobal (ids : (listof symbol))]
  [PyNonlocal (ids : (listof symbol))]
  [PyGlobalEnv]
  
  [PyModule (exp : PyExpr)]

  ;; End scoping addit

  [PySeq (es : (listof PyExpr))]
  
  ;;represents primitive operation with two or more arguments
  [PyBoolOp (op : symbol) (values : (listof PyExpr))]
  
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
  
  ;;represents unary primitives
  [PyPrimOp (op : symbol) (arg : PyExpr)]
  
  ;;represents if statement
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  
  [PyComp (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))]
  
  [PyAssign (lhs : (listof PyExpr)) (value : PyExpr)]
  
  [PyLambda (args : (listof symbol)) (body : PyExpr)]
  [PyDef (name : symbol) (args : (listof symbol)) (body : PyExpr)]
  
  [PyVoid]
  [PyPass]
  
  [PyRaise (exn : PyExpr)]
  [PyReturn (val : PyExpr)]
  
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyList (mutable : boolean) (elts : (listof PyExpr))]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

