module Interpreter where

import AST

-- TODO: Fix the formatting

contains idt lst =
  case lst of [] -> False
              (h:t) -> if (h == idt) then True else (contains idt t)

isClosed expr idLst =
  case expr of (Var x) -> if (contains x idLst) then True else False
               (Int n) -> True
               (Bool b) -> True
               (Plus p1 p2) -> if (isClosed p1 idLst && isClosed p2 idLst) then True else False
               (Minus m1 m2) -> if (isClosed m1 idLst && isClosed m2 idLst) then True else False
               (Equal eq1 eq2) -> if (isClosed eq1 idLst && isClosed eq2 idLst) then True else False
               (And a1 a2) -> if (isClosed a1 idLst && isClosed a2 idLst) then True else False
               (Or or1 or2) -> if (isClosed or1 idLst && isClosed or2 idLst) then True else False
               (Not no) -> if (isClosed no idLst) then True else False
               (If if1 if2 if3) -> if (isClosed if1 idLst && isClosed if2 idLst && isClosed if3 idLst) then True else False
               (Appl app1 app2) -> if (isClosed app1 idLst && isClosed app2 idLst) then True else False
               (Function fid func) ->
                  let idLst2 = if (not (contains fid idLst)) then fid:idLst else idLst in isClosed func idLst2
               (Let lid let1 let2) -> let lidLst2 = if (not (contains lid idLst)) then lid:idLst else idLst in (isClosed let1 lidLst2 && isClosed let2 lidLst2)
               otherwise -> True

subs expr val idt =
  case expr of (Var x) -> if x == idt then val else Var(x)
               (Int n) -> Int n
               (Bool b) -> Bool b
               (Plus p1 p2) -> Plus (subs p1 val idt) (subs p2 val idt)
               (Minus m1 m2) -> Minus (subs m1 val idt) (subs m2 val idt)
               (Equal eq1 eq2) -> Equal (subs eq1 val idt) (subs eq2 val idt)
               (And a1 a2) -> And (subs a1 val idt) (subs a2 val idt)
               (Or or1 or2) -> Or (subs or1 val idt) (subs or2 val idt)
               (Not no) -> Not (subs no val idt)
               (If if1 if2 if3) -> If (subs if1 val idt) (subs if2 val idt) (subs if3 val idt)
               (Appl app1 app2) -> Appl (subs app1 val idt) (subs app2 val idt)
               (Function fid func) -> if fid == idt then (Function fid func) else Function fid (subs func val idt)
               (Let lid let1 let2) -> if lid == idt then Let idt (subs let1 val idt) let2 else Let lid (subs let1 val idt) (subs let2 val idt)
               otherwise -> expr

isBool expr =
  case expr of Bool b -> return b
               _ -> Left "Type Mismatch!"

isInt expr =
 case expr of Int n -> return n
              _ -> Left "Type Mismatch!"

isFun expr =
  case expr of Function x fun -> return (x, fun)
               _ -> Left "Type Mismatch!"

eval expr =
  if (isClosed expr []) then
    case expr of (Var x) -> return (Var x)
                 (Bool b) -> return (Bool b)
                 (Int n) -> return (Int n)
                 (And e1 e2) -> do
                   v1 <- ((eval e1) >>= isBool)
                   v2 <- ((eval e2) >>= isBool)
                   return (Bool (v1 && v2))
                 (Or e1 e2) -> do
                   v1 <- ((eval e1) >>= isBool)
                   v2 <- ((eval e2) >>= isBool)
                   return (Bool (v1 || v2))
                 (Not e) -> do
                   v <- ((eval e) >>= isBool)
                   return (Bool (not v))
                 (Plus e1 e2) -> do
                   v1 <- ((eval e1) >>= isInt)
                   v2 <- ((eval e2) >>= isInt)
                   return (Int (v1 + v2))
                 (Minus e1 e2) -> do
                   v1 <- ((eval e1) >>= isInt)
                   v2 <- ((eval e2) >>= isInt)
                   return (Int (v1 - v2))
                 (Equal e1 e2) -> do
                   v1 <- ((eval e1) >>= isInt)
                   v2 <- ((eval e2) >>= isInt)
                   return (Bool (v1 == v2))
                 (If e1 e2 e3) -> do
                   b1 <- ((eval e1) >>= isBool)
                   if b1 then (eval e2) else (eval e3)
                 (Function x func) -> return (Function x func)
                 (Appl e1 e2) -> do
                   (x, fun) <- ((eval e1) >>= isFun)
                   v <- (eval e2)
                   eval (subs fun v x)
                 (Let x e1 e2) -> do
                   val <- eval e1
                   eval (subs e2 val x)
  else Left ("Expression is not closed!")
