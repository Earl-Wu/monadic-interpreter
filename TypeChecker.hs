{-# LANGUAGE ScopedTypeVariables #-}
module TypeChecker where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MB
import qualified Data.Either as E
import Data.Function ((&))
import AST
import TypecheckM
import ClosureM
import System.IO.Unsafe

type ConstraintSet = S.Set Constraint
type GammaMap = M.Map Ident Fbtype

-- Helper fun: findEqualTypes
-- @cset: Set of constraints
-- @t: Fbtype to look up
-- Returns: A set of all types equal to t in cset
findEqualTypes :: ConstraintSet -> Fbtype -> S.Set Fbtype
findEqualTypes cset t =
  cset &
  S.toList &
  MB.mapMaybe (\(Constraint t1 t2) ->
    if t1 == t then Just t2 else Nothing) &
  S.fromList &
  S.union (S.fromList[t])

-- Helper fun: findTypes
-- @cset: Set of constraints
-- Returns: A set of types that are in the first position of the constraints
-- findTypes :: ConstraintSet -> S.Set Fbtype
-- findTypes cset =
--   cset &
--   S.toList &
--   map (\(Constraint t1 t2) -> t1) &
--   S.fromList

-- Step One: Symmetry --

-- NOTE: Use with ClosureM
symClosure :: ClosureM ()
symClosure = do
  Constraint t1 t2 <- pickConstraint ()
  emit $ Constraint t2 t1
  return ()

-- Step Two: Transitivity --

-- NOTE: Use with ClosureM
transClosure :: ClosureM ()
transClosure = do
  Constraint t1 t2 <- pickConstraint ()
  Constraint t3 t4 <- pickConstraint ()
  if (t2 == t3) then emit $ Constraint t1 t4 else return ()

-- NOTE: Use with ClosureM
funClosure :: ClosureM ()
funClosure = do
  Constraint t1 t2 <- pickConstraint ()
  case t1 of
    TArrow t3 t4 ->
      case t2 of
        TArrow t5 t6 ->
          do
            emit $ Constraint t3 t5
            emit $ Constraint t4 t6
        _ -> return ()
    _ -> return ()

-- Helper fun: funTest
-- @t: The input type to check
-- Returns: If t is a TArrow type, return Just t. Otherwise, return nothing.
funTest :: Fbtype -> Maybe Fbtype
funTest t =
  case t of (TArrow t1 t2) -> Just (TArrow t1 t2)
            otherwise -> Nothing

-- Deductive Closure --

closure :: ConstraintSet -> ConstraintSet
closure cs =
  -- closureStep :: ClosureM [()]
  -- sequenc :: (Monad m) => [m a] -> m [a]
  let closureStep = sequence [symClosure, transClosure, funClosure] in
  let ((_::[[()]]), newcs) = runClosureM cs closureStep in
  let cs' = S.union cs newcs in
  if (cs == cs') then cs' else closure cs'

-- Check Contradiction --

contraForOne :: Constraint -> Either String ()
contraForOne cons =
  let Constraint c1 c2 = cons in
  let boolXst = if (c1 == TBool) then True else (c2 == TBool) in
  let intXst = if (c2 == TInt) then True else (c2 == TInt) in
  let funXst =
        if (MB.isJust (funTest c1)) then True else (MB.isJust (funTest c2))
  in
  if ((boolXst && intXst) || (intXst && funXst) || (boolXst && funXst))
  then Left "Type Error"
  else return ()

contraCheck :: ConstraintSet -> Either String ConstraintSet
contraCheck cset =
  cset &
  S.toList &
  map contraForOne &
  E.lefts &
  (\lst -> if (null lst) then return cset else Left "Type Error!")

-- Getting the type --

-- got rid of ConstraintSet as an argument so that it's not threading the constraintSets
-- reason for doing so: Each subproof tree won't affect the others
-- GammaMap is not threaded because we only really need to read from it and not returning it (eventaully)
get_type :: Expr -> GammaMap -> TCM (Fbtype, ConstraintSet)
get_type expr recs =
  case expr of (Var x) -> let temp = M.lookup x recs
                          in case temp of
                            Nothing -> retError "Type Error!"
                            Just y -> return (y, S.empty)
               (Int n) -> return (TInt, S.empty)
               (Bool b) -> return (TBool, S.empty)
               (Plus e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (let temp = S.fromList [Constraint t1 TInt, Constraint t2 TInt]
                         in (TInt, S.unions [temp, eq1, eq2]))
               (Minus e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (let temp = S.fromList [Constraint t1 TInt, Constraint t2 TInt]
                         in (TInt, S.unions [temp, eq1, eq2]))
               (Equal e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (let temp = S.fromList [Constraint t1 TInt, Constraint t2 TInt]
                         in (TBool, S.unions [temp, eq1, eq2]))
               (And e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (let temp = S.fromList [Constraint t1 TBool, Constraint t2 TBool]
                         in (TBool, S.unions [temp, eq1, eq2]))
               (Or e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (let temp = S.fromList [Constraint t1 TBool, Constraint t2 TBool]
                         in (TBool, S.unions [temp, eq1, eq2]))
               (Not e) -> do
                 (t, eq) <- (get_type e recs)
                 return (TBool, S.union eq (S.fromList[Constraint t TBool]))
               (If e1 e2 e3) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 (t3, eq3) <- (get_type e3 recs)
                 return (t2, S.unions [eq1, eq2, eq3, (S.fromList[Constraint t1 TBool])])
               (Let var e1 e2) -> do
                 (t1, eq1) <- (get_type e1 recs)
                 let newRecs = M.insert var t1 recs
                 (t2, eq2) <- (get_type e2 newRecs)
                 return (t2, (S.union eq1 eq2))
               (Function fid e) -> do
                 newT <- nextTVar ()
                 let newRecs = M.insert fid newT recs
                 (t, eq) <- (get_type e newRecs)
                 return (TArrow newT t, eq)
               (Appl e1 e2) -> do
                 newT <- nextTVar ()
                 (t1, eq1) <- (get_type e1 recs)
                 (t2, eq2) <- (get_type e2 recs)
                 return (newT, S.unions [eq1, eq2, (S.fromList[Constraint t1 (TArrow t2 newT)])])

-- substitution --

filter_pred :: Fbtype -> Bool
filter_pred thing =
  case thing of (TVar x) -> False
                otherwise -> True

substitute :: Fbtype -> ConstraintSet -> Either String Fbtype
substitute t cset =
  case t of (TArrow t1 t2) -> do
                type1 <- substitute t1 cset
                type2 <- substitute t2 cset
                return (TArrow type1 type2)
            (TVar x) -> let tpset = findEqualTypes cset t -- finding types equal to t
                        in tpset &
                        S.toList &
                        \lst -> if (elem TInt lst)
                                then return TInt
                                else if (elem TBool lst)
                                     then return TBool
                                     else let cleanL = filter filter_pred lst
                                          in if (null cleanL)
                                             then return (minimum lst)
                                             else let temp_fun = minimum cleanL
                                                  in case temp_fun of (TArrow tp1 tp2) -> if (tp1 == t || tp2 == t)
                                                                                          then Left "Type Error4"
                                                                                          else do
                                                                                             tau1 <- substitute tp1 cset
                                                                                             tau2 <- substitute tp2 cset
                                                                                             return (TArrow tau1 tau2)
            otherwise -> return t

-- typeChecker main function --
typecheck :: Expr -> Either String Fbtype
typecheck expr =
  let gamma = M.empty
  in do
    (t, eqs) <- runTCM (get_type expr gamma)
    let cset = closure eqs
    doa <- contraCheck cset
    res <- substitute t doa
    return res
