-- This module contains a short Curry program to infer types of
-- simple lambda expressions together with some unit tests.

import Test.Prop
import Control.Search.SearchTree.Unsafe

-- Expressions are basically lambda expressions:
data Exp = Var Int
         | Apply Exp Exp
         | Lambda Int Exp

-- Type expressions: type variable and functional types
data TExp = TVar Int | TFunc TExp TExp

-- A type environment associates to each variable index a type expression:
type TEnv = [(Int,TExp)]

-- The typing rules defines an operation that, given a type evironment,
-- maps an expression to a type expression:
typeOf :: TEnv -> Exp -> TExp
typeOf tenv (Var i) = maybe unknown id (lookup i tenv)
typeOf tenv (Apply e1 e2) =
  let (TFunc t1 t2) = typeOf tenv e1
      t = typeOf tenv e2
  in t =:= t1 &> t2
typeOf tenv (Lambda x e) =
  let xt free
      rt = typeOf ((x,xt):tenv) e
  in TFunc xt rt

-- In order to use the inferred types, we have to transform
-- unbound (type expression) variables into type variables:
instTVars :: TExp -> TExp
instTVars texp =
  if isVar texp
  then TVar (varId texp)
  else case texp of
        TVar _         -> texp
        TFunc t1 t2    -> TFunc (instTVars t1) (instTVars t2)

-- Since the indices of the type variables are quite big, we
-- number them in a type expression:
numberTVars :: TExp -> TExp
numberTVars texp = snd (nTV [] texp)
 where
  nTV tvs (TVar i) = maybe (let j = length tvs in ((i,j):tvs, TVar j))
                           (\j -> (tvs, TVar j))
                           (lookup i tvs)
  nTV tvs (TFunc t1 t2) = let (tvs1,t1') = nTV tvs t1
                              (tvs2,t2') = nTV tvs1 t2
                           in (tvs2, TFunc t1' t2')

-- We put all elements together.
-- Note the use of `Control.SearchTree.Unsafe.someValue`
-- in order to avoid an arbitrary instantiation of unbound variables
-- during the type inference process:
inferType :: Exp -> TExp
inferType = numberTVars . instTVars . someValue . typeOf []

-- Maybe we want to show type expression in a human-readable format:
showTExp :: TExp -> String
showTExp (TVar i) = [chr (97 + i)]
showTExp (TFunc t1 t2) = "(" ++ showTExp t1 ++ " -> " ++ showTExp t2 ++ ")"

-- Shows the inferred type of an expression.
showType :: Exp -> String
showType = showTExp . inferType

------------------------------------------------------------------------------
-- Example expressions:

-- identity function: \x -> x
idExp :: Exp
idExp = Lambda 1 (Var 1)

-- twice function: \f x -> f (f x)
twiceExp :: Exp
twiceExp = Lambda 1 (Lambda 2 (Apply (Var 1) (Apply (Var 1) (Var 2))))

m1 :: TExp
m1 = instTVars $ someValue (typeOf [] idExp)

m2 :: TExp
m2 = instTVars $ someValue (typeOf [] twiceExp)

-- Some tests for the type inference:
testTypeOfId :: Prop
testTypeOfId = (showType idExp) -=- "(a -> a)"

testTypeOfTwice :: Prop
testTypeOfTwice = (showType twiceExp) -=- "((a -> a) -> (a -> a))"

----------------------------------------------------------------
