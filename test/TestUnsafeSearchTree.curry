------------------------------------------------------------------------------
-- Some tests for the library Control.SearchTree.Unsafe
------------------------------------------------------------------------------

import Test.Prop
import Control.Search.SearchTree.Unsafe
import qualified Control.Search.SearchTree as ST

------------------------------------------------------------------------------
-- Testing isVar:

boolVarPair = x =:= (_::Bool,_::Bool) &> x where x free

testIsVar1 = always (not (isVar (someValue boolVarPair)))

testIsVar2 = always (isVar (fst (someValue boolVarPair)))

------------------------------------------------------------------------------
-- Testing varId:

diffVarIds1 =
  let (a,b) = someValue boolVarPair
  in (varId a /= varId b)

testGetVarId1 = always diffVarIds1

boolEqVarPair = let x,y,z free in (x =:= (y::Bool,z) & y=:=z) &> x

diffVarIds2 =
  let (a,b) = someValue boolEqVarPair
  in (varId a == varId b)

testGetVarId2 = always diffVarIds2

------------------------------------------------------------------------------
-- The following tests also demonstrate why the encapsulated search
-- with unbound variables in result values is non-declarative.

testNumOfNongroundValues =
  (length (allValuesDFS (someSearchTree (id _ :: Bool)))) -=- 1

testNumOfGroundValues =
  (length (allValuesDFS (someSearchTree (not (not (_::Bool)))))) -=- 2

-- However, there is no difference w.r.t. the SearchTree library
-- if the argument variable is always instantiated:

testNumberOfSearchTreeValues1 =
   (length (ST.allValuesDFS (ST.someSearchTree (id (aValue::Bool)))))
   -=- 2

testNumberOfSearchTreeValues2 =
   (length (ST.allValuesDFS (ST.someSearchTree (not (not (aValue::Bool))))))
   -=- 2

------------------------------------------------------------------------------
