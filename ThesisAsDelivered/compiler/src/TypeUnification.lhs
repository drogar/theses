\begin{code}
module TypeUnification where
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Qtypes


data TypeEquation = TypeEquation Qtype Qtype
                  deriving (Eq, Show, Ord)

tupleizeTE :: TypeEquation -> (Qtype,Qtype)
tupleizeTE (TypeEquation qt1 qt2) = (qt1, qt2)

unifyTE :: Map Identifier Qtype -> TypeEquation ->
           Map Identifier Qtype
unifyTE mp (TypeEquation qt1 qt2) = unify mp qt1 qt2

unifyTESet :: Map Identifier Qtype -> Set TypeEquation ->
              Map Identifier Qtype
unifyTESet mp setTE
    = unifyList mp $ Set.toList $ Set.map tupleizeTE setTE


unify :: Map Identifier Qtype -> Qtype -> Qtype -> 
         Map Identifier Qtype

unify _ BIT QBIT = error "Unification : bit == qbit"
unify _ QBIT INT = error "Unification : Qbit == int"
unify _ BIT INT  = error "Unification : bit == int"

unify _ BIT (DECLTYPE id ts) = error $ "Unification : BIT = DATA "++id
unify _ QBIT (DECLTYPE id ts) = error $ "Unification : QBIT = DATA "++id
unify _ INT (DECLTYPE id ts) = error $ "Unification : INT = DATA "++id

unify un BIT (TYPEVAR id) 
    =  case (Map.lookup id un) of
         (Just BIT) -> un
         (Just _) -> error $ "Unification: Typevar "++id++" already bound"
         Nothing -> Map.insert id BIT un
unify un QBIT (TYPEVAR id) 
    =  case (Map.lookup id un) of
         (Just QBIT) -> un
         (Just _) -> error $ "Unification: Typevar "++id++" already bound"
         Nothing -> Map.insert id QBIT un
unify un INT (TYPEVAR id) 
    =  case (Map.lookup id un) of
         (Just INT) -> un
         (Just _) -> error $ "Unification: Typevar "++id++" already bound"
         Nothing -> Map.insert id INT un

unify un (TYPEVAR a) (TYPEVAR b) = un --Is this right

unify un (TYPEVAR a) nt@(DECLTYPE tid ts) 
   = case (Map.lookup a un) of
         (Just t@(DECLTYPE t1 t2)) 
              -> if t1 == tid && t2 == ts then un
                 else error $ "Unification: Typevar "++ a ++
                             " already bound to " ++ (show t) ++ 
                             ", cannot rebind to "++(show nt)
         (Just t) 
              -> error $ "Unification: Typevar "++ a ++
                         " already bound to " ++ (show t) ++ 
                         ", cannot rebind to " ++(show nt)
         Nothing -> Map.insert a nt un
 

unify un (DECLTYPE tid ts) (DECLTYPE pid ps)
   | tid == pid = unifyList un $ zip ts ps
   | otherwise = error $ "Unification: Unable to unify variant data types " 
                         ++ tid++", "++pid
unify _ (Qproc _ _) _  = error "Unification: does not work on function types"
unify _ _ (Qproc _ _)  = error "Unification: does not work on function types"

unify un a b = unify un b a -- catch other side.

unifyList :: Map Identifier Qtype -> [(Qtype,Qtype)] ->
             Map Identifier Qtype
unifyList un [] = un
unifyList un ((a,b):ts)
          = unifyList (unify un a b) ts

appunify :: Map Identifier Qtype -> Qtype -> Qtype
appunify un (TYPEVAR id)
         = case (Map.lookup id un) of
              (Just t) -> t
              Nothing -> TYPEVAR id

appunify un (DECLTYPE tid ts)
         = DECLTYPE tid (List.map (appunify un) ts)

appunify _ t = t


\end{code}
