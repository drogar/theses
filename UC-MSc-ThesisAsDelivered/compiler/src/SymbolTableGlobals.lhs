\begin{code}
module SymbolTableGlobals (
                          addGlobalTransforms
                          ) where
import SemTypes
import Qtypes
import Data.Map as Map
import Data.List as List

seTransforms = [ SeFun "Not" "" (Just NotGate) (([],[]),([],[]))
                           [] [QBIT] [QBIT] [],
                 SeFun "T" "" (Just Tgate) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "Phase" "" (Just Phase) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "Had" "" (Just Hadamard) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "RhoX" "" (Just RhoX) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "RhoY" "" (Just RhoY) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "RhoZ" "" (Just RhoZ) (([],[]),([],[]))
                            [] [QBIT] [QBIT] [],
                 SeFun "Swap" "" (Just Swap) (([],[]),([],[]))
                            []  [DECLTYPE "List" [QBIT]] 
                           [DECLTYPE "List" [QBIT]] [],
--[QBIT,QBIT] [QBIT,QBIT] [],
                 SeFun "Toffoli3" "" (Just Toffoli) (([],[]),([],[]))
                            [] 
                           [QBIT,QBIT,QBIT] [QBIT,QBIT,QBIT] [],
                 SeFun "Rot" "" (Just Rotate) (([],[]),([],[]))
                             [INT] [QBIT] 
                           [QBIT] [],
                 SeFun "UM" "" (Just UM) (([],[]),([],[]))
                             [INT,INT,INT] [DECLTYPE "List" [QBIT]] 
                           [DECLTYPE "List" [QBIT]] [],

                 SeFun "CNot" "" (Just (Controlled NotGate)) (([],[]),([],[]))
                       []
                           [QBIT,QBIT] [QBIT,QBIT] [] ]
 {- ,                 SeFun "CT" "" (Just (Controlled Tgate)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CPhase" "" (Just (Controlled Phase)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CHad" "" (Just (Controlled Hadamard)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CRhoX" "" (Just (Controlled RhoX)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CRhoY" "" (Just (Controlled RhoY)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CRhoZ" "" (Just (Controlled RhoZ)) [] 
                           [QBIT,QBIT] [QBIT,QBIT],
                 SeFun "CSwap" "" (Just (Controlled Swap)) [] 
                           [QBIT,QBIT,QBIT] [QBIT,QBIT,QBIT],
                 SeFun "CToffoli3" "" (Just (Controlled Toffoli)) [] 
                           [QBIT,QBIT,QBIT,QBIT] [QBIT,QBIT,QBIT,QBIT],
                 SeFun "CRot" "" (Just (Controlled Rotate)) [] 
                           [INT,QBIT,QBIT,QBIT] [QBIT,QBIT,QBIT]]

   Need way to generate the Controlled and Inverse version on the fly from
   the base ones.
-}

globalST = Map.fromList $ List.map (\ s -> (gname s, s)) seTransforms

addGlobalTransforms :: SemStateMonad()
addGlobalTransforms = setSymTabGlobal globalST

\end{code}
