\begin{code}

module QSM.QSPretty where
import Data.ClassComp
import Data.ClassicalData
import Data.Basis
import Data.LazyNum
import Data.Map as Map
import Data.List as List
import QSM.QuantumStack
import Data.Basis
import Data.LazyNum
import Simulator.SimBase

ltxPretty :: QuantumStack OurBasis LazyNum -> String
ltxPretty StackZero = "0"
ltxPretty (StackData d ) = show d
ltxPretty (StackInt nm st) = "INT("++nm++")"
ltxPretty (StackCons nm st) = "CONS("++nm++")"
ltxPretty (StackQbit nm sb)
          = "\\qsqbit{"++nm++"}{"
               ++ (ltxPretty (val (Zero,Zero) sb)) ++ "}{"
               ++ (ltxPretty (val (Zero,One) sb)) ++ "}{"
               ++ (ltxPretty (val (One,Zero) sb)) ++ "}{"
               ++ (ltxPretty (val (One,One) sb)) ++ "}{}"



\end{code}
