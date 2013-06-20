\begin{code}
module TestStack where
import StackOperations
import Data.ClassComp
import QuantumStack
import Data.LazyNum
import Instructions


startstack :: QuantumStack LazyNum
startstack=StackInt (StackInt (StackData (Snum 0.15)) (StackData (Snum 0.25)) )
	    (StackInt (StackData (Snum 0.3)) (StackData (Snum 0.3)) )



\end{code}
