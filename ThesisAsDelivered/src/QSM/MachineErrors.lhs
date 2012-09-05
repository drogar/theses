%include polycode.fmt
\subsection{Error Definitions}\label{subsec:errordefinitions}

\begin{singlespace}
\begin{code}
module QSM.MachineErrors where
import QSM.QuantumStack
import Data.Matrix

badDiscard =  "MachineException: Can not discard when multiple branches exist"
backwardsJump = "MachineException: Jumping back - must iterate"
wrongCall = "MachineException: Call must be processed by Stream functions"
useDataCheck =  "MachineException: Use must be done on a StackInt node"
splitDataCheck = "MachineException: Can not use other than StackCons in a split"
measureDataCheck = "MachineException: Measure must be done on a StackQBit node"
qcontrolBadEnd =  "MachineException: End of QControl with invalid Dump Element."
bindDataCheck = "MachineException : Bind of non Cons stack"
bindMultiCons = "MachineException : Bind with multiple cons"
unbindDataCheck = "MachineException : UnBind of non Cons stack"
unbindNothingBound = "MachineException: UnBinding with no bound variables"
unbindBadCons = "MachineException : UnBind with multiple cons"
matByStackError :: String -> String -> String
matByStackError typ bt
  = "MachineException: Cannot transform from a "++typ ++ ". Data is "++bt
stackByMatError :: String -> String -> String
stackByMatError  typ bt
  = "MachineException: Cannot transform to a "++typ ++ ". Data is "++bt
stackToMatError = "MachineCheck: toMatrix: Operations on negative number of qbits illegal."
setValsError = "MachineCheck: setValsFromMat: Operations on negative number of qbits illegal."
setValsDataError = "MachineCheck: setValsFromMat: You may not set the values of non-Qbit elements in the stack"

setValsTypeError :: (Quantum a b) => Int -> 
                    Matrix (QuantumStack a b) -> 
                    QuantumStack a b -> String
setValsTypeError n m q =  
    "MachineCheck: SetValsFromMat error on n="++(show n)++
         ";   m="++(showMat m)++" ;;;;   q="++(show q)

clsstackError = "MachineCheck: CStack operation: Insufficient Stack elements for classical operation"
clsstackTypeError = "MachineCheck: CStack operation: Invalid types for classical operation"
\end{code}
\end{singlespace}
