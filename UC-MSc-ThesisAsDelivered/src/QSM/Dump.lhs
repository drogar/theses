%include polycode.fmt
\subsection{Description of the dump}
The  quantum stack machine has a |Dump|, used by various
instructions to hold intermediate states of the machine.
%if false
\begin{code}
module QSM.Dump(DumpElement(..),
                showDumpTop,
            Dump)
    where
import Data.Basis
import QSM.ClassicalStack
import QSM.Instructions
import QSM.Transformations
import Data.ClassComp
import QSM.QuantumStack
import Data.Map as Map
import Data.Array as Array
\end{code}
%endif

Another essential part of the machine is the |Dump| which is used
when working with |Use|, |Split|, |Measure| and |Call|.
%%%%Thesis dumpDefinition
{\begin{figure}[htbp]
\begin{singlespace}
\begin{code}

data (Quantum a b) => DumpElement a b =
	  DumpQControl {
                     returnLabel :: Label,
		     branchesToDo:: [(QuantumStack a b, Label)], 
                     resultQStack :: QuantumStack a b,
                     saveClsStack :: ClassicalStack}|
          DumpCall { returnLabel :: Label,
                     returnEp :: EntryPoint,
                     saveClsStack ::ClassicalStack} 
	  deriving  Show

type Dump a b = [DumpElement a b]
\end{code}
\end{singlespace}
\caption{Haskell definition of the Dump}\label{fig:haskellDefinitionOfDump} 
\end{figure}}
 The |DumpElement| for the first three types, |DumpQControl|, provides
all that is needed to complete those instructions on the various branches
of a |Split, Use| or |Measure|. It has the following items:
\begin{itemize}
\item{} |returnLabel|: The |Label| to continue with when the 
instruction is complete. This is normally the instruction 
following the quantum control.
\item{} |saveClsStack|: As the |ClassicalStack| is reset before each
invocation of a branch and then restored after all are completed,
it is saved here.
\item{} |resultQStack|: As the partial branches are processed, 
the  resulting quantum stacks are accumulated, merging after each branch.
\item{} |branchesToDo|: This is a list of pairs, whose first item is
 derived from the quantum stack. Each one is a single branched 
|Qstack|.  The second item in the pair is the label for the code
to execute at this point. Note that in the the 
case of |Use| for integers, it would have been possible to simply use
 a single label as all
branches execute the same code. However, doing it this way allows for a
single data element for Quantum Control.
\end{itemize}

%if false
\begin{code}

showDumpEl :: (Quantum a b)=>DumpElement a b -> String
showDumpEl d@(DumpQControl _ _ _ _)
  =  "Quantum Control: { \n"
        ++ " returnTo :" ++ show (returnLabel d) ++ "\n"
        ++ " branches remaining :" ++ show (length $ branchesToDo d) ++ "\n"
        ++ " saved Classical :" ++ show (saveClsStack d) ++ "\n"
        ++ " current result :" ++ show (resultQStack d) ++ "\n"
        ++ "}"
 
showDumpEl (d@(DumpCall _ _ _))
   = "Call: { \n"
        ++ " returnTo :" ++ (returnEp d) ++ ";" 
               ++ show (returnLabel d) ++ "\n"
        ++ " saved Classical :" ++ show (saveClsStack d) ++ "\n"
        ++ "}"   

showDumpTop :: (Quantum a b)=>Dump a b -> String
showDumpTop = showDumpTop' 5

showDumpTop' :: (Quantum a b)=>Int->Dump a b -> String
showDumpTop' _ [] = "Empty Dump"
showDumpTop' 0 (d:ds)
   = showDumpEl d
        ++ " + "++ (show $ length ds)++" Remaining dump elements."
showDumpTop' n (d:ds)
   = showDumpEl d ++"\n"++ (showDumpTop' (n-1) ds)
\end{code}
%endif

