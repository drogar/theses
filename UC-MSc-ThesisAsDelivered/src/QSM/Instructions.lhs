%include polycode.fmt
\subsection{Description of the machine instructions}\label{subsec:theinstructionsofthemachine}
The instruction set has been designed to strike a balance between
a reasonable instruction (i.e., machine like) 
and a useful instruction, (i.e., a program
does not require hundreds of instructions to do a unit of work).

%if false
\begin{code}
module QSM.Instructions(
		    Label,
		    Instruction(..),
		    Code,
                    EntryPoint,
                    CodePointer,
                    Memory)
		    where
import Data.Basis
import QSM.Transformations
import Data.ClassComp
import QSM.QuantumStack
import Data.Map as Map
import QSM.ClassicalStack
\end{code}

\subsubsection{Labels}\label{subsec:instructionlabels}
\begin{singlespace}
\begin{code}
type Label = Int
\end{code}
\end{singlespace}
%endif
\subsubsection{Instruction Definitions}\label{subsec:instructiondefintions}

The |Instruction| data type is a sum type of the 
different classical and quantum instructions available in the machine.
\begin{code}
data Instruction a = 
\end{code}
\paragraph{Node Creation}\label{para:nodecreation}
There are three instructions which allow us to create data on the 
stack and one which binds sub-nodes into a datatype.
\begin{code}
      QLoad StackAddress a | QCons StackAddress Constructor |
      QMove StackAddress |  QBind StackAddress |
\end{code}
\paragraph{Quantum stack Node Deletion}\label{para:qstacknodedeletion}
Conversely, three instructions remove data from the 
quantum stack.
\begin{code}
      QUnbind StackAddress | QDiscard | QDelete |
\end{code}
\paragraph{Quantum stack manipulation}\label{para:quantumstackmanipulation}
One of the similarities between a quantum stack and classical stack
is the top nodes are the primary targets of instructions. Because
of this, instructions to move items up the stack are required.
\begin{code}
      QPullup StackAddress | QName StackAddress StackAddress |
\end{code}
\paragraph{Unitary transformation and control}\label{para:unitarytransform}
Specific unitary transformations are 
applied to the top of the stack. These
are affected by the control of the transfroms.
\begin{code}
      AddCtrl | QCtrl | UnCtrl | QApply Int UnitaryOp |
\end{code}
Arbitrary transformations are definable and a compiler may create
them. These
transformation are  unitary matrices, which are applied to the 
\qbit{}s at the top of the stack. 

\paragraph{Measurement, Deconstruction and Choice}\label{para:measurmentdeconstruction}
The |Measure| instruction measures a \qbit, the |Split| does a
case deconstruction of a declared data type. The |EndQC| instruction
steps through the list of cases set up by a |Split, Measure| or |Use|.
\begin{code}
    EndQC |  Split [(Constructor , Label)] | 
    Measure Label Label |
\end{code}
\subsubsection{Using Classical Values}\label{subsubsec:usingclassicalvalues}
The |Use| instruction will execute the code at |Label| for 
each possible value a classical element can have.
\begin{code}
      Use Label |
\end{code}
\paragraph{Classical Control}\label{para:classicalcontrol}
Instructions for standard flow control changes are next.
\begin{code}
      Jump Label | CondJump Label |
      Call Int EntryPoint | Return Int |  NoOp |
\end{code}
\subsubsection{Classical Operations}\label{subsubsec:classicalOperations}
Instructions for standard classical stack operations complete our set of 
instructions.
\begin{code}
      CGet Int | CPut Int | CApply ClassicalOp |
      CPop | CLoad (Either Int Bool)
\end{code}
%if false
\begin{code}
   deriving (Eq, Show, Read)
\end{code}
%endif

\subsubsection{CodeMemory definition}
The |Code| type is used to hold the currently active list
of instructions. |Memory| holds all of a programs instructions
as a map from the functions entry point to its list of instructions.

{
\begin{singlespace}
\begin{code}
type Code a = [Instruction a ]
type EntryPoint = String
type CodePointer = (EntryPoint, Label) 
type Memory a  = Map EntryPoint (Code a)
\end{code}
\end{singlespace}
}

