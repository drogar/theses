%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"
\subsection{Description of the quantum stack machine and its operation}\label{subsec:QSM:machinedescription}
The  quantum  stack machine consists of a variety of stacks, instructions
registers and other bookkeeping apparati.
%if false
\begin{code}
module QSM.QSM (module Data.Basis,
	        module QSM.Transformations,
	        module Data.ClassComp,
                module Data.Stream,
	        module QSM.Instructions,
                module QSM.Dump,
                module QSM.QuantumStack,
                module QSM.ClassicalStack,
                startMachine,
                noCode,
                pow,
                getCode,
                currIp, 
                qapply,
                qapply',
                hasProc,
                stackToMat, -- only needed for testing
                cTransform,-- only needed for testing
                stackByMat,-- only needed for testing
                matByStack,-- only needed for testing
                liftS,
                liftBMStoCMS,
                makeCMS,
                go,
                runMachine,
                runCMS,
                mainproglabel,
	        MachineState,
                BMS(..),
                CMS(..))
    where
import Data.Basis
import QSM.Transformations
import QSM.ControlStack
import Data.ClassComp
import Data.Stream
import QSM.QuantumStack
import Data.Map as Map
import Data.List as List
import QSM.ClassicalStack
import QSM.Instructions
import QSM.Dump
import Data.Matrix
import QSM.MachineErrors
import Data.Tuples
import Data.Stack

mainproglabel = "main"

\end{code}
%endif
\subsubsection{The machine state}\label{sec:QSM:machinestate}
As discussed earlier in \vref{sec:stackmachineoperation}, there are a 
variety of descriptions for the the machine state. In that section, we
described \bms, \lbms, \cms{} and \ms. 

These are shown in \vref{fig:haskelldefofsimplifiedstate} for \lbms,
\vref{fig:haskellDefinitionOfInterMediateState} for \bms{} and
\vref{fig:haskellDefinitionOfMachineState} for \cms{} and \ms.

Note that in all cases, there are three items to hold what was described
as a simple list of code (\cd) in that earlier section. This is the
|runningCode|, |instructionPointer| and |codeMem| in \bms, with 
similar names in the other states.
{
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
data (Quantum a b ) => BMS a b = 
    BMS { quantumStack :: QuantumStack a b,
         classicalStack :: ClassicalStack,
	 runningCode :: [Instruction a ],
         codeMem :: Memory a ,
         instructionPointer :: (EntryPoint,Label),
	 dump ::(Dump a b),
        namesupply :: Int}
    deriving Show
\end{code}
\end{singlespace}
\caption{Haskell definition of the basic machine state}\label{fig:haskelldefofsimplifiedstate} 
\end{figure}
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
data (Quantum a b ) => LBMS a b = 
    LBMS { quantumStackLbld :: Controlled(QuantumStack a b),
         classicalStackLbld :: ClassicalStack,
	 runningCodeLbld :: [Instruction a ],
         codeMemLbld :: Memory a ,
         instructionPointerLbld :: (EntryPoint,Label),
	 dumpLbld ::(Dump a b),
         namesupplyLbld :: Int}
    deriving Show
\end{code}
\end{singlespace}
\caption{Haskell definition of the labelled machine state}\label{fig:haskellDefinitionOfInterMediateState} 
\end{figure}
\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
data (Quantum a b) => CMS a b =
    CMS {cmsCodeMem :: Memory a,
         controlStack :: [ControlStack a b],
         cmsInstructionPointer :: (EntryPoint, Label),
         cmsRunningCode:: [Instruction a],
         ctrldMS ::[ ((Int,ClassicalStack), 
                      (Controlled(QuantumStack a b), Dump a b))]
        }
    deriving Show

type MachineState a b = Stream (CMS a b)
\end{code}
\end{singlespace}
\caption{Haskell definition of the controlled and complete machine states}\label{fig:haskellDefinitionOfMachineState} 
\end{figure}
}

\subsubsection{Transforming between the state types}
The four stages of machine  descriptions were created because
various instructions are 
more naturally implemented on different stages.  However, the program
must be 
able lift all functions defined on any of the states to |MachineState|,
the infinite list of \cms{} items.

Lift functions are provided to do this such that
\[ (\bms\to\bms)\xrightarrow{lift}(\lbms\to\lbms)
\xrightarrow{lift}(\cms\to\cms).\]

First, to lift endo-functions defined on \bms{} to \lbms, 
define helper functions |decontrolLbld| and |recontrolLbld| which
pull a |QuantumStack| element from the |Controlled| type and
then reapply that control. Lifting the function $f$ is then just
\begin{equation}
lift\ f = recontrolLbld \circ ([id,f])\circ decontrolLbld.\label{eq:liftbmsup}
\end{equation}

{\begin{singlespace}
\begin{code}
 
decontrolLbld :: (Quantum a b) => LBMS a b -> 
             (BMS a b, QuantumStack a b -> Controlled (QuantumStack a b))
decontrolLbld (LBMS cqs cs rc cm ip d n) =
         (BMS qs cs rc cm ip d n, ctlr)
   where (qs,ctlr) = splitcontrolled cqs 

recontrolLbld :: (Quantum a b) => (BMS a b, QuantumStack a b -> 
                      Controlled (QuantumStack a b)) -> LBMS a b 
recontrolLbld (BMS qs cs rc cm ip d n, ctlr) = LBMS (ctlr qs) cs rc cm ip d n
                                            

liftBMStoLBMS :: (Quantum a b)=> (BMS a b -> BMS a b) ->
               LBMS a b -> LBMS a b
liftBMStoLBMS f = recontrolLbld . (app1of2 f) . decontrolLbld 

\end{code}
\end{singlespace}
}

In a similar way, to lift endo-functions 
defined on \bms{} up to to \cms{}, we
define helpers functions |decontrol| and |recontrol|. In this
case, these functions are somewhat more complicated, producing and
consuming a list of \lbms{} items and the \cms{} list of |ControlStack|,
using other subordinate functions described below.
As before,lifting the function $f$ is then just
\begin{equation}
lift\ f = recontrol \circ ([id,map\ f])\circ decontrol.\label{eq:liftuptocms}
\end{equation}

{\begin{singlespace}
\begin{code}
liftLBMStoCMS :: (Quantum a b) => (LBMS a b -> LBMS a b) ->
               CMS a b -> CMS a b
liftLBMStoCMS f = recontrol . (app1of2 (List.map  f)) . decontrol

decontrol :: (Quantum a b) => CMS a b ->([LBMS a b], [ControlStack a b])
decontrol (CMS cm ctls ip rc cmss)
    = ( List.map (buildLBMS cm ip rc) cmss, ctls)

recontrol :: (Quantum a b) => ([LBMS a b], [ControlStack a b])->CMS a b
recontrol (cmss,ctls)
     = CMS cm ctls ip rc cmss' 
         where (cm,ip,rc) = commonLBMS $ head cmss
               cmss' = List.map stacksOfLBMS cmss

\end{code}
\end{singlespace}
}

Three helper functions contribute to |decontrol| and |recontrol|.
All three are simple structure manipulations.

{\begin{singlespace}
\begin{code}
buildLBMS :: (Quantum a b) =>Memory a -> (EntryPoint,Label) ->
            [Instruction a] ->
            ((Int,ClassicalStack), 
             (Controlled(QuantumStack a b), Dump a b)) ->
            LBMS a b
buildLBMS cm ip rc ((n,s), (cqs, d)) = LBMS cqs s rc cm ip d n

commonLBMS :: (Quantum a b) => LBMS a b ->
            (Memory a, (EntryPoint,Label), [Instruction a])
commonLBMS msLbld =
       (codeMemLbld msLbld, 
        instructionPointerLbld msLbld, 
        runningCodeLbld msLbld)

stacksOfLBMS :: (Quantum a b) => LBMS a b ->
              ((Int,ClassicalStack), 
               (Controlled (QuantumStack a b), Dump a b))
stacksOfLBMS msLbld =
      ((namesupplyLbld msLbld,classicalStackLbld msLbld), 
       (quantumStackLbld msLbld, dumpLbld msLbld))

makeCMS ::(Quantum a b) => BMS a b -> CMS a b
makeCMS ms = cms
             where bms' = recontrolLbld (ms, Full)
                   cms = recontrol ([bms'],[])

\end{code}
\end{singlespace}
}

Lifting an endo-function on \bms{} is accomplished by composing the 
two intermediate lifts.

{\begin{singlespace}
\begin{code}
liftBMStoCMS :: (Quantum a b) => (BMS a b -> BMS a b) ->
               CMS a b -> CMS a b
liftBMStoCMS  = liftLBMStoCMS . liftBMStoLBMS

\end{code}
\end{singlespace}
}

Lifting to a |Stream| is accomplished using the |monad|
definition of |Stream|:


{\begin{singlespace}
\begin{code}

liftS :: (a -> a) -> Stream a -> Stream a
liftS f strm
      = do s <- strm
           return $ f s
\end{code}
\end{singlespace}
}
\subsection{Machine transitions}\label{subsec:QSM:machinetransitions}
\subsubsection{Top level functions}\label{subsubsec:QSM:toplevelfunctions}
 The function |go| picks up the 
current instruction \emph{at a particular depth in the 
infinite list of machine states} and continues calling 
|runMachine| until it runs out of instructions.

{\begin{singlespace}
\begin{code}
go :: (Quantum a b) => Int ->MachineState a b -> MachineState a b
go depth mstate
    = let ci = cmscurrIns $ hd $ dropI depth mstate
      in case ci of
           Just _ -> go depth $ runMachine mstate
           Nothing ->  mstate
\end{code}
\end{singlespace}
}

The function |runMachine| lifts the transition
function |runCMS'|, while applying the special logic needed for 
|Call|, |Jump| and |CondJump|.

{
\begin{singlespace}
\begin{code}

runMachine' :: (Quantum a b) => MachineState a b -> MachineState a b
runMachine' mstate@(Stream mshead mstatetl)
    = newstate
      where ci = cmscurrIns mshead
            newstate  = 
             case ci of
                (Just (Call n entpt)) -> 
                    do cms <- mstate
                       return $ liftBMStoCMS (enterFunc n entpt) cms
                _ -> runMachine mstate

runMachine mstate@(Stream mshead mstatetl)
    = newstate
      where ci = cmscurrIns mshead
            newstate  = 
             case ci of
                Nothing -> Stream mshead $ runMachine mstatetl
                (Just (Call n entpt)) -> rcall n entpt mstate
                (Just (Jump lbl)) -> 
                     if lbl <= (cmscurrIp mshead) 
                     then Stream (cmsZeroTheQstack mshead) $ 
                          runMachine mstatetl
                     else Stream (runCMS' ci mshead) $ 
                          runMachine mstatetl
                (Just (CondJump lbl)) -> 
                     if lbl <= (cmscurrIp mshead) 
                     then Stream (cmsZeroTheQstack mshead) $ 
                          runMachine mstatetl
                     else Stream (runCMS' ci mshead) $ 
                          runMachine mstatetl
                (Just _) -> Stream (runCMS' ci mshead) $ 
                            runMachine mstatetl

\end{code}
\end{singlespace}
}

\subsubsection{Recursive Function Transitions}\label{subsubsec:QSM:recursivefunctiontransitions}
In the Quantum stack machine,  all function calls are treated as 
recursive calls. This means that each function call directly affects the 
|Stream| of the machine state.

The function |rcall| makes this happen explicitly by creating a new 
|Stream|. The head of this |Stream| is always the non-terminating 
representation, i.e. a zeroed stack. The tail creates a new stack, starting
with the current stack values, and resetting the instruction pointer to 
the first instruction of the function, and altering the |Dump| and classical
stack appropriately. 

The effect of this is that when calling functions, one must look further and 
further down the stream of stacks to actually see results.

{
\begin{singlespace}
\begin{code}
rcall :: (Quantum a b) => Int -> EntryPoint -> MachineState a b -> 
         MachineState a b
rcall n entpt mstate
      = Stream ms0 mstaterest
        where cms = hd mstate
              ms0 = liftBMStoCMS (incCp . zeroTheQstack) cms
              mstate' = tl mstate
              mstaterest = runMachine' mstate' 

enterFunc :: (Quantum a b) => Int-> EntryPoint ->BMS a b -> BMS a b
enterFunc n entpt ms
  = BMS (quantumStack ms) newcs newcd 
            (codeMem ms) newip (d:dump ms) (namesupply ms)
    where newip = (entpt,0)
          newcd = getCode (codeMem ms) newip
          (newcs,cs') = stackSplitAt n $ classicalStack ms
          d = DumpCall (1+(currIp ms)) (ep ms) cs'


\end{code}
\end{singlespace}
}

\subsubsection{Machine transitions for each instruction}\label{subsubsec:QSM:machinetransitions}
The function |runBMS'| implements the actual machine transitions for 
each state of the machine.

{
\begin{singlespace}
\begin{code}

runCMS' :: (Quantum a b) =>  Maybe(Instruction a) -> CMS a b -> CMS a b
runCMS' Nothing c = c
runCMS' (Just ins) c = runCMS ins c

runCMS :: (Quantum a b) =>  Instruction a -> CMS a b -> CMS a b
runCMS AddCtrl cms = 
    cmsIncCp cms{controlStack = addControl $ controlStack cms}
runCMS UnCtrl cms = 
    cmsIncCp cms{controlStack = tlcs,
                 ctrldMS = newCtrldMS}
        where (tlcs,newCtrldMS) = unControl (controlStack cms) (ctrldMS cms)

runCMS QCtrl cms = 
    cmsIncCp cms{controlStack = ncs,
                    ctrldMS = newCtrldMS}
    where (ncs,newCtrldMS) = qControl (controlStack cms) (ctrldMS cms)

runCMS (QApply n transop ) cms 
   = cmsIncCp $ cms{ctrldMS = List.map (qapply n transop) $ ctrldMS cms}

runCMS ins cms = liftBMStoCMS (runBMS' ins) cms

qapply ::(Quantum a b) => Int -> UnitaryOp ->
        ((Int,ClassicalStack),( Controlled (QuantumStack a b) ,d)) ->
        ((Int,ClassicalStack), (Controlled (QuantumStack a b),d))
qapply i transop ((n,cs),(cq,d))
       = ((n',cs'),(cq',d))
         where ((n',cs'), cq') = qapply' i transop n cs cq

qapply'::(Quantum a b) => Int -> UnitaryOp -> Int-> ClassicalStack ->
        Controlled (QuantumStack a b) ->
        ((Int, ClassicalStack), Controlled (QuantumStack a b))
qapply' i transop n cs cq =
    ((n,Data.Stack.fromList cs'), cq''')
    where (topn, cs') = splitAt i $  QSM.ClassicalStack.toList cs
          trans = getTransform topn transop
          (names, cq') = unzipControl $ fmap (prepare (qorder trans)) cq
          cq'' = cTransform trans cq'
          cq''' = fmap (rotateInOrder $ reverse names) cq''
       
\end{code}
\end{singlespace}
}

\paragraph{Node construction} is done with the four instructions
|QLoad|, |QCons|, |QMove| and |QBind|. 


{
\begin{singlespace}
\begin{code}

runBMS' :: (Quantum a b) => Instruction a -> BMS a b -> BMS a b
runBMS' (QLoad nm v) ms 
   = let qs = quantumStack ms
     in incCp $ ms{quantumStack = StackQbit nm $ qv [((v,v),qs)]}

runBMS' (QCons nm c) ms 
   = let qs = quantumStack ms
     in incCp $ ms{quantumStack = StackCons nm $ dv [(c,([],qs))]}

runBMS' (QBind nm ) ms 
   = let qs = quantumStack ms
         ns = namesupply ms
         (ns', qs') = bind nm ns qs
     in incCp $ ms{quantumStack = qs', namesupply = ns'}

runBMS' (QMove nm ) ms 
   = let qs = quantumStack ms
         (val,cs) = popM $ classicalStack ms
         cval = case val of
                  Nothing -> Left 0
                  (Just a) -> a
     in incCp $ ms{quantumStack = StackInt nm $ cv [(cval,qs)],
                   classicalStack = cs}
\end{code}
\end{singlespace}
}

\paragraph{Node destruction} is the natural complement of construction
and is done by the two instructions |QUnbind| and |QDiscard|. Note
that |QDiscard| works on all types of nodes, but requires them to have
only a single sub-stack in each case.

{
\begin{singlespace}
\begin{code}
runBMS' (QDelete ) ms 
   = let qs = quantumStack ms
     in  incCp $  ms{quantumStack = discard  qs}
      

runBMS' (QDiscard ) ms 
   = let qs = quantumStack ms
         bc = branchCount qs
     in if bc == 1 
        then incCp $ ms{quantumStack = discard  qs,
                        classicalStack = pushM (topVal qs) (classicalStack ms) }
        else error badDiscard


runBMS' (QUnbind nm ) ms 
   = let qs = quantumStack ms         
     in incCp $ ms{quantumStack = unbind nm qs}
\end{code}
\end{singlespace}
}

\paragraph{Quantum stack manipulation} consists of two instructions,
|QPullup| and |QName|. 


{
\begin{singlespace}
\begin{code}
runBMS' (QPullup nm ) ms 
   = let qs = quantumStack ms         
     in incCp $ ms{quantumStack = rotateup  nm qs}



runBMS' (QName oldnm newnm ) ms 
   = let qs = quantumStack ms         
     in incCp $ ms{quantumStack = firstRenameInStack oldnm newnm qs}
\end{code}
\end{singlespace}
}

\paragraph{Quantum control} instructions allow us to apply instructions
to different sub-branches of a node.  



The details of the functions used in this code are below.

{
\begin{singlespace}
\begin{code}

runBMS' (Use lbl) ms
    = let qs = quantumStack ms
      in doUse qs lbl ms

runBMS' (Split cns_lbl_list) ms
    = let qs = quantumStack ms
      in doSplit qs (Map.fromList cns_lbl_list) ms

runBMS' (Measure lbl0 lbl1) ms
    = let qs = quantumStack ms
      in doMeasure qs lbl0 lbl1 ms

runBMS' EndQC ms
   = let dmp = head $ dump ms
     in doqcEnd dmp ms
\end{code}
\end{singlespace}
}

\paragraph{Classical control} comprises the standard jump / call / 
return types of instructions. Note that the transitions here are based
on the assumption one is deep enough in the stream to actually do a 
|Call|. For example, at the start of the stream, a |Call| instruction
actually just returns a zeroed quantum stack. See the sub-section on
function calls: \vref{subsubsec:QSM:recursivefunctiontransitions}.

{
\begin{singlespace}
\begin{code}
runBMS' (Jump lbl) ms
   | lbl > ( currIp ms)
     = ms{runningCode = newcode, instructionPointer = newptr}
   | otherwise = error backwardsJump
       where newptr = (ep ms, lbl)
             newcode = getCode (codeMem ms) newptr

runBMS' (CondJump lbl) ms
    = let (value, cs) = popM $ classicalStack ms
      in case value of 
           Just (Right False) -> runBMS' (Jump lbl) ms{classicalStack = cs}
           _ -> incCp ms{classicalStack = cs}              
   
runBMS' (Call _ _ ) _ = error wrongCall

runBMS' (Return n) ms
     = let cs = classicalStack ms
           d = head $ dump ms
       in BMS (quantumStack ms) (addn n cs $ saveClsStack d)
              (getCode (codeMem ms) (returnEp d, returnLabel d))
              (codeMem ms) 
              (returnEp d, returnLabel d)
              (tail $ dump ms)
              (namesupply ms)
\end{code}
\end{singlespace}
}

\paragraph{Classical instructions} manipulate the classical stack
in the machine allowing standard integer and Boolean operations.


{
\begin{singlespace}
\begin{code}
runBMS' CPop ms
     = incCp ms{classicalStack = snd $ popM $ classicalStack ms}

runBMS' (CGet n) ms
     = let cs = classicalStack ms
       in incCp ms{ classicalStack = push (stackElem cs n) cs}

runBMS' (CPut n) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = stkput n cs}

runBMS' (CApply cop) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = (getStackOp cop) cs}

runBMS' (CLoad value) ms
     = let cs = classicalStack ms
       in incCp ms{classicalStack = push value cs}
   
runBMS' (NoOp) ms = incCp ms
\end{code}
\end{singlespace}
}

\subsubsection{Support for data casing, measure and use}
\label{subsubsec:supportforcase}
The function |doUse| handles the work of splitting a |StackInt| node down
so that a series of instructions may be executed on each of the
subbranches. Applying this to a |StackZero| element results in a
no-operation, while applying it to anything else will cause a 
machine exception. The general pattern of this function is
repeated in |doSplit| and |doMeasure| below. 

{\begin{singlespace}
\begin{code}
doUse :: (Quantum a b) => QuantumStack a b -> Label-> BMS a b -> 
         BMS a b
doUse qs@(StackZero) _ ms
          = incCp ms
doUse qs@(StackInt _ _) lbl ms
      = let qss = breakQuantum qs
            cs = classicalStack ms
            dumpU = DumpQControl (1 + (currIp ms)) (zip qss $ repeat lbl)
                     (fromInteger 0) cs
        in BMS (fromInteger 0) cs [EndQC] (codeMem ms) 
                 (instructionPointer ms) (dumpU : dump ms) (namesupply ms)
doUse _ _ _ = error useDataCheck
\end{code}
\end{singlespace}
}

The |doSplit| instruction works only on |StackCons| nodes.

{\begin{singlespace}
\begin{code}
doSplit :: (Quantum a b) => QuantumStack a b -> Map Constructor Label ->
           BMS a b -> BMS a b
doSplit qs@(StackZero) _ ms
          = incCp ms

doSplit qs@(StackCons _ _) jumpMap ms
      = let qss = breakQuantum qs
            cs = classicalStack ms
            dumpU = DumpQControl (1 + (currIp ms)) (associateCons jumpMap qss)
                     (fromInteger 0) cs
        in BMS (fromInteger 0) cs [EndQC] (codeMem ms) 
                  (instructionPointer ms) (dumpU : dump ms)  (namesupply ms)

doSplit _ _ _ = error splitDataCheck
\end{code}
\end{singlespace}
}

Finally, |doMeasure| measures a \qbit{} and sets up the
system for executing code on its \ket{0} and \ket{1} branches.
Recall that the density matrix notation, which is implemented by
the quantum stack, has four values for a \qbit. The two off-diagonal
values are discarded immediately by this instruction, leaving the
diagonal values (\ket{0} and \ket{1}).

{\begin{singlespace}
\begin{code}
doMeasure :: (Quantum a b) => QuantumStack a b -> Label -> Label -> 
             BMS a b -> BMS a b
doMeasure qs@(StackZero) _ _ ms
          = incCp ms

doMeasure qs@(StackQbit _ _) lbl0 lbl1 ms
      = let qss = breakQuantum qs
            qss' = List.filter (\ (StackQbit _ qvals) -> 
                                    (uncurry (==) . head . keys) qvals ) qss
            cs = classicalStack ms
            dumpU = DumpQControl (1 + (currIp ms)) (assocQbs qss' lbl0 lbl1)
                     (fromInteger 0) cs
        in BMS (fromInteger 0) cs [EndQC] (codeMem ms) 
               (instructionPointer ms) (dumpU : dump ms)  (namesupply ms)
doMeasure _ _ _ _ = error measureDataCheck 
\end{code}
\end{singlespace}
}

Once a split, measure or use is started, intermediate results 
are accumulated on the dump. The structure of this dump element is
shared by all three of the instructions. Stepping through the sub-results
and finalizing the result is done by the |EndQC| instruction, which
uses the function |docqEnd| below. The first part of the definition 
below handles the case when all sub-stacks have been done. This returns
the quantum stack to a merge of all the intermediate results and removes
the intermediate result element from the dump.

{\begin{singlespace}
\begin{code}
doqcEnd :: (Quantum a b) => DumpElement a b -> BMS a b -> BMS a b
doqcEnd (DumpQControl ret [] resultqs savecs) ms
         = BMS (resultqs + (quantumStack ms)) savecs
              (getCode (codeMem ms) ((ep ms), ret))
              (codeMem ms) ((ep ms, ret)) (tail $ dump ms)  (namesupply ms)
\end{code}
\end{singlespace}
}

In the second part of the definition, the dump element is changed to
add in the current intermediate result, while removing the next leg 
to be executed and making it the current quantum stack.

{\begin{singlespace}
\begin{code}
doqcEnd (DumpQControl ret ((nextqs,nextlbl):qss) resultqs savecs) ms
    = let dmp = DumpQControl ret qss 
                (resultqs + (quantumStack ms)) savecs
      in BMS nextqs savecs
         (getCode (codeMem ms) ((ep ms), nextlbl))
         (codeMem ms) ((ep ms, nextlbl)) (dmp : (tail $ dump ms))  
          (namesupply ms)
doqcEnd _ _ = error qcontrolBadEnd
\end{code}
\end{singlespace}
}   

Some of the details of associating various sub-stacks with the appropriate
labels in the executing code is broken out as separate functions below.

{\begin{singlespace}
\begin{code}
assocQbs :: (Quantum a b) => [QuantumStack a b] -> Label -> Label -> 
            [(QuantumStack a b, Label)]
assocQbs [] _ _ = []
assocQbs ((qs@(StackQbit _ qvals)):qss) lbl0 lbl1
    = let (v,_) = head $ keys qvals
      in (if (v == head basis) then (qs, lbl0)
          else (qs, lbl1)):assocQbs qss lbl0 lbl1
assocQbs _ _ _ = error measureDataCheck

associateCons :: (Quantum a b) => Map Constructor Label->
                 [QuantumStack a b] -> [(QuantumStack a b, Label)]
associateCons jumpMap [] = []
associateCons jumpMap ((qs@(StackCons _ dvals)):qss)
    = let cons = head $ keys dvals
          lbl = findWithDefault endQCLabel cons jumpMap
      in (qs, lbl):associateCons jumpMap qss
associateCons _ _ = error splitDataCheck
\end{code}
\end{singlespace}
}

\subsubsection{Support for node construction and deletion}
\label{subsubsec:supportfornode}
The act of binding nodes to data nodes requires 
traversing the quantum stack for the desired name and creating a new
name for it. That new name is then attached to the data node.

{
\begin{singlespace}
\begin{code}
bind :: (Quantum a b) => StackAddress -> Int -> QuantumStack a b 
     -> (Int,QuantumStack a b)
bind nm ns theQstack@(StackCons dname dvals)
   | 1 == Map.size dvals 
       = let [(c,(bvs,qs))] = Map.toList dvals
             ns' = ns + 1
             numBound = length bvs
             newnm = "c"++(show ns)++ "_" ++ 
                     (show numBound) ++ "_" ++ nm
         in  (ns',StackCons dname $ dv [(c,(newnm:bvs, 
                                            firstRenameInStack nm newnm qs))])
   | otherwise = error  bindMultiCons

bind _ _ _ = error bindDataCheck
\end{code}
\end{singlespace}
}

Unbinding node from data nodes requires 
renaming the bound node and removing from the list of those 
 attached to the data node.

{
\begin{singlespace}
\begin{code}
unbind :: (Quantum a b) => StackAddress -> QuantumStack a b 
       -> QuantumStack a b
unbind nm (StackCons dname dvals)
   | 1 == Map.size dvals 
       = let [(c,(bvs,qs))] = Map.toList dvals
             numBound = length bvs
             oldnm = head bvs
             qs' = rotateup oldnm qs
         in if (numBound == 0) 
            then error unbindNothingBound
            else StackCons dname $ dv [(c,(tail bvs, 
                                         setName nm qs'))]
   | otherwise = error unbindBadCons 

unbind _ _ = error unbindDataCheck
\end{code}
\end{singlespace}
}

\subsubsection{Support for unitary transforms}
\label{subsubsec:supportforunitary}
Applying unitary transforms is done via  matrix multiplication.
To do the multiplication requires transforming the stack to a matrix,
then using the new values of the matrix to reset the stack. 

The first defined function, |cTransform|, will transform a |Controlled|
quantum stack. This will result in one of four possibilities:
\begin{equation}
\mathrm{cTransform}\ T\ Q =
\begin{cases}
Q & \mathrm{IdentityOnly\ control}\\
T Q & \mathrm{LeftOnly\ control}\\
Q T^{*} & \mathrm{RightOnly\ control}\\
T Q T^{*} & \mathrm{Full\ control}
\end{cases}
\end{equation}

{
\begin{singlespace}
\begin{code}
cTransform :: (Quantum a b)=>Trans b -> Controlled (QuantumStack a b) -> 
             Controlled (QuantumStack a b)
cTransform mtrans q@(IdentityOnly q') = q

cTransform mtrans (LeftOnly q)
    = LeftOnly $ trimStack  $ ctrans' (qorder mtrans) tr q
          where tr = \ qs -> matByStack mtrans qs

cTransform mtrans (RightOnly q) 
    = RightOnly $ trimStack $ ctrans' (qorder mtrans) tr q
          where tr = \qs -> stackByMat qs $ conjtrans mtrans

cTransform mtrans (Full q)
    = Full $ trimStack $ ctrans' (qorder mtrans) tr q
          where tr = \qs -> genmatmul (+) (*^) 
                           (matByStack mtrans qs) (conjtrans mtrans)


ctrans' :: (Quantum a b) => Int ->
           (QuantumStack a b -> Matrix (QuantumStack a b))->
           QuantumStack a b -> QuantumStack a b
ctrans' order f q = setValsFromMat order (f q) q

\end{code}
\end{singlespace}
}

The function |matByStack| performs a matrix multiplication of
the subject matrix with a quantum stack. The quantum stack is first
transformed to a matrix of the appropriate size. The result is a 
matrix of quantum stacks.

{
\begin{singlespace}
\begin{code}
matByStack :: (Quantum a b) => Matrix b -> QuantumStack a b -> 
              Matrix (QuantumStack a b)
matByStack m StackZero = zeroMat $ qorder m
matByStack _ st@(StackData _) 
    = error $ matByStackError "data" (show st)
matByStack _ st@(StackCons _ _) 
    = error $ matByStackError "Constructor" (show st)
matByStack m st@(StackInt _ _) 
    = error $ matByStackError "Int"  $  
            (show m) ++  (show st)
matByStack m sqb@(StackQbit _ _) 
    = newVals 
      where newVals = genmatmul (+) (^*) m (stackToMat order sqb)
	    order = qorder m
\end{code}
\end{singlespace}
}



The function |stackByMat| is complementary to matByStack, multiplying 
a quantum stack on the right by a matrix and is
used in |cTransform| for |RightOnly| controlled transforms.

{
\begin{singlespace}
\begin{code}
stackByMat :: (Quantum a b) => QuantumStack a b -> Matrix b -> 
              Matrix (QuantumStack a b)
stackByMat StackZero m = zeroMat $ qorder m
stackByMat st@(StackData _) _ 
    = error $ stackByMatError "data" (show st)
stackByMat st@(StackCons _ _) _ 
    = error $ stackByMatError "Constructor" (show st)
stackByMat st@(StackInt _ __) m 
    = error $ stackByMatError "Int"  $  
            (show m) ++  (show st)
stackByMat sqb@(StackQbit _ _) m
    = newVals 
      where newVals = genmatmul (+) (*^) (stackToMat order sqb) m
	    order = qorder m

\end{code}
\end{singlespace}
}

The function |setValsFromMat| and dependent function |svfm|
extract the values from a matrix of quantum stacks and
assigns them as new sub-stacks of the argument quantum stack.

{
\begin{singlespace}
\begin{code}
setValsFromMat ::(Quantum a b) => Int->
                 Matrix (QuantumStack  a b) -> 
                 QuantumStack a b -> 
                 QuantumStack a b
setValsFromMat n m qb@(StackQbit s qvl) = svfm (names n qb) n m qb 
setValsFromMat n m q = error $ setValsTypeError n m q
     
\end{code}
\end{singlespace}
}


{
\begin{singlespace}
\begin{code}
svfm :: (Quantum a b) => [String]-> Int-> Matrix (QuantumStack  a b) -> 
        QuantumStack a b -> QuantumStack a b
svfm nms 1 m qb@(StackZero)
    = StackQbit (head nms) (qv [((a,b), indexM (ei a) (ei b) m) | 
                                a<- basis, b<- basis ])
svfm nms n m sq@(StackZero) 
    = StackQbit (head nms) (qv [((a,b), svfm (tail nms) 
                                 (n-1) (grab  (ei a) (ei b) m) (StackZero))
                                | a<- basis, b<- basis ])

svfm nms n m sq@(StackQbit _ qvl) 
    | n == 1 = StackQbit (head nms) (qv [((a,b), indexM (ei a) (ei b) m) | 
                                a<- basis, b<- basis ])
    | n > 1 = StackQbit (head nms) (qv [((a,b), svfm (tail nms) 
                                         (n-1) (grab  (ei a) (ei b) m) (val (a,b) qvl))
                                | a<- basis, b<- basis ])
    | otherwise =  error setValsError

svfm _ _ _ _
    = error setValsDataError

\end{code}
\end{singlespace}
}

Converting a stack to a matrix is done by a recursive descent down
the stack. For example, if 
converting to a $4\times 4$ matrix (two \qbit{}s), 
|stackToMat| converts the four sub matrices of the second level 
\qbit. These four $2\times 2$ matrices 
are then amalgamated via "pasting" to create  a $4\times 4$ matrix.

{
\begin{singlespace}
\begin{code}
stackToMat :: (Quantum a b) => Int->QuantumStack  a b -> 
              Matrix (QuantumStack a b)
stackToMat 1 qs 
    =  [ [ val (a,b) (qvalues qs) 
                     | b<-basis] 
               | a<- basis]


stackToMat n qs 
    | n > 1 = reduceM theMat
    | otherwise = error stackToMatError
    where theMat =  [[findWithDefault (zeroMat (pow 2 (n-1))) (a,b) 
                            prev | b<- basis] | a<- basis]
	  prev = Map.map (stackToMat (n-1)) (qvalues qs)

pow :: Int -> Int -> Int
pow n m  = foldl (*) 1 $ take m $ repeat n

zeroMat  n =  (take n $ repeat $ take n $ repeat StackZero)
\end{code}
\end{singlespace}
}

\subsubsection{Miscellaneous transition support}
\label{subsubsec:miscellaneoustransitionsupport}
This section has a variety of simple functions used in the operation
and setup of the quantum machine.

{
\begin{singlespace}
\begin{code}

cmsZeroTheQstack::(Quantum a b) => CMS a b -> CMS a b
cmsZeroTheQstack  = liftBMStoCMS zeroTheQstack

zeroTheQstack::(Quantum a b) => BMS a b -> BMS a b
zeroTheQstack ms = ms{quantumStack = fromInteger 0}

noCode :: Memory a
noCode = Map.singleton mainproglabel []

startMachine :: (Quantum a b ) => (QuantumStack a b ) -> Memory a -> 
                MachineState a b

startMachine qs mem = 
   return $ CMS mem [] ("main", 0) (getCode mem ("main", 0))
              [((0,emptyStack), (Full qs,[]))]



hasProc :: (Basis a) => Memory a -> EntryPoint -> Bool
hasProc = flip (Map.member)

getCode :: (Basis a) => Memory a -> (EntryPoint, Label) -> [Instruction a]
getCode mem (ep,start)
    | start == endQCLabel = [EndQC]
    | otherwise =  drop start $ 
                   findWithDefault emptyCodeBlock ep mem

ep :: (Quantum a b ) => BMS a b -> EntryPoint
ep  = fst . instructionPointer

currIp  :: (Quantum a b ) => BMS a b -> Label
currIp = snd . instructionPointer 

cmscurrIp  :: (Quantum a b ) => CMS a b -> Label
cmscurrIp = snd . cmsInstructionPointer 

currIns  :: (Quantum a b ) => BMS a b -> Maybe (Instruction a)
currIns = ci' . runningCode

cmscurrIns  :: (Quantum a b ) => CMS a b -> Maybe (Instruction a)
cmscurrIns = ci' . cmsRunningCode

ci' :: [a] -> Maybe (a)
ci' ([]) = Nothing
ci' (a:_) = Just a

emptyCodeBlock :: (Basis a) => Code a
emptyCodeBlock =  []

incCp  :: (Quantum a b) => BMS a b -> BMS a b
incCp ms = ms{instructionPointer = (ep ms, 1 + (currIp ms)),
              runningCode = tail $ runningCode ms}

cmsIncCp  :: (Quantum a b) => CMS a b -> CMS a b
cmsIncCp = liftBMStoCMS incCp

endQCLabel = -1
\end{code}
\end{singlespace}
}
