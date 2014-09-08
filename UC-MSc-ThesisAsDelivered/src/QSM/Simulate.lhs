%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"
\subsection{Determination of simulation}\label{subsec:QSM:simulation}

%if false
\begin{code}
module QSM.Simulate where
import QSM.QSM
import Data.Map as Map
import Data.LazyNum
import Data.List as List
import Data.Tuples
import System.Random
import Data.Complex
import Simulator.SimBase

\end{code}
%endif
\subsubsection{Canonicalizing the quantum stack}\label{subsubsec:QSM:canonicalization}

When calculating a simulation of the quantum stack, the display of variables
needs to be in a predictable order. The rule for this is to group
all dependant stacks of a data type with that datatype.

{
\begin{singlespace}
\begin{code}

canonicalize :: (Quantum a b ) => QuantumStack a b -> QuantumStack a b
canonicalize (StackCons nm dvals) = 
    let canonRot = (canonicalize) . (uncurry rotateInOrder)
    in StackCons nm $ 
              Map.map regroup $
                Map.map ((app2of2 canonRot) . dup) dvals

canonicalize q = q

-- regroup -- = (pair (fst . fst) (pair (fst . snd)  id)) 
regroup :: ((a,b),c) -> (a,c)
regroup ((a,b),c) = (a,c)
                                 
\end{code}
\end{singlespace}
}

\subsubsection{Picking sub-trees}
To "run" the quantum machine, a random number between 0 and 1 is chosen. 
This is used to create a choice throughout the tree. The function 
|chooseIt| does this, returning a list of pairs of strings. Each
pair is composed of the node name and type paired with its value.


{\begin{singlespace}
\begin{code}
 
chooseIt :: QuantumStack OurBasis LazyNum -> IO (Double, [(String,String)])
chooseIt q = do rval <- randomIO
                return $ (rval, chooseAndDescend rval q [])

chooseAndDescend ::  Double -> QuantumStack OurBasis LazyNum ->
                    [(String,String)]-> [(String,String)]
chooseAndDescend _ (StackData _ ) lis = lis
chooseAndDescend _ (StackZero ) lis = lis
chooseAndDescend f s lis 
    | f > (realPart . approximate . trace) s = ("","DIVERGE"):lis
chooseAndDescend f (StackQbit nm qvals) lis =
                 let possibleChoices = List.filter ((uncurry (==)) . fst) $
                                            Map.toAscList qvals
                     (f',sval,choice) = chooseq f possibleChoices
                 in chooseAndDescend f' choice  ((nm,sval):lis) -- ((((show f')++" "++nm),sval):lis)
chooseAndDescend f (StackInt nm cvals) lis =
                 let (f',sval,choice) = choosei f $ Map.toAscList cvals
                 in chooseAndDescend f' choice ((nm,sval):lis) -- ((((show f')++" "++nm),sval):lis)
chooseAndDescend f (StackCons nm dvals) lis =
                 let (f',sval,choice) = choosed f $ List.map assoc $ Map.toAscList dvals
                 in chooseAndDescend f' choice ((nm,sval):lis) -- ((((show f')++" "++nm),sval):lis)

chooseq  :: Double -> [((OurBasis,OurBasis),QuantumStack OurBasis LazyNum)] ->
            (Double, String, QuantumStack OurBasis LazyNum)
chooseq  f lis
    = (f',show $ fst $ fst chosen, snd chosen) 
         where (f',chosen) = choose f lis

choosei  :: Double -> [(ClassicalData,QuantumStack OurBasis LazyNum)] ->
            (Double,String, QuantumStack OurBasis LazyNum)
choosei  f lis
    = (f',show $ fst  chosen, snd chosen) 
         where (f',chosen) = choose f lis

choosed  :: Double -> 
            [((Constructor, [StackAddress]),QuantumStack OurBasis LazyNum)] ->
            (Double,String, QuantumStack OurBasis LazyNum)
choosed  f lis
    = (f', show $ fst  chosen, snd chosen) 
        where (f',chosen) = choose f lis


choose :: Double -> [(a,QuantumStack OurBasis LazyNum)] ->
          (Double,(a,QuantumStack OurBasis LazyNum))
choose f lis = (f',lis !! ind)
               where  traced = List.map (realPart . approximate) $ scanl1 (+) $ List.map (trace . snd) lis
                      ind = (length $ takeWhile (< f) traced)
                      f' = if (0 == ind) then f
                           else f - (traced !! (ind-1))

\end{code}
\end{singlespace}
}
