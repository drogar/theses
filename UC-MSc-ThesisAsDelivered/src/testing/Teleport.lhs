\begin{code}
module TestStackFunctions where
import QSM.Transformations
import QSM.QSM
import TestMatrix
import QSM.QuantumStack
import Data.LazyNum
import Data.ClassComp
import Data.Basis
import Data.Matrix
import Data.Map as Map
import Data.List as List
import Data.Stream
import Data.InfList
import Assembler.AssemParser
import QSM.ControlStack
import QSM.Simulate
import QSM.QSPretty


mat1::Matrix LazyNum

mat1 = [ [(Svar "a"), (Svar "b")], [(Svar "c"), (Svar "d")]]



s00 :: QuantumStack OurBasis LazyNum
s01 :: QuantumStack OurBasis LazyNum
s10 :: QuantumStack OurBasis LazyNum
s11 :: QuantumStack OurBasis LazyNum
s00 = StackData (Svar "\\alpha\\overline{\\alpha}")
s01 = StackData (Svar "\\alpha\\overline{\\beta}")
s10 = StackData (Svar "\\beta\\overline{\\alpha}")
s11 = StackData (Svar "\\beta\\overline{\\beta}")

makekey::Int->(OurBasis,OurBasis)
makekey 0 = (Zero,Zero)
makekey 1 = (Zero,One)
makekey 2 = (One,Zero)
makekey 3 = (One,One)

n :: TD

n = StackQbit "nu" $ qv [((Zero,Zero),s00),
	 ((Zero,One),s01),
	 ((One,Zero),s10),
	 ((One,One),s11)]


type TD = QuantumStack OurBasis LazyNum

setStack :: TD -> BMS OurBasis LazyNum -> BMS OurBasis LazyNum
setStack qs bms = bms{quantumStack = qs}


--qtst = StackQbit "b1" (qv [((Zero,Zero), (StackData (fromInteger 1)))])

os = StackData (fromInteger 1)
mtst :: BMS OurBasis LazyNum

mtst = BMS os emptyStack [QLoad "p" Zero, QApply 0 NotGate] noCode ("emp",0) [] 0

newmst :: [Instruction OurBasis] -> BMS OurBasis LazyNum -> BMS OurBasis LazyNum
newmst iis ms = ms{runningCode = iis}

strmtst :: MachineState OurBasis LazyNum

strmtst = return $ makeCMS mtst
newstrmst :: [Instruction OurBasis] -> MachineState OurBasis LazyNum 
          -> MachineState OurBasis LazyNum
newstrmst iis = liftS $ liftBMStoCMS  (newmst iis)

iset = [CLoad (Left 1),CLoad (Left 5), CLoad (Left 12), Jump 6, 
        CApply CAdd, QMove "I", QLoad "p" Zero,
        QDiscard, QLoad "p2" Zero, QApply 0 NotGate]

multistep :: (Quantum a b)=>Int -> MachineState a b -> MachineState a b
multistep 0 ms = ms
multistep n ms = multistep (n-1) $ runMachine ms



iomultistep :: Int -> 
             MachineState OurBasis LazyNum -> IO(MachineState OurBasis LazyNum)
iomultistep 0  ms = return ms
iomultistep n  ms 
    = do let ms' = runMachine ms
         putStrLn $ ltxPretty ((fst . splitcontrolled . fst . snd . head . ctrldMS) $ hd ms')
         iomultistep (n-1)  ms'

replaceStack t = (liftS. liftBMStoCMS) (setStack t)

--ms <- loadFile "teletest1.qpo"


loadFile :: String -> 
            IO (MachineState OurBasis LazyNum) 
loadFile fpth
    = do inss <- readFile fpth
         Right (trs,mb) <- parseQPA "" "" inss
         print mb
         let ip = (mainproglabel, 0)
             ms = startMachine (fromInteger 1) noCode
             ms' = do mstate <- ms
                      return mstate{cmsCodeMem = mb,
                                    cmsInstructionPointer=ip,
                                   cmsRunningCode = getCode mb ip}
         return ms'


 
\end{code}
