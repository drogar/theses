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


mat1::Matrix LazyNum

mat1 = [ [(Svar "a"), (Svar "b")], [(Svar "c"), (Svar "d")]]



s00 :: QuantumStack OurBasis LazyNum
s01 :: QuantumStack OurBasis LazyNum
s10 :: QuantumStack OurBasis LazyNum
s11 :: QuantumStack OurBasis LazyNum
s00 = StackData (Svar "A")
s01 = StackData (Svar "B")
s10 = StackData (Svar "C")
s11 = StackData (Svar "D")

makekey::Int->(OurBasis,OurBasis)
makekey 0 = (Zero,Zero)
makekey 1 = (Zero,One)
makekey 2 = (One,Zero)
makekey 3 = (One,One)
makeqdata :: Int->Int ->((OurBasis,OurBasis), QuantumStack OurBasis LazyNum)
makeqdata 0 i = ((Zero,Zero),StackData (Svar (show i)))
makeqdata 1 i = ((Zero,One),StackData (Svar (show i)))
makeqdata 2 i = ((One,Zero),StackData (Svar (show i)))
makeqdata 3 i = ((One,One),StackData (Svar (show i)))

makeqb :: String ->Int -> QuantumStack OurBasis LazyNum
makeqb nm i = StackQbit nm (qv [makeqdata j (i+j) | j <- [0..3]])

makeqb2 nm nm2 i = StackQbit nm (qv [(makekey j, makeqb nm2 (i+(j*4))) | j <- [0..3]])
makeqb3 nm nm2 nm3 i = StackQbit nm (qv [(makekey j, makeqb2 nm2 nm3 (i+(j*16))) | j <- [0..3]])
vl :: QV OurBasis  LazyNum

vl = qv [((Zero,Zero),s00),
	 ((Zero,One),s01),
	 ((One,Zero),s10),
	 ((One,One),s11)]

qb1only = StackQbit "bit1" (qv [((Zero,Zero), s00)])
qb2only = StackQbit "bit2" (qv [((Zero,Zero), qb1only)])
qb3only = StackQbit "bit3" (qv [((Zero,Zero), qb2only)])



tstackq3 ::TD
tstackq3 = StackQbit "third" (qv [((Zero,Zero),tstackqq),
				   ((Zero,One),tstack2),
				   ((One,Zero),tstack3),
				   ((One,One),tstack4)])
type TD = QuantumStack OurBasis LazyNum

tstackb1 = StackInt "fb" (cv [(Left 0, s00),(Left 1, s01)])
tstackb2 = StackInt "fb" (cv [(Left 0, s10),(Left 1, s11)])

tsb = StackInt "f" (cv [(Left 0, tstackb1),(Left 1, tstackb2)])

tsb2 = StackInt "f" (cv [(Left 0, tstack1),(Left 1, tstack2)])

tstack1 :: TD
tstack1 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "00 00")),
				 ((Zero,One),StackData (Svar "00 01")),
				 ((One,Zero),StackData (Svar "00 10")),
				 ((One,One),StackData (Svar "00 11"))])
tstack2 :: TD
tstack2 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "01 00")),
				 ((Zero,One),StackData (Svar "01 01")),
				 ((One,Zero),StackData (Svar "01 10")),
				 ((One,One),StackData (Svar "01 11"))])
tstack3 :: TD
tstack3 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "10 00")),
				 ((Zero,One),StackData (Svar "10 01")),
				 ((One,Zero),StackData (Svar "10 10")),
				 ((One,One),StackData (Svar "10 11"))])
tstack4 :: TD
tstack4 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "11 00")),
				 ((Zero,One),StackData (Svar "11 01")),
				 ((One,Zero),StackData (Svar "11 10")),
				 ((One,One),StackData (Svar "11 11"))])

tstackqq ::TD
tstackqq = StackQbit "second" (qv [((Zero,Zero),tstack1),
				   ((Zero,One),tstack2),
				   ((One,Zero),tstack3),
				   ((One,One),tstack4)])


toMat q = Map.map (stackToMat 1) (qvalues q)


mlnToMqs ::Matrix LazyNum -> Matrix (QuantumStack OurBasis LazyNum)
mlnToMqs m = (Prelude.map (Prelude.map (StackData::LazyNum -> QuantumStack  OurBasis LazyNum))  m)



stack1 :: TD
stack1 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "00 00")),
				 ((Zero,One),StackData (Svar "00 01")),
				 ((One,Zero),StackData (Svar "00 10")),
				 ((One,One),StackData (Svar "00 11"))])
stack2 :: TD
stack2 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "01 00")),
				 ((Zero,One),StackData (Svar "01 01")),
				 ((One,Zero),StackData (Svar "01 10")),
				 ((One,One),StackData (Svar "01 11"))])
stack3 :: TD
stack3 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "10 00")),
				 ((Zero,One),StackData (Svar "10 01")),
				 ((One,Zero),StackData (Svar "10 10")),
				 ((One,One),StackData (Svar "10 11"))])
stack4 :: TD
stack4 = StackQbit "first" (qv [((Zero,Zero),StackData (Svar "11 00")),
				 ((Zero,One),StackData (Svar "11 01")),
				 ((One,Zero),StackData (Svar "11 10")),
				 ((One,One),StackData (Svar "11 11"))])

stack ::TD
stack = StackQbit "second" (qv [((Zero,Zero),stack1),
				 ((Zero,One),stack2),
				 ((One,Zero),stack3),
				 ((One,One),stack4)])

stackem ::TD
stackem = StackQbit "second" (qv [((Zero,Zero),s00),
				  ((One,One),s11)])


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



iomultistep :: (Quantum a b)=>Int -> 
             MachineState a b -> IO(MachineState a b)
iomultistep 0  ms = return ms
iomultistep n  ms 
    = do let ms' = runMachine ms
         iomultistep (n-1)  ms'

r00 :: TD
r00 = StackQbit "r" (qv [((Zero,Zero), StackData (Snum 0.25))])

r01 :: TD
r01 = StackQbit "r" (qv [((Zero,Zero), StackData (Sbop Div (Snum 0.25) (Sfun SquareRoot (Snum 2.0))))])

r11 :: TD
r11 = StackQbit "r" (qv [((Zero,Zero), StackData (Snum 0.325))])

qvls = (qv [((Zero,Zero), r00),
                      ((Zero,One), r01),
                      ((One,Zero), r01),
                      ((One,One), r11)])
q :: TD 
q = StackQbit "q" qvls

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

initControl :: [ControlStack OurBasis LazyNum]
initControl = []

ic2 = addControl initControl

(ctl1,qsctld) = withControl ic2 [Full q] 

dctl1 = removeControl ctl1 qsctld

--ctl2 = addQbitToCS [r11, r11, r11, r11] ctl1

--dctl2 = dropControl ctl2

(ctlts,tsqs) = withControl ic2 [Full tsb]

(ctlts2, tsqs2) = withControl ctlts tsqs

tsunctl2 = removeControl ctlts2 tsqs2


q00 :: TD
q00 = StackQbit "q" (qv [((Zero,Zero), StackData (Snum 0.5))])
q11 = StackQbit "q" (qv [((One,One), StackData (Snum 0.5))])



hrq :: TD 
hrq = StackQbit "r" (qv [((Zero,Zero), q00),
                      ((One,Zero), q00),
                      ((Zero,One), q00),
                      ((One,One), q00)])

testsim = StackQbit "q1" (qv [((Zero,Zero), 
                                StackQbit "q2" (qv [((Zero,Zero), StackData (Snum 0.25)),
                                                ((One,One), StackData (Snum 0.25))])),
                              ((One,One), 
                                StackQbit "q2" (qv [((Zero,Zero), StackData (Snum 0.25)),
                                                ((One,One), StackData (Snum 0.25))]))])

leg00 q = qv [((Zero,Zero),q)]

nqb nm sub = StackQbit nm $ leg00 sub

lq0 :: TD
lq0 = StackQbit "lq0" (qv [((Zero,Zero),StackData (Snum 1.0))])

lq1 :: TD
lq1 = nqb "lq1" lq0

lq2 :: TD
lq2 = nqb "lq2" lq1

lq3 :: TD
lq3 = nqb "lq3" lq2

nil :: TD
nil = StackCons "nil" $ dv [("Nil",([],lq3))]

cons nm bnd1 bnd2 sub = StackCons nm $ dv [("Cons",([bnd1,bnd2],sub))]

lc1 :: TD
lc1 = cons "lc1" "lq3" "nil" nil
lc2 :: TD
lc2 = cons "lc2" "lq2" "lc1" lc1
lc3 :: TD
lc3 = cons "lc3" "lq1" "lc2" lc2
lc4 :: TD
lc4 = cons "lc4" "lq0" "lc3" lc3


p4 = prepare 4 lc4

nms = reverse $ fst p4

rolc4 = rotateInOrder nms $ snd p4

cqlc4 = unzipControl $ fmap (prepare 4) (Full lc4)

qlc4 = snd cqlc4

utrans :: Trans LazyNum
utrans = getTransform (List.map Left [15,7,0]) UM

cstack1:: ClassicalStack
cstack1 = Stack [Left 15,Left 7, Left 0]


 
\end{code}
