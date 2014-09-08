\begin{code}

module TestMachine where
import MachineState
import MachineOperation
import StackFunctions
import Transformations
import Basis
import LazyNum
import Stream
import InfList
import Data.Map as Map


qs :: Stream (Qstack OurBasis LazyNum)
qs = fmap (rotateup "r") $ return $ fromInteger 0

s5 = StackBit "s" $ Map.singleton Zero (StackData (Snum 0.5))

rmds5 = trimStack $ removeSubStacks [Zero] s5

alqb = allocQbit "q" $ return rmds5
stm = stackToMat 1 $ hd alqb

trsnf = transform hadamard $ hd alqb

prog1 :: CodeMemory OurBasis

prog1 = [SetBit, NewQbit]

prog2 = prog1 ++ [StartLoop Nothing [UseName "f", Pullup, EndLoop]]

prog3 = prog1 ++ [StartLoop (Just 2) prog2]

r1 = Prelude.map (\i -> addToLevel i prog1 [SetBit]) 

r2 = Prelude.map (\i -> addToLevel i prog2 [SetBit]) 

r3 = Prelude.map (\i -> addToLevel i prog3 [SetBit]) 

slvl = singleton mainproglabel 0

taslvl = toAscList slvl

ftaslvl = Prelude.map fst taslvl

zft = zip ftaslvl [0..]

fzft = Map.fromAscList zft
\end{code}
