%include polycode.fmt
\subsection{Description of the control stack}\label{subsec:controlstackdescription}

%if false
\begin{code}

module QSM.ControlStack where
import Data.ClassComp
import Data.ClassicalData
import Data.Basis
import Data.LazyNum
import Data.Matrix
import Data.List as List
import Data.Map as Map
import QSM.QuantumStack
import QSM.ClassicalStack
import QSM.Dump

\end{code}
%endif


The |ControlStack| is a higher order type, a function from 
a list of controlled quantum stacks to a list of 
controlled quantum stacks.  The types and data 
constructs needed are shown 
in \vref{fig:controlstack}.

\subsubsection{Representation details}

\begin{figure}[htbp]
\begin{singlespace}
\begin{code}
type ControlStack a b = 
     ([Controlled (QuantumStack a b)],[Controlled (QuantumStack a b)])
         -> ([Controlled (QuantumStack a b)],[Controlled (QuantumStack a b)])

instance Show (ControlStack a b) where
    show _ = "Funct"

data Controlled a
       = IdentityOnly a
       | LeftOnly a
       | RightOnly a
       | Full a
         deriving (Show, Eq)
\end{code}
\end{singlespace}
\caption{Definition of the control stack}\label{fig:controlstack}
\end{figure}

This module also defines a |Functor| for the 
|Controlled| data modifier as
all the functions defined on quantum stacks, with the 
exception of transformations, commute with |Controlled|.
|Transformations| require adjusting according to  the constructor of 
|Controlled| that is used.

{\begin{singlespace}
\begin{code}
instance Functor Controlled where
  fmap f (IdentityOnly a) = IdentityOnly (f a)
  fmap f (LeftOnly a) = LeftOnly (f a)
  fmap f (RightOnly a) = RightOnly (f a)
  fmap f (Full a) = Full (f a)

splitcontrolled :: Controlled a -> (a, b-> Controlled b)
splitcontrolled  (IdentityOnly a) = (a, IdentityOnly)
splitcontrolled  (LeftOnly a) = (a, LeftOnly)
splitcontrolled  (RightOnly a) = (a, RightOnly)
splitcontrolled  (Full a) = (a, Full)

unzipControl :: Controlled (a,b) -> (a, Controlled b)
unzipControl c = (fst a, f $ snd a)
                 where (a,f) = splitcontrolled c



\end{code}
\end{singlespace}}

\subsubsection{Adding and removing control.}
Because of the definition, the addition of new control points is
trivial, only requiring concatenating the |id| function to the
front of the list.

{\begin{singlespace}
\begin{code}
addControl :: [ControlStack a b] -> [ControlStack a b]
addControl  = (:) ctrlNoOp

ctrlNoOp :: ([a],[a]) -> ([a],[a])
ctrlNoOp ([],aas) = (aas,[])
ctrlNoOp (aas,[]) = (aas,[])
ctrlNoOp _ = error "Illegal control start"
\end{code}
\end{singlespace}}

Similarly, removing control becomes simple as well. Note that
here, this process  affects both the |ControlStack| and the |QuantumStack| of
the machine state. If there is no currently active control, this
is a "do-nothing" function rather than an error.

When there actually is a control point, it will be removed and
applied to the input |QuantumStack|. This new |QuantumStack| and the
remaining part of the list of |ControlStack|s are returned by the
function.

\begin{singlespace}
\begin{code}
removeAllControl :: (Quantum a b) => [ControlStack a b]  ->
                    [Controlled (QuantumStack a b)] ->
                    Controlled (QuantumStack a b)
removeAllControl [] cqs = head cqs
removeAllControl fs cq = (uncurry removeAllControl) $ removeControl fs cq

removeControl :: (Quantum a b) => [ControlStack a b]  ->
                 [Controlled (QuantumStack a b)] ->
                 ([ControlStack a b], [Controlled (QuantumStack a b)])
removeControl [] cq = ([], cq)
removeControl (f:fs) cq = (fs, fst $ f ([],cq))

unControl ::  (Quantum a b) => [ControlStack a b]  ->
              [ ((Int, ClassicalStack), 
                (Controlled(QuantumStack a b), Dump a b))] ->
              ([ControlStack a b],
               [ ((Int,ClassicalStack), 
                 (Controlled(QuantumStack a b), Dump a b))])
unControl [] lis = ([],lis)
unControl fs lis = (fs',newlis)
    where (ncss,cqds) = unzip lis
          (cqs, ds) = unzip cqds
          (fs',cqs') = removeControl fs cqs
          cqds' = zip cqs' ds
          newlis = zip ncss cqds'
\end{code}
\end{singlespace}

\subsubsection{Moving items to the |ControlStack|}
This is where the actual coding
work is required. A function is created that
will recreate the quantum stack with the current top element and
pass back the new list of |Controlled QuantumStack|s.

This is broken down into simpler pieces. The first delegation is to
 a function called |withControl| which operates on a list
of |ControlStack| items and list of |Controlled| |QuantumStack| items,
returning a pair of those lists. The |withControl| function also 
delegates
to the function |makeControl| which creates the uncontrol function
and the new list when given the list of quantum stacks. |withControl|
 then combines that function with the current one at the head of the
control stack and returns the elements.

\begin{singlespace}
\begin{code}
qControl ::  (Quantum a b) => [ControlStack a b]  ->
              [ ((Int,ClassicalStack), 
                 (Controlled(QuantumStack a b), Dump a b))] ->
              ([ControlStack a b],
               [ ((Int,ClassicalStack), 
                  (Controlled(QuantumStack a b), Dump a b))])
qControl [] lis = ([],lis)
qControl fs lis = (fs',newlis)
    where (ncss,cqds) = unzip lis
          (cqs, ds) = unzip cqds
          (fs',cqs') = withControl fs cqs
          cqds' = zip cqs' (cycle ds)
          newlis = zip (cycle ncss) cqds'

withControl :: (Quantum a b) => [ControlStack a b] ->
               [Controlled (QuantumStack a b)] ->
               ([ControlStack a b], [Controlled (QuantumStack a b)])
withControl (f:fs) qs = 
    ((f . g):fs, ctrlldqs)
        where (g, ctrlldqs) = makeControl qs
\end{code}
\end{singlespace}

The next function, |makeControl| delegates to the function |makeControl'|
with an accumulator
to create the control stack function. This function recurses,
pasting intermediate results to the accumulator and delegating
the work of creating a function and list of an individual quantum
stack to |mc|. 

\begin{singlespace}
\begin{code}
makeControl :: (Quantum a b) => 
               [Controlled (QuantumStack a b)] ->
               (ControlStack a b, [Controlled (QuantumStack a b)])
makeControl = makeControl' (id,[])

makeControl' :: (Quantum a b) => 
               (ControlStack a b, [Controlled (QuantumStack a b)]) ->
               [Controlled (QuantumStack a b)] ->
               (ControlStack a b, [Controlled (QuantumStack a b)])
makeControl' (cs', qs') [] = (cs', qs')
makeControl' (cs', qs') (q:qs)
             = makeControl' (f . cs', qs' ++ cq) qs
               where (f, cq) = mc q
\end{code}
\end{singlespace}

Our base functions, |mc| and |mc'|  operate over the various types of 
the quantum stack. For simple nodes, it moves that node off,
creates a list of the sub-nodes and returns a function that 
would paste that back onto the quantum stack. For
data nodes, it also removes all the dependent children named
in the constructors.

\begin{singlespace}
\begin{code}
mc :: (Quantum a b) => Controlled (QuantumStack a b)->
      (ControlStack a b, [Controlled (QuantumStack a b)])
mc (IdentityOnly q@(StackQbit _ _)) 
      = (f, List.map idonlytr qs)
        where (f, qs) = mc' IdentityOnly q
mc (LeftOnly q@(StackQbit _ _)) 
      = (f, List.map leftonlytr qs)
        where (f, qs) = mc' LeftOnly q
mc (RightOnly q@(StackQbit _ _)) 
      = (f, List.map rightonlytr qs)
        where (f, qs) = mc' RightOnly q
mc (Full q@(StackQbit _ _)) 
      = (f,  qs)
        where (f, qs) = mc' Full q

mc (IdentityOnly q) = mc'  IdentityOnly q
mc (LeftOnly q) = mc'  LeftOnly q
mc (RightOnly q) = mc'  RightOnly q
mc (Full q) = mc' Full q
\end{code}
\end{singlespace}

|mc'| returns a function that "pastes" back the controlled
data node to the sub-stacks of the node. It memoizes the 
various pieces needed to do this including the |StackAddress| 
for all types, the keys of the data values for all types and
for constructors, the list of dependent nodes.

For the |StackQbit| nodes, the returned quantum stacks are controlled
as per their key placement as a sub-stack. The return function resets
the control to its previous value.

\begin{singlespace}
\begin{code}
mc' :: (Quantum a b) => 
        (QuantumStack a b -> Controlled (QuantumStack a b)) ->
        QuantumStack a b->
        (ControlStack a b, [Controlled (QuantumStack a b)])
mc' ctl (StackQbit nm qvs)
    = (f, List.map (uncurry controlIt) ascl) 
      where ascl = Map.toAscList qvs
            (keys, vals) = unzip ascl
            f = \ (acc, ctlqvls) -> 
                (acc ++ 
                 [ctl $ StackQbit nm $ qv $  zip keys $ 
                  List.map uncontrolIt $ ctlqvls], 
                 drop (length keys) ctlqvls)

\end{code}
\end{singlespace}

For the |StackInt| nodes, the returned quantum stacks are controlled
as per the control type passed in. Controlling by an int value
does nothing to quantum transformations, therefore it must remain
the same as the parent was. Similarly, when uncontrolling, the function
sets the control value of the new combined |StackInt| to what it was
previously.

\begin{singlespace}
\begin{code}
mc' ctl (StackInt nm cvs)
    = (f, List.map ctl vals)
      where ascl = Map.toAscList cvs
            (keys, vals) = unzip ascl
            f = \ (acc, ctlqvls) -> 
                (acc ++ 
                 [ctl $ StackInt nm $ cv $ zip keys $ 
                  List.map uncontrolIt $  ctlqvls], 
                 drop (length keys) ctlqvls)
\end{code}
\end{singlespace}


For the |StackCons| nodes, the direct processing is similar to that
of |StackInt|, allowing for the list of dependent nodes. 

The main difference is that controlling by a data type controls by 
\emph{all} of the elements of that data type. For example,
 controlling
by a |List (Qbit)| with 3 \qbit{}s in it will control by
 all of those \qbit{}s. This is achieved by calling the 
function |controlAll| defined below.

\begin{singlespace}
\begin{code}
mc' ctl (StackCons nm dvls)
   = controlAll (subnms) (f, List.map (ctl . snd ) vals)
      where ascl = Map.toAscList dvls
            (keys, vals) = unzip ascl
            subnameList = List.map fst vals
            subnms = concat $ subnameList            
            f = \ (acc, qvls) -> 
                (acc ++ 
                 [ctl $ StackCons nm $ dv $ zip  keys $ 
                  zip subnameList $ List.map uncontrolIt $ qvls], 
                 drop (length keys) qvls)
\end{code}
\end{singlespace}

The |StackZero| nodes are simply propagated as a placeholder.

\begin{singlespace}
\begin{code}
mc' ctl StackZero
    = (f, [ctl StackZero])
      where f = \ (acc, qvls) -> 
                (acc ++ [ctl $ StackZero ],
                 tail  qvls)
\end{code}
\end{singlespace}

The |controlAll| function is used when controlling |StackCons| elements.
For each name passed to the function, it rotates that up in the quantum stack
and then adds it to the current control.

\begin{singlespace}
\begin{code}
controlAll :: (Quantum a b) => [StackAddress] -> 
              (ControlStack a b, [Controlled (QuantumStack a b)])  ->
             (ControlStack a b, [Controlled (QuantumStack a b)]) 
controlAll [] (f,qs) = (f,qs)
controlAll (nm:nms) (f,qs)
    = (g, newqs)
    where qs' = List.map (fmap (rotateup nm)) qs
          (f',newqs) = withControl [f] qs'
          g = head f'

\end{code}
\end{singlespace}

As an example, consider 
applying a $controlled-T$ transform to the state Q, comprised of two 
\qbit{}s. This gives:
\begin{equation}
\begin{bmatrix}I&0\\0&T\end{bmatrix}
\begin{bmatrix}Q_{00}&Q_{01}\\Q_{10}&Q_{11}\end{bmatrix}
\begin{bmatrix}I&0\\0&T^*\end{bmatrix}
 =
\begin{bmatrix}Q_{00}&Q_{01} T^*\\T Q_{10}&T Q_{11} T^*\end{bmatrix}
\end{equation}
From this, a general  pattern to implement when controlling by a
\qbit{} becomes obvious. This is implemented by the function |controlIt|.

\begin{singlespace}
\begin{code}
controlIt :: (Basis a) => (a,a)-> b->
             Controlled b
controlIt (a,b)
          | a == hb && b == hb = IdentityOnly
          | a == hb && b /= hb = LeftOnly
          | a /= hb && b == hb = RightOnly
          | otherwise = Full
          where hb = head basis

uncontrolIt :: Controlled a -> a
uncontrolIt (IdentityOnly a) = a
uncontrolIt (LeftOnly a) = a
uncontrolIt (RightOnly a) = a
uncontrolIt (Full a) = a 


idonlytr :: Controlled a -> Controlled a
leftonlytr :: Controlled a -> Controlled a
rightonlytr :: Controlled a -> Controlled a
idonlytr (IdentityOnly a) = IdentityOnly a
idonlytr (LeftOnly a) = IdentityOnly a
idonlytr (RightOnly a) = IdentityOnly a
idonlytr (Full a) = IdentityOnly a

leftonlytr (IdentityOnly a) = IdentityOnly a
leftonlytr (LeftOnly a) = LeftOnly a
leftonlytr (RightOnly a) = IdentityOnly a
leftonlytr (Full a) = LeftOnly a

rightonlytr (IdentityOnly a) = IdentityOnly a
rightonlytr (LeftOnly a) = IdentityOnly a
rightonlytr (RightOnly a) = RightOnly a
rightonlytr (Full a) = RightOnly a


\end{code}
\end{singlespace}


