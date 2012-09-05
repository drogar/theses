%include polycode.fmt
%format ^* = "\ltimes"
%format *^ = "\rtimes"

\subsection{Description of the quantum stack}\label{subsec:quantumstackdescription}

%if false
\begin{code}

module QSM.QuantumStack where
import Data.ClassComp
import Data.ClassicalData
import Data.Basis
import Data.LazyNum
import Data.Map as Map
import Data.List as List

\end{code}
%endif

The quantum stack holds both \emph{quantum} and \emph{probabilistic}
data in the quantum stack  machine.

\emph{Quantum} data consists of \qbit{}s. These are stored in the
quantum stack isomorphically to their density matrix notation. 
Assuming a basis of \ket{0} and \ket{1}, four entries are used
to store a \qbit. This is the same as the density matrix for the 
\qbit{} $\alpha\ket{0} + \beta\ket{1}$ which is 
{\begin{singlespace}
$\begin{pmatrix}
\alpha\bar{\alpha}& \alpha\bar{\beta}\\
\beta\bar{\alpha}& \beta\bar{\beta}
\end{pmatrix}$\end{singlespace}}.

\emph{Probabilistic} data in the stack may be an \Int,
\Bool{} or \Datatype. 
These are represented as multi-branched trees, with one branch per
integer or logical value or data constructor. This is  isomorphic to
a tuple of branches.

The leaves of the tree are single valued numeric values. These values
represent the actual value of the density matrix.

Finally, when all the leaves below a branch are $0$, a 
special representation is used to effect sparseness.

Our  code is written assuming a generic |Basis| which
may have more than two elements. For example if a |Basis| had three
elements,  a 
\qbit{} representation would require six values. (A $3\times 3$ matrix
with the below diagonal elements not stored).

\subsubsection{Representation details}

All nodes of the stack follow a common representation pattern of
\[(\mathrm{nodename}, [\mathrm{\texttt{cons}}\to\mathrm{sub-stack}]).\]
The sub-stack in the case of \Datatype{}s also contains the list
of variables lower in the stack which are \emph{bound} to that
constructor.

\begin{singlespace}
\begin{code}

type StackAddress = String

type Constructor = String

type CV a b = Map (ClassicalData) (QuantumStack a b)

type QV a b = Map (a,a) (QuantumStack a b)

type DV a b = Map Constructor ([StackAddress], (QuantumStack a b) )

class (Basis a, Show a, Comp b) => Quantum a b
\end{code}
\end{singlespace}

For ease of use,  a variety of creator and accessor functions are defined.
The creator functions are named the same
as the type they create,  but in lower case. 
%if false
\begin{code}


qv :: (Quantum a b) => 
      [((a,a),QuantumStack a b)] -> 
      QV a b
qv = fromList

cv :: (Quantum a b) => 
      [(ClassicalData,QuantumStack a b)] -> 
      CV a b
cv = fromList

dv :: (Quantum a b) => 
      [(Constructor,([StackAddress],QuantumStack a b))] -> 
      DV a b
dv = fromList
\end{code}
%endif
The functions |val| and |valMaybe| are accessor functions that will return a 
quantum substack given a substack of a |QV| element.
%if false
\begin{code}
val :: (Quantum a b) => 
       (a,a) -> 
       QV a b  -> 
       QuantumStack a b
val (a,b) mp 
        = findWithDefault (fromInteger 0) (a,b) mp

valmaybe :: (Quantum a b) => 
            (a,a) -> 
            QV a b  -> 
            Maybe (QuantumStack a b)
valmaybe =  Map.lookup


\end{code}
%endif

The quantum stack is a tree with a variable number of branches at
each level and data at the leaves. Data is always added to the top
of the stack. Manipulations of data at the top are performed by 
applying the proper linear combinations to the sub-stacks. (For more 
detail see |apply|).

There are  five 
different constructors for |QuantumStack| elements:
\begin{description}
\item{|StackZero|} Explicit laziness used when adding \bits{} or \qbits{} to the
stack. In those cases, one or more of the sub stacks has all zero
entries at the leaves. Rather than computing this, it is  simply tagged 
as such.
\item{|StackData a|} The leaves of the stack where the entries of the
series of density matrices are stored. This also serves as a starting
point for a stack, with the data being the value $1$.
\item{|StackBit name (CV a)|} A named 
\bit{} entry in the stack, with the sub-stacks
for each basis element.
\item{|StackQbit name (QV a)|} A named \qbit{} entry, with the sub-stacks
 corresponding to the product of the the basis elements. 
\end{description}
{\begin{figure}[htbp]
\begin{singlespace}
\begin{code}

data  (Quantum a b) =>QuantumStack a b
    = StackZero  |
      StackData b |
      StackInt StackAddress (CV a b) |
      StackQbit StackAddress (QV a b) |
      StackCons StackAddress (DV a b)

\end{code}
\end{singlespace}
\caption{Haskell definition of the quantum stack}\label{fig:haskellDefinitionOfQstack} 
\end{figure}}
We provide definitions of instances for the  predefined Haskell classes 
|Eq|, |Show|, |Num| and |Comp| 
for |QuantumStack a b|. These definitions 
depend upon the |b| type being a
member of |Show| and |Comp|.  The actual definitions are elided for 
compactness.

\subsubsection{Quantum Stack support functions}\label{subsubsec:quantumstacksupportfunctions}
This section contains a number of support functions used throughout
the machine. 

The function |breakQuantum| 
is used when processing \emph{quantum control}
instructions (see \vref{para:measurmentdeconstruction}). Each of those
types of instructions requires accessing the sub-branches of the tree
individually.

{
\begin{singlespace}
\begin{code}

breakQuantum :: (Quantum a b) => QuantumStack a b -> 
               [QuantumStack a b]
breakQuantum (StackInt sa cvals) 
    = let citems = Map.toList cvals
      in List.map ((StackInt sa) . cv . (:[])) citems
breakQuantum (StackQbit sa qvals) 
    = let qitems = Map.toList qvals
      in List.map ((StackQbit sa) . qv . (:[])) qitems
breakQuantum (StackCons sa dvals) 
    = let ditems = Map.toList dvals
      in List.map ((StackCons sa) . dv . (:[])) ditems
breakQuantum q = [q]

\end{code}
\end{singlespace}
}

%if codeOnly || showClassDerivations
\begin{code}

instance (Quantum a b) =>  Eq (QuantumStack a b) where
    (==) StackZero StackZero = True
    (==) StackZero (StackData b) = (b == fromInteger 0)
    (==) (StackData b) StackZero = (b == fromInteger 0)
    (==) (StackData b) (StackData b') = (b == b')
    (==) (StackInt s mp) (StackInt s' mp') 
	= (s==s') && (isSubmapOf mp mp') && (isSubmapOf mp' mp)
    (==) (StackQbit s mp) (StackQbit s' mp') 
	= (s==s') && (isSubmapOf mp mp') && (isSubmapOf mp' mp)
    (==) (StackCons nm mp)  (StackCons nm' mp')
         = (nm == nm') && 
           (isSubmapOf mp mp') && (isSubmapOf mp' mp)
    (==) _ _ = False

instance (Quantum a b) => Show (QuantumStack a b) where
    show StackZero = "0"
    show (StackData d) = show d
    show (StackInt s mp) = "<I- "++s++": "++(show mp)++" ->"
    show (StackQbit s mp) = "<Q- "++s++": "++(show mp)++" ->"
    show (StackCons nm mp) = "<C- "++nm++"("++(show mp)++") ->"

dtypePlus :: (Quantum a b) => 
             ([StackAddress], (QuantumStack a b)) ->
            ([StackAddress], (QuantumStack a b)) ->
            ([StackAddress], (QuantumStack a b))
dtypePlus (nms1, stk1) (nms2, stk2) 
          | List.foldr (&&) True $ 
            List.zipWith (==) nms1 nms2 
                = (nms1 , stk1 + stk2)
          | otherwise = error "dtypePlus: Undefined stack add"

instance (Quantum a b) => Num (QuantumStack a b) where
    (+) StackZero a = trimStack a
    (+) a StackZero = trimStack a
    (+) (StackData a) (StackData b) = trimStack (StackData (a+b))
    (+) (StackInt s1 cv1) (StackInt s2 cv2) 
	| s1 == s2 = trimStack ( StackInt s1 (unionWith (+) cv1 cv2))
    (+) (StackQbit s1 qv1) (StackQbit s2 qv2)
	| s1 == s2 = trimStack (StackQbit s1 (unionWith (+) qv1 qv2))
    (+) (StackCons s1 dv1) (StackCons s2 dv2)
	| s1 == s2 = trimStack (StackCons s1 (unionWith (dtypePlus) dv1 dv2))
    (+) stk1 stk2 =
          let nms1 = theName stk1
	      stk2' = rotateup nms1 stk2
          in if (nms1 == theName stk2') then stk1 + stk2' 
	     else error "Undefined stack add"
    
    (*) _ _ = error "Multiplcation of two stacks is not valid"

    negate StackZero = StackZero
    negate (StackData a) = StackData (negate a)
    negate (StackInt s cv) = StackInt s (Map.map (negate) cv)
    negate (StackQbit s qv) = StackQbit s (Map.map (negate) qv)
    negate (StackCons s dv) = StackCons s (Map.map (tosecond negate) dv)
    
    abs StackZero = StackZero
    abs (StackData a) = StackData (abs a)
    abs (StackInt s cv) = StackInt s (Map.map (abs) cv)
    abs (StackQbit s qv) = StackQbit s (Map.map (abs) qv)
    abs (StackCons s dv) = StackCons s (Map.map (tosecond abs) dv)

    signum StackZero = StackZero
    signum (StackData a) =  StackData (signum a)
    signum (StackInt s cv) = StackInt s (Map.map (signum) cv)
    signum (StackQbit s qv) = StackQbit s (Map.map (signum) qv)
    signum (StackCons s dv) = StackCons s (Map.map (tosecond signum) dv)

    fromInteger 0 = StackZero
    fromInteger n = StackData (fromInteger n)

\end{code}
%endif

All the value data is stored at the leaves of the quantum stack, 
while operations are defined on the actual stacks. 
The operations, such as setting a value, 
unitary transformations
etc., create linear combinations of the current sub-stacks as 
new sub-stacks. 
A left scalar multiple function |^*| and
a corresponding right scalar multiple function |*^| are defined to support
this. Note that  
scalar multiplication 
creates a |StackZero| if the 
scalar is $0$. 

{
\begin{singlespace}
\begin{code}

(^*) ::(Quantum a b)=>  b-> QuantumStack a b -> 
       QuantumStack a b
b ^* _ | b == fromInteger 0 = StackZero
b ^* StackZero = StackZero
b ^* (StackData c) = StackData (b*c)
b ^* s = descendMap (b ^*) s


(*^) ::(Quantum a b)=> QuantumStack a b ->    b->
       QuantumStack a b
x *^ y = y ^* x

\end{code}
\end{singlespace}
}

The auxiliary function |trimStack| is used to 
reduce full stacks of
zero values to |StackZero|, which is then used by the |Num.+| function.
%if codeOnly || showSupportFunctionDetail
\begin{code}

trimStack ::(Quantum a b)=> 
            QuantumStack a b->
            QuantumStack a b
trimStack StackZero = StackZero
trimStack (StackData b) 
	  | b == fromInteger 0 = StackZero
	  | otherwise = StackData b
trimStack (StackInt s cv) 
	  = let cv' = Map.map trimStack cv
		cond = Map.fold (\ stk b -> b && stk == StackZero) True cv'
		cv'' = Map.filter eraseZeroes cv'
            in if cond then StackZero else (StackInt s cv'')
trimStack (StackQbit s qv) 
	  = let qv' = Map.map trimStack qv
		cond = Map.fold (\ stk b -> b && stk == StackZero) True qv'
		qv'' = Map.filter eraseZeroes qv'
            in if cond then StackZero else (StackQbit s qv'')
trimStack (StackCons s dv) 
	  = let dv' = Map.map (tosecond trimStack) dv
		cond = Map.fold (\ stk b -> b && (snd stk) == StackZero) True dv'
		dv'' = Map.filter ( eraseZeroes . snd) dv'
            in if cond then StackZero else (StackCons s dv'')
\end{code}
%endif
The accessor function |qvalues| retrieves the |QV| sub element of
a |QuantumStack|.
%if codeOnly || showSupportFunctionDetail
\begin{code}



qvalues ::(Quantum a b) => 
          QuantumStack a b -> 
          QV a b
qvalues (StackQbit _ qvls) 
   = Map.union congqvals qvls
     where congqvals = Map.fromList gtvals
           gtvals = [((a,b), (val (a,b) qvls)) | a<- basis, b<-basis, a > b]
qvalues StackZero = Map.empty
qvalues _ = error "qvalues: No qvals for element that is not quantum"

\end{code}
%endif
The |descendMap| function works like a functor, applying a |QuantumStack|
endomorphism to the sub-stacks.
%if codeOnly || showSupportFunctionDetail
\begin{code}


descendMap :: (Quantum a b) =>
              ( QuantumStack a b -> QuantumStack a b) ->
              QuantumStack a b ->
              QuantumStack a b 
descendMap f StackZero 
    = f StackZero
descendMap f (StackData d) 
    = f (StackData d)
descendMap f (StackInt s cvals) 
    = StackInt s (Map.map f cvals)
descendMap f (StackQbit s qvals) 
    = StackQbit s (Map.map f qvals)
descendMap f (StackCons s dvals) 
    = StackCons s (Map.map (tosecond f) dvals)


\end{code}
%endif
The function |rotateup| works in conjunction with the |pullup| and |descendMap|
functions to rotate a \bit{} or \qbit{}
 to the top of the stack. The first argument
is the name (address) of the \qbit{}
 to bring up. The second is the |QuantumStack| to 
rotate.

The function works by doing a recursive check and descent. There are three
options:
\begin{description}
\item[Already at top] --- Just return the current stack.
\item[Second item on quantum stack] --- Delegate  to |pullup|.
\item[Somewhere else] --- Use |descendMap| to rotate the desired (q)bit to the
top of the sub-stacks, then apply |pullup|.
\end{description}

{
\begin{singlespace}
\begin{code}

rotateInOrder :: (Quantum a b) => [String] -> QuantumStack a b -> 
                 QuantumStack a b
rotateInOrder [] q = q
rotateInOrder (nm:nms) q = rotateInOrder nms $ rotateup nm q

rotateup :: (Quantum a b) => String -> QuantumStack a b -> 
            QuantumStack a b
rotateup _ StackZero = StackZero
rotateup _ (StackData b) = StackData b
rotateup s stack
	 | s == (theName stack) 
             = stack
	 | s == (secondName stack) 
             = trimStack $ 
               pullup s stack
	 | otherwise 
             = trimStack $ 
               pullup s $ 
               descendMap (rotateup s) stack
\end{code}
\end{singlespace}}

The function |pullup| handles the case of the 
search address being either on top or the 
second element of the |QuantumStack|. As with |rotateup|, if 
the target node is already 
on top,
no change is made. In the case where the target node
 is the next element on the stack,
an exchange is done to bring up that stack. This is achieved by 
building a new stack with the second element at 
the top and the first element 
at the first set of sub-stacks. The sub-stacks (up to 16 in the case of 
two qbits) are re-ordered so that their
"tree path" remains the same. 
The "tree path" can be thought of as the path 
needed to descend the sub-stacks.

%if codeOnly || showSupportFunctionDetail
\begin{code}

pullup :: (Quantum a b) => 
          String -> 
          QuantumStack a b -> 
          QuantumStack a b
pullup s StackZero = StackZero

pullup s st@(StackInt str cvals)
       | s == str = st
       | s == (secondName st) 
	   = case (snd $ Map.elemAt 0 cvals) of
	     StackInt _ _ -> 
		 (StackInt s (cv [(innerk, StackInt  str (cv [(k, subIntStack innerk $
                                                          subIntStack k st) |
							 k<- getIntCons st])) |
				  innerk<- getSecondLvlIntCons st])) 
	     StackQbit _ _ -> 
		 (StackQbit s (qv [((a,c), StackInt  str (cv [(b, subQbitStack (a,c) $
                                                                    subIntStack b st) |
							      b<- getIntCons st])) |
				   a<- basis, c<- basis]))
             StackCons _ dvals ->  -- todo - how to prevent dependent roll ups
		 (StackCons s (dv [(cons, (getSecondLevelConsAddresses cons st,
                                           StackInt  str (cv [(b, subConsStack cons $ 
                                                                subIntStack b  st) |
							   b<- getIntCons st]))) |
				       cons <- getSecondLvlConstructors st]))
             _ -> error "pullup: Second level of StackInt is DATA or ZERO"
       | otherwise = st

pullup s st@(StackQbit str qvals)
       | s == str = st
       | s == (secondName st) 
	   = case (snd $ Map.elemAt 0 qvals) of
	     StackInt _ _ -> 
                 (StackInt s (cv [(a, StackQbit  str 
                                       (qv [((b,c), subIntStack a $
                                                  subQbitStack (b,c) st) |
                                             b<- basis, c<- basis])) | 
				  a<- getSecondLvlIntCons st])) 
	     StackQbit _ _ -> 
	         (StackQbit s (qv [((a,b), 
                                    StackQbit  
                                    str (qv [((c,d), subQbitStack (a,b) $
                                                       subQbitStack (c,d) st) | 
					     c<-basis, d<- basis])) | 
				   a<- basis, b<- basis])) 
             StackCons _ dvals ->  
		 (StackCons s (Map.fromList 
                                      [(cons, (getSecondLevelConsAddresses cons st,
                                        StackQbit  str (qv [((a,b), subConsStack cons $ 
                                                                 subStack [a,b] st) |
							   a<-basis, b<- basis]))) |
				       cons <- getSecondLvlConstructors st]))
             _ -> error "pullup: Second level of StackQBit is DATA or ZERO"
       | otherwise = st

pullup s st@(StackCons str dvals)
       | s == str = st
       | s == (secondName st) 
	   = case (snd $ snd $ Map.elemAt 0 dvals) of
	     StackInt _ _ -> 
                 (StackInt s (cv [(a, StackCons  str 
                                       (Map.fromList [(cons, (getConsAddresses cons st ,
                                                              subIntStack a  $
                                                              subConsStack cons st)) |
                                             cons <- getConstructors st])) | 
				  a<- getSecondLvlIntCons st])) 
	     StackQbit _ _ -> 
	         (StackQbit s (qv [((a,b), StackCons str
                                             (Map.fromList [(cons, (getConsAddresses cons st ,
                                                                    subQbitStack (a,b) $
                                                                    subConsStack cons st)) | 
                                             cons <- getConstructors st])) | 
				   a<- basis, b<- basis])) 
             StackCons _ innerdvals -> 
		 (StackCons s (dv
                               [(innercons, (getSecondLevelConsAddresses innercons st ,
                                             StackCons  str 
                                             (Map.fromList [(cons, (getConsAddresses cons st,
                                                                    subConsStack innercons $
                                                                    subConsStack cons st)) | 
                                                            cons <- getConstructors st]))) |
				innercons <-getSecondLvlConstructors st]))
             _ -> error "pullup: Second level of StackCons is DATA or ZERO"

       | otherwise = st

pullup _ s@(StackData _)
       = error "pullup: StackData elements may not be pulled up"

\end{code}
%endif
%if codeOnly || showMinorFunctions
\begin{code}

trace  :: (Quantum a b) => 
          QuantumStack a b -> 
          b
trace  StackZero = fromInteger 0
trace  (StackData b) = b
trace  (StackInt _ cvals)
       = fold (+) (fromInteger 0) $ 
         Map.map trace cvals
trace (StackQbit _ qvals)
       = fold (+) (fromInteger 0) $ 
         Map.map trace $ 
         Map.filterWithKey (\(a,b) _ -> a == b) qvals
trace (StackCons _ dvals)
       = fold (+) (fromInteger 0) $ 
         Map.map (trace . snd ) dvals


rename ::  (Quantum a b) =>
           (StackAddress->StackAddress) -> 
           QuantumStack a b -> 
           QuantumStack a b
rename _ StackZero = StackZero
rename _ qs@(StackData _) = qs
rename r qs = setName (r $ theName qs) qs

firstRenameInStack ::  (Quantum a b) => 
                       StackAddress->
                       StackAddress -> 
                       QuantumStack a b -> 
                       QuantumStack a b
firstRenameInStack _ _ StackZero = StackZero
firstRenameInStack _ _ qs@(StackData _) = qs
firstRenameInStack old new qs
    | old == (theName qs) 
        = setName new qs
    | otherwise 
        = descendMap (firstRenameInStack old new) qs



discard :: (Quantum a b) => 
           QuantumStack a b -> 
           QuantumStack a b
discard stk =
    case stk of 
	StackInt _ cvals -> 
	  fold (+) StackZero cvals
	StackQbit _ qvals -> 
	  fold (+) StackZero 
		(qvals Map.\\ ( Map.fromList (offdiag basis)))
        StackCons _ dvals ->
	  let (nms, dvs) = unzip $ Map.elems dvals
              discardEach = List.map ((foldl (.) id) . 
                                      (List.map (\ x -> discard . (pullup x)))) nms
              dvls = zipWith ($) discardEach dvs
          in foldl (+) StackZero dvls
        _ -> stk
   
\end{code}
%endif
%if codeOnly || showMinorFunctions
\begin{code}


tosecond :: (b->c) -> (a,b) -> (a,c)
tosecond f (a,b) = (a, f b)

branchCount :: (Quantum a b) => 
               QuantumStack a b -> 
               Int
branchCount = length . breakQuantum


topVal :: (Quantum a b) => 
          QuantumStack a b -> 
          Maybe (ClassicalData)
topVal (StackInt _ cvals) = Just $ head $ Map.keys cvals
topVal _ = Nothing




cjgt :: (Quantum a b) => 
        QuantumStack a b -> 
        QuantumStack a b
cjgt StackZero = StackZero
cjgt (StackData b) = StackData $ conjgt b
cjgt (StackInt s cv) = StackInt s (Map.map cjgt cv)
cjgt (StackQbit s qv) = StackQbit s (Map.map cjgt qv)
cjgt (StackCons s dv) 
    = StackCons s (Map.map (tosecond cjgt) dv)


eraseZeroes :: QuantumStack a b -> Bool
eraseZeroes StackZero = False
eraseZeroes qs = True




theName  :: (Quantum a b) => QuantumStack a b -> String
theName  (StackInt s _) = s
theName  (StackQbit s _) = s
theName  (StackCons s _) = s
theName  _ = ""




setName  :: (Quantum a b) => 
            String ->
            QuantumStack a b ->
            QuantumStack a b 
setName  s (StackInt _ cv) = (StackInt s cv)
setName  s (StackQbit _ qv) = (StackQbit s qv)
setName  s (StackCons _ dv) = (StackCons s dv)
setName  _ sd = sd

secondName :: (Quantum a b) => QuantumStack a b -> String
secondName (StackInt _ cvals)
	   | Map.null cvals  = ""
	   | otherwise = theName . snd $  Map.elemAt 0 cvals
secondName (StackQbit _ qvals)
	   | Map.null qvals  = ""
	   | otherwise = theName . snd $  Map.elemAt 0 qvals
secondName (StackCons _ dvals)
	   | Map.null dvals  = ""
	   | otherwise = theName . snd . snd $  Map.elemAt 0 dvals
secondName _ = ""



depth :: (Quantum a b)=> QuantumStack a b -> Int
depth StackZero = 1
depth (StackData b) = 1
depth (StackInt _ cvls) = 1 + (Map.fold max 0 $ Map.map depth  cvls)
depth (StackQbit _ qvls) = 1 + (Map.fold max 0  $ Map.map depth  qvls)
depth (StackCons _ dvls) = 1 + (Map.fold max 0  $  Map.map (depth . snd)  dvls)

names ::  (Quantum a b) => Int -> QuantumStack a b->[String]
names 1 qb = [theName qb]
names n (StackInt nm bvals)  =
    nm: names (n-1) (subTreeWithDepth (n-1) $ elems bvals)

names n (StackQbit nm qvals)  =
    nm: names (n-1) (subTreeWithDepth (n-1) $ elems qvals)

names n (StackCons nm dvals)  =
    nm: names (n-1) (subTreeWithDepth (n-1) $ List.map snd $ elems dvals)

names _ _ = error "Can not get further names from Zero or leaves"




subTreeWithDepth ::  (Quantum a b) => Int -> 
                     [QuantumStack a b] -> QuantumStack a b
subTreeWithDepth n qstks 
    = head deepStacks
      where deepStacks = List.filter (\a -> n <= depth a) qstks
\end{code}
%endif

|subStack| allows us to choose a particular sub stack via the "tree path". 
It is given a list of basis elements, which are used to descend the 
|QuantumStack| until the list is exhausted. Two elements of the list are
used
for each \qbit.
%if codeOnly || showMinorFunctions
\begin{code}

subStack :: (Quantum a b) => [a] -> QuantumStack a b -> QuantumStack a b
subStack _ StackZero = StackZero
subStack [] s = s
subStack (basel:baser:bases) (StackQbit _ qval)
         = subStack bases $ val (basel, baser) qval
subStack _ _ = error "Looking for substacks in all the wrong places"

subIntStack :: (Quantum a b) => ClassicalData -> 
               QuantumStack a b -> QuantumStack a b
subIntStack _ StackZero = StackZero
subIntStack base (StackInt _ cval)
         = findWithDefault (fromInteger 0) base cval
subIntStack _ _ = error "Looking for subBitstacks in all the wrong places"

subQbitStack :: (Quantum a b) => (a,a) -> 
                QuantumStack a b -> QuantumStack a b
subQbitStack _ StackZero = StackZero
subQbitStack base (StackQbit _ qval)
         =  val base qval
subQbitStack _ _ = error "Looking for subQbitstacks in all the wrong places"



subConsStack :: (Quantum a b) => Constructor -> QuantumStack a b -> QuantumStack a b
subConsStack _ StackZero = StackZero
subConsStack cons (StackCons _ mp)
    = snd $ findWithDefault ([],StackZero) cons mp
subConsStack _ _ = error "Looking for constructor substacks in all the wrong places"

getSecondLvlStacks  :: (Quantum a b) => QuantumStack a b -> [QuantumStack a b]
getSecondLvlStacks  (StackData _) = []
getSecondLvlStacks  StackZero = []
getSecondLvlStacks  (StackInt _ cvals)
                    = Map.elems cvals
getSecondLvlStacks  (StackQbit _ qvals)
                    = Map.elems qvals
getSecondLvlStacks  (StackCons _ dvals)
                    = List.map snd $ Map.elems dvals


getIntCons   ::  Quantum a b => QuantumStack a b -> [ClassicalData]
getIntCons   (StackInt _ cvals) 
    = keys cvals
getIntCons  _ = []




getSecondLvlIntCons  :: Quantum a b => QuantumStack a b -> 
                        [ClassicalData]
getSecondLvlIntCons  
  = concat . (List.map getIntCons) . getSecondLvlStacks

getConstructors   ::  Quantum a b => QuantumStack a b -> [Constructor]
getConstructors   (StackCons _ dvals) 
    = keys dvals
getConstructors  _ = []




getSecondLvlConstructors  :: Quantum a b => QuantumStack a b -> [Constructor]
getSecondLvlConstructors  
  = concat . (List.map getConstructors) . getSecondLvlStacks

getConsAddresses   :: (Quantum a b) => Constructor -> 
                      QuantumStack a b -> [StackAddress]
getConsAddresses   cons (StackCons _ dvals) 
    = fst $ findWithDefault ([], StackZero) cons dvals
getConsAddresses cons _ = []
\end{code}
%endif
%TODO The second level one is interesting in that there may be 
%different addressess bound under the same constructor. I'm not sure what 
%is the correct thing to do in that case. Is it as simple as just renaming 
%the conflicting 
%ones. Is there a deterministic way of doing that? 

%Or does that imply a different structure is really required, i.e. a map from 
% (|Constructor|, [StackAddress]) pairs to the sub stacks. My gut likes the second 
%better. i.e., you can have a list that is Nil OR Cons ('1', Nil) OR
%Cons ('0', (Cons '1', Nil)). I'll continue for now with out changing that.
%
%For example how would you rename to Nil and Cons ('1', Nil) being the same thing - 
%doesn't make sense to me.

%For now, Take head of addresses
{
\begin{singlespace}
\begin{code}


getSecondLevelConsAddresses :: (Quantum a b) => Constructor -> 
                      QuantumStack a b -> [StackAddress]
getSecondLevelConsAddresses cons 
   = head . (List.map (getConsAddresses cons)) . getSecondLvlStacks
                     
\end{code}
\end{singlespace}
}


The function |prepare| is used when doing a transform on a quantum stack. It will
rotate up the requisite number of \qbits{} for the transform.

{\begin{singlespace}
\begin{code}

prepare::(Quantum a b) => Int -> QuantumStack a b -> 
         ([StackAddress],QuantumStack a b)
prepare n q@(StackCons _ _) = 
    let (oldnms, upnms) = getUpNames n q
        q' = List.foldl (flip rotateup) q $ reverse upnms
    in (oldnms, q')
prepare _ qs = ([],qs)
\end{code}
\end{singlespace}}

The function |getUpNames| is used only by |prepare| to determine 
the names of both the data nodes that are going to possibly be
rotated down and the names of the \qbit{}s to rotate up. The 
function |getUpNames| will only return 
the required number of \qbit{} names.

It uses the subordinate functions |getQbits| which returns 
a list of the bound variables which are \qbits{}  and |getDstacks|
which returns a list of pairs of names of other data nodes and \qbits{}
at lower levels in the quantum stack. These subordinate functions
are explained more fully below.



{\begin{singlespace}
\begin{code}
getUpNames ::(Quantum a b) => Int -> QuantumStack a b -> 
             ([StackAddress],[StackAddress])
getUpNames n qs 
    | n <= 0 = ([],[])
getUpNames n (StackCons nm cmap)
    | 1 == Map.size cmap
       = let [(_,(subs,q))] = Map.toList cmap
             qbnms = getQbits subs q
             dstacks = getDStacks subs q
             m = n - (length qbnms)
             morenames = List.foldl (\(x,y) (a,b) -> (x ++ a, y++b)) ([],[]) $
                         (List.map  $ getUpNames m) dstacks
         in (nm:fst morenames,
               take n $ qbnms ++ (snd morenames))
    | otherwise = 
        error "Indeterminate qbit order for transform"
\end{code}
\end{singlespace}}

The function |getQbits| filters its first argument (a list of
|StackAddress|es) by determining which of them are \qbit{}s in the
current quantum stack. The filtered list is then returned.

The filtering function |isQbit| returns true if the top node is 
a \qbit{} with the correct name, otherwise it recurses down the quantum stack.

{\begin{singlespace}
\begin{code}
getQbits ::Quantum a b => [StackAddress] -> QuantumStack a b ->
           [StackAddress]
getQbits nms q = List.filter (flip (isQbit) q) nms


isQbit ::Quantum a b => StackAddress -> QuantumStack a b -> Bool
isQbit nm (StackQbit qnm qvls) 
       = nm == qnm || (Map.fold (||) False $ Map.map (isQbit nm) qvls)
isQbit nm (StackInt _ cvls) 
       = Map.fold (||) False $ Map.map (isQbit nm) cvls
isQbit nm (StackCons _ dvls) 
       = Map.fold (||) False $ Map.map ((isQbit nm) . snd) dvls
isQbit _ _ = False
\end{code}
\end{singlespace}}

The function |getDStacks| first filters its first argument (a list of
|StackAddress|es) by determining which of them are data nodes. These 
are then paired with the current quantum stack and in each case, that node
is rotated up to the top of the quantum stack. This is then 
suitable for further recursion by |getUpNames|

The filtering function |isDtype| returns true if the top node is 
a data node with the correct name, otherwise it recurses down the quantum stack.


{\begin{singlespace}
\begin{code}

getDStacks ::Quantum a b => [StackAddress] -> QuantumStack a b ->
            [QuantumStack a b]
getDStacks nms q = 
    (List.map (uncurry rotateup)) $ getDtypesNmAndQs nms q

getDtypesNmAndQs ::Quantum a b => [StackAddress] -> 
                   QuantumStack a b ->
                   [(StackAddress, QuantumStack a b)]
getDtypesNmAndQs nms q = 
    zip (List.filter (flip (isDtype) q) nms) $ repeat q
            


isDtype ::Quantum a b => StackAddress -> QuantumStack a b -> Bool
isDtype nm (StackCons dnm dvls) 
       = nm == dnm || (Map.fold (||) False $ 
                          Map.map ((isDtype nm) . snd) dvls)
isDtype nm (StackInt _ cvls) 
       = Map.fold (||) False $ Map.map (isDtype nm) cvls
isDtype nm (StackQbit _ qvls) 
       = Map.fold (||) False $ Map.map (isDtype nm) qvls
isDtype _ _ = False

\end{code}
\end{singlespace}
}
