\incsec{Types for Symbol Table}
\label{inscec:typessymboltable}
%if false
\begin{code}
module SemTypes where
import Qtypes
import IrTypes
import Data.Tuples
import Control.Monad.State
import Data.Map as Map
import Data.List as List( delete, map)
import Data.Set as Set
import Data.Stack as Stack
import CompSupport(hasDuplicates, mkflabel)
import TypeUnification
import SemanticErrors
 
\end{code}
%endif

\incsubsubsec{\hasktypenoref{SymEntry}}
\label{haskelltype:SymEntry}\index{Compiler Data Types!symbol table!SymEntry}
This type is the basic entry in the symbol table, holding our information about
either arguments, variables or functions that are known at a particular point 
in time. 

For arguments and variables, we track the offset, which level it is at, its name and type.
Functions track their level, name, a generated code label, arguement types and return types.

Currently types are limited to \bit, \qbit{} and a variable length integer.
\CodeContinueNumbers
\begin{code}


data SymEntryGlobal
    =  SeData {
               gname::String,
               steTypeVars ::[Identifier],
               steConstructors :: [ConsIdentifier] } 
    |  SeCons {
               gname::String,
	       goffset::Int,
	       argtypes::[Qtype],
               rettypes::[Qtype] }-- Always just one..
    |  SeFun {
              gname::String,
              cdlabel::String,
              transform :: Maybe UnitaryTransform,
              parmnames :: (([Nodename],[Nodename]),
                            ([Nodename],[Nodename])),
              cargtypes::[Qtype],
              qargtypes::[Qtype],
              qrettypes::[Qtype],
              crettypes::[Qtype] }
     deriving (Eq,Show)

data SymEntryClassical 
    = SeUse  {cname::Nodename,
	      coffset::Int ,
	      level::Level,
	      ctype::Qtype }
    | SeCArg  {
	      coffset::Int,
	      level::Level,
	      cname::Nodename,
	      ctype::Qtype }
   | SeCVar  {level :: Level,
	      coffset::Int,              
	      cname::Nodename,
	      ctype::Qtype }
     deriving (Eq,Show)

data SymEntryLinear
    = SeCaseId  {
		 offset::Int ,
	         name::Nodename,
	         qtype::Qtype }
    | SeLArg  {
	      offset::Int ,
	      name::Nodename,
	      qtype::Qtype }
    | SeVar {
	     name::Nodename,
	     qtype::Qtype }
     deriving (Eq,Show)


\end{code}
%endif
\incsubsubsec{\hasktypenoref{SymbolTable}}
\label{haskelltype:SymbolTable}\index{Compiler Data Types!symbol table!SymbolTable}
The symbol table is held a a map from strings (the identifier) to symbol
table entries. As we will be keeping a stack of symbol tables in the
semantic analysis phase, we need only keep the 
current current definition of a particular variable.
\CodeContinueNumbers
\begin{code}
type SymbolTableLinear = Map.Map String SymEntryLinear
type SymbolTableGlobal = Map.Map String SymEntryGlobal
type SymbolTableClassical = Map.Map String SymEntryClassical

\end{code}

\begin{code}
nameAndType :: SymEntryLinear -> (Nodename, Qtype)
nameAndType se = (name se, qtype se) 
\end{code}

\incsubsubsec{\hasktypenoref{QBitUsageTable}}
\label{haskelltype:QBitUsageTable}\index{Compiler Data Types!symbol table!QBitUsageTable}
This table is similar to the symbol table, but it maps to  lists of
symbol table useage. FIXME WHY AM I HERE.

Qbit usage is important to track as we are not allowed to duplicate
 qbits. This means, for example, that if we call a routine with
 two qbit arguments, we must pass by reference the references must \emph{not}
 be to the same qbit. Similarly, multi-qbit transformations must
be applied to distinct \qbit{}s.

\CodeContinueNumbers
\begin{code}
type QBitUsageTable  = Map.Map String [SymEntryLinear]
\end{code}

\CodeContinueNumbers
\begin{code}

emptyFqbitusage :: QBitUsageTable
emptyFqbitusage = Map.empty

\end{code}
\incsubsubsec{\haskfuncnoref{offLvlType}}\label{haskellfunction:offLvlType}\index{Compiler Functions!Symbol Table!offLvlType}
This gets the offset, the level and the current types.
\CodeContinueNumbers
\begin{code}

makeUseEntries :: Int -> Level -> [SymEntryLinear] -> [SymEntryClassical]
makeUseEntries  = mkUseEntries' 

mkUseEntries' :: Int-> Level -> [SymEntryLinear] -> [SymEntryClassical]
mkUseEntries' i l [] = []
mkUseEntries' i l ((SeVar nn t):es) 
    = (SeUse nn i l t): (mkUseEntries' (i-1) l es) 
mkUseEntries' i l ((SeLArg o nn t):es) 
    = (SeUse nn i l t): (mkUseEntries' (i-1) l es) 
mkUseEntries' i l ((SeCaseId o nn t):es) 
    = (SeUse nn i l t): (mkUseEntries' (i-1) l es) 

inUse :: SymEntryClassical -> Bool
inUse = insideUse . level

callingLabel :: SymEntryGlobal -> String
callingLabel (SeFun gnm cdl _ _ _ _ _ _)
             = mkflabel gnm cdl
callingLabel _ = error callinglblOnCons

-- subInArgs :: [SymEntry]->[SymEntry]->[SymEntry]
-- subInArgs [] _ = []
-- subInArgs (q:qs) ss= (subInArgsList q ss) : (subInArgs qs ss)

subInArgsList :: [SymEntryLinear] ->[SymEntryLinear] -> [SymEntryLinear]
subInArgsList [] _ = []
subInArgsList (~(SeCaseId offset name QBIT):qs) ss
	         = (ss !!(-(1 + offset))):(subInArgsList qs ss)
subInArgsList (q:qs) ss
	    = q:(subInArgsList qs ss) 


argsmatch :: SymEntryGlobal->[SymEntryLinear]->Bool
argsmatch se args =
     foldl (&&) True $ zipWith (==) (argtypes se) $ List.map qtype args
 
 

selectQBits :: [SymEntryLinear]->[SymEntryLinear]
selectQBits [] = []
selectQBits (s:ss) 
	     | qtype s == QBIT = s:selectQBits ss
	     | otherwise = selectQBits ss
\end{code}
\incsubsubsec{\hasktypenoref{SemanticState}}
\label{haskelltype:SemanticState}\index{Compiler Data Types!symbol table!SemanticState}
The monad used in the semantic analysis phase carries the current
level (function and block), the symbol table, the storage requirement, the
number of arguments and a counter for creating new code labels.
\incsubsec{Declaration Level}
\label{incsec:monadsupport}
The semantic analysis and code generation makes extensive use of 
Exception-State-IO monads. We simplify the use of the monads by the definition
of various functions and a data type.
\CodeContinueNumbers
\begin{code}
data SemanticState 
     = SemState {semLvl :: Level,
		 stateStabGlobal :: SymbolTableGlobal,
		 stateStabLinear :: SymbolTableLinear,
		 stateStabClassical :: SymbolTableClassical,
--		 stor :: Storage,
		 numArgs :: Int,
		 idCounter :: Int,
                 typeVarCounter :: Int,
		 qbitDups :: Stack QBitUsageTable,
                 typeEquations :: Set TypeEquation,
                 warnings :: [String],
                 cStackOffset :: Stack Int
		}
\end{code}
\incsubsubsec{\hasktypenoref{SemStateMonad}}
\label{haskelltype:SemStateMonad}\index{Compiler Data Types!symbol table!SemStateMonad}
A monad based on a state transformer of the \hasktypenoref{SemanticState} and 
\hasktypenoref{IO}.
\CodeContinueNumbers
\begin{code}
type SemStateMonad a =  StateT SemanticState IO a 
\end{code}
%if false
\CodeContinueNumbers
\begin{code}

dumpst  ::  SemStateMonad ()
dumpst 
     = do stab <- getSymTabLinear
	  liftIO $ print stab
	  return ()


dumpsts  ::  SemStateMonad ()
dumpsts 
     = do liftIO $ print "Reg SymTab"
          stab <- getSymTabLinear
          liftIO $ print "got stab"
	  liftIO $ print stab
          liftIO $ print "Type SymTab"
          stabt <- getSymTabGlobal
	  liftIO $ print stabt
          liftIO $ print "Classical SymTab"
          stabc <- getSymTabClassical
	  liftIO $ print stabc
	  return ()


\end{code}
%endif

\begin{code}

\end{code}
\incsubsec{|SemStateMonad| accessors}
\label{haskellfunction:getSemLvl}
\label{haskellfunction:setSemLvl}
\label{haskellfunction:getSymTab}
\label{haskellfunction:setSymTab}
\label{haskellfunction:getQbitUsage}
\label{haskellfunction:setQbitUsage}
\label{haskellfunction:addQbitUsage}
\index{Compiler Functions!Semantic Analysis!|SemStateMonad| accessors}
\begin{code}

incOffsetByN :: Int-> SemStateMonad ()
incOffsetByN n = modify (\st -> let (top, offStack) = pop $ cStackOffset st
                                 in st{cStackOffset = push (n+ top) offStack})

incOffset :: SemStateMonad ()
incOffset = incOffsetByN 1

decOffset :: SemStateMonad ()
decOffset = incOffsetByN (-1)
            
decOffsetByN :: Int -> SemStateMonad()
decOffsetByN = incOffsetByN . negate

setOffsetStack :: Stack Int -> SemStateMonad ()
setOffsetStack sis = modify (\st -> st{cStackOffset = sis})

popOffset :: SemStateMonad Int
popOffset = do (top, cstack) <- gets (pop . cStackOffset)
               setOffsetStack cstack
               return top

getOffset :: SemStateMonad Int
getOffset = gets (fst . pop . cStackOffset)

pushOffsetN :: Int -> SemStateMonad ()
pushOffsetN n = do cso <- gets cStackOffset
                   modify (\st -> st{cStackOffset = push n cso})
pushOffset :: SemStateMonad ()
pushOffset = pushOffsetN 0

copyAndPushOffset :: SemStateMonad ()
copyAndPushOffset = do off <- getOffset
                       pushOffsetN off
                       
getSemLvl  ::  SemStateMonad Level
getSemLvl = gets semLvl

setSemLvl  ::  Level -> SemStateMonad ()
setSemLvl l = modify (\ state -> state {semLvl = l})

warnssm :: String -> SemStateMonad ()
warnssm warning = modify (\ssm -> ssm {warnings = warning : (warnings ssm)})

modSemTopOfStack :: (SemanticState -> Stack a)-> 
                     (Stack a -> SemanticState->SemanticState)->
                     (a -> a) ->SemStateMonad ()
modSemTopOfStack getit putit modit
     = do st <-gets getit
	  let (s, st') = pop st
              s' = modit s
          setSemStack putit $ push s' st'

getSemStack  :: (SemanticState -> Stack a)-> SemStateMonad a
getSemStack f =  gets (Stack.get . f)

popSemStack  :: (SemanticState -> Stack a)-> 
                 (Stack a -> SemanticState->SemanticState)->SemStateMonad a
popSemStack getit putit 
     =  do st <-gets getit
	   let (s, st') = pop st
           setSemStack putit st'
	   return s

pushSemStack ::  (SemanticState -> Stack a)-> 
                  (Stack a -> SemanticState->SemanticState) ->
                  a -> SemStateMonad ()
pushSemStack getit putit st 
     = do stk <- gets getit
          setSemStack putit $ push st stk

setSemStack  ::   (Stack a -> SemanticState->SemanticState) ->
                   Stack a -> SemStateMonad ()
setSemStack putit s = modify (putit s)

{-
getSymTabClassical :: SemStateMonad (SymbolTableClassical)
getSymTabClassical = getSemStack stateStabClassical

updateSymTabClassical :: Stack SymbolTableClassical ->
                             SemanticState ->SemanticState
updateSymTabClassical s st = st{stateStabClassical = s}

pushSymTabClassical :: SymbolTableClassical -> SemStateMonad ()
pushSymTabClassical = pushSemStack stateStabClassical updateSymTabClassical
-}

updateSymTabLinear :: SymbolTableLinear ->SemanticState ->SemanticState
updateSymTabLinear s st = st{stateStabLinear = s}

updateSymTabGlobal :: SymbolTableGlobal ->SemanticState ->SemanticState
updateSymTabGlobal s st = st{stateStabGlobal = s}

updateSymTabClassical :: SymbolTableClassical ->SemanticState ->SemanticState
updateSymTabClassical s st = st{stateStabClassical = s}


modSymTabGlobal :: (SymbolTableGlobal -> SymbolTableGlobal) ->SemStateMonad ()
modSymTabGlobal f 
    = modify (\ state -> state{stateStabGlobal 
                                   = f $ stateStabGlobal state})

modSymTabLinear :: (SymbolTableLinear -> SymbolTableLinear) ->SemStateMonad ()
modSymTabLinear f 
    = modify (\ state -> state{stateStabLinear 
                                   = f $ stateStabLinear state})

modSymTabClassical :: (SymbolTableClassical -> SymbolTableClassical) ->SemStateMonad ()
modSymTabClassical f 
    = modify (\ state -> state{stateStabClassical 
                                   = f $ stateStabClassical state})


getSymTabGlobal  ::  SemStateMonad SymbolTableGlobal
getSymTabGlobal =  gets stateStabGlobal

getSymTabLinear  ::  SemStateMonad SymbolTableLinear
getSymTabLinear =  gets stateStabLinear

getSymTabClassical  ::  SemStateMonad SymbolTableClassical
getSymTabClassical =  gets stateStabClassical

setSymTabGlobal  ::  SymbolTableGlobal -> SemStateMonad ()
setSymTabGlobal st = modSymTabGlobal (const st)

setSymTabLinear  ::  SymbolTableLinear -> SemStateMonad ()
setSymTabLinear st = modSymTabLinear (const st)

setSymTabClassical  ::  SymbolTableClassical -> SemStateMonad ()
setSymTabClassical st = modSymTabClassical (const st)

getQbitUsage  ::  SemStateMonad QBitUsageTable
getQbitUsage =  gets (Stack.get . qbitDups)

popQbitUsage  ::  SemStateMonad QBitUsageTable
popQbitUsage =  do st <-gets  qbitDups
		   let (s, st') = pop st
		   setQbitUsage st'
		   return s

pushQbitUsage ::  QBitUsageTable -> SemStateMonad ()
pushQbitUsage st = modify (\ state -> 
			    state {qbitDups = push st (qbitDups state)})

setQbitUsage  ::  Stack QBitUsageTable -> SemStateMonad ()
setQbitUsage s = modify (\ state -> state {qbitDups = s})

addQbitUsage  ::  String->[SymEntryLinear] -> SemStateMonad ()
addQbitUsage k e
     = do qbu <- popQbitUsage
          pushQbitUsage $ Map.insertWith (++) k e qbu
{-     = modify (\state -> 
	       state {qbitDups 
		      = Map.insertWith (++) k e (qbitDups state) })

getfuncdefs  ::  SemStateMonad [String]
getfuncdefs   =  gets infuncDefs

setfuncdefs  ::  [String] -> SemStateMonad ()
setfuncdefs s = modify (\ state -> state {infuncDefs = s})

pushfuncdef  ::  String-> SemStateMonad ()
pushfuncdef s
     = modify (\state -> 
	       state {infuncDefs = s:(infuncDefs state)})

popfuncdef  ::  SemStateMonad ()
popfuncdef =
    modify ( \ state -> 
	         state {infuncDefs 
		        = case (infuncDefs state) of
		            [] -> []
		            (_:ss) -> ss
		       })

getrecfuncnames  ::  SemStateMonad [String]
getrecfuncnames   =  gets recfuncNames

setrecfuncnames  ::  [String] -> SemStateMonad ()
setrecfuncnames s = modify (\ state -> state {recfuncNames = s})

pushrecfuncname  ::  String-> SemStateMonad ()
pushrecfuncname s
     = modify (\state -> 
	       state {recfuncNames = s:(recfuncNames state)})

poprecfuncname  ::  SemStateMonad ()
poprecfuncname 
     = modify (\state -> 
	       state {recfuncNames
		      = case (recfuncNames state) of
		      [] -> []
		      (_:ss) -> ss
		     })

querypoprecfuncname  ::  String->SemStateMonad ()
querypoprecfuncname slook
     = modify ( \ state -> 
	            state {recfuncNames 
                               = List.delete slook  (recfuncNames state)})
-}
{-
getStorage  ::  SemStateMonad Storage
getStorage = gets stor

setStorage  ::  Storage ->SemStateMonad ()
setStorage t = modify (\ state -> state {stor = t})
-}
  
inclabel  ::  SemStateMonad ()
inclabel = modify (\ state -> 
		    state {idCounter 
			   = 1+idCounter state})

inctypevar  ::  SemStateMonad ()
inctypevar = modify (\ state -> 
		          state {typeVarCounter 
			             = 1+typeVarCounter state})

{-
incbits  ::  SemStateMonad ()
incbits  = modify (\ state -> 
		    state {stor = sinc BIT 1 $ stor state})
addtobits  :: Int ->  SemStateMonad ()
addtobits i = modify (\ state -> 
		    state {stor = sinc BIT i $ stor state})
incqbits  ::  SemStateMonad ()
incqbits = modify (\ state -> 
		    state {stor = sinc QBIT 1 $ stor state})
-} 
incargs  ::  SemStateMonad ()
incargs = modify (\ state -> 
		   state {numArgs = 1 + numArgs state})
decargs  ::  SemStateMonad ()
decargs = modify (\ state -> 
		   state {numArgs = (-1) + numArgs state})

getlabel  ::  SemStateMonad Int
getlabel = gets (idCounter)

getTypeEquations :: SemStateMonad (Set TypeEquation)
getTypeEquations = gets typeEquations

addTypeEquation :: TypeEquation -> SemStateMonad ()
addTypeEquation eq@(TypeEquation t1 t2) 
    = if t1 /= t2
      then modify (\state -> state {typeEquations =
                                        Set.insert eq (typeEquations state)})
      else return ()
newTypeVar :: SemStateMonad String
newTypeVar = 
    do tvc <- gets typeVarCounter
       modify (\s -> s{typeVarCounter = 1 + tvc})
       return $ "tv" ++ (show tvc)
{-
getbits  ::  SemStateMonad Int
getbits  = gets (bitsOfStorage . stor)
getqbits  ::  SemStateMonad Int
getqbits = gets (qbitsOfStorage . stor)
getargs  ::  SemStateMonad Int
getargs = gets (numArgs)

setbits  ::  Int -> SemStateMonad ()
setbits n = modify (\ state -> state{stor = setStore BIT n $ stor state})
setqbits  ::  Int -> SemStateMonad ()
setqbits n = modify (\ state -> state{stor = setStore QBIT n $ stor state})
-}
setargs  ::  Int -> SemStateMonad ()
setargs n = modify (\state -> state{numArgs = n})


\end{code}
\begin{code}

symTabMerge :: SymbolTableLinear -> SymbolTableLinear -> 
               SemStateMonad ([Istmt], [Istmt], SymbolTableLinear)
symTabMerge st1 st2
    = do tes <- gets typeEquations
         --liftIO $ print "merge st - getting map difference"
         --liftIO $ print "First for merge"
         --liftIO $ print st1
         --liftIO $ print "Second for merge"
         --liftIO $ print st2
         let apmap = unifyTESet Map.empty tes
             s1minuss2 = Map.difference st1 st2
             s2minuss1 = Map.difference st2 st1
             s1only = Map.map (updateStTypes apmap) s1minuss2
             s2only = Map.map (updateStTypes apmap) s2minuss1

         stmts1 <- mapM (uncurry makeDiscard) $ Map.toList s1only
         stmts2 <- mapM (uncurry makeDiscard) $ Map.toList s2only
         mapM_ (uncurry warnDelete) $ Map.toList  s1only
         mapM_ (uncurry warnDelete) $ Map.toList  s2only
         let newst =  Map.intersection st1 st2
         return (stmts1, stmts2, newst)
         

warnDelete :: String -> SymEntryLinear -> SemStateMonad()
warnDelete id se
    = warnssm $ unbalancedCreation id $ show $ qtype se

updateStTypes :: Map Identifier Qtype -> SymEntryLinear -> SymEntryLinear
updateStTypes mp (SeVar n t)
              = SeVar n $ appunify mp t
updateStTypes mp (SeLArg o n t)
              = SeLArg o n $ appunify mp t
updateStTypes _ e = e         

makeDiscard :: String -> SymEntryLinear -> SemStateMonad (Istmt)
makeDiscard id (SeVar n t)
    = return $ Idiscard [n]
makeDiscard id _ = return $ Idiscard []

         

\end{code}
