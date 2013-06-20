\incsec{The semantic checking}
\label{incsec:semantic}
%if false
\begin{code}
module Semantic where
import IrTypes
import Qtypes
import Data.Tuples
import Control.Monad.State as State
import SemTypes
import SymbolTable
import Data.List as List
import Data.Bits
import Data.Stack
import CompSupport(hasDuplicates)
import TypeUnification
import Data.Map as Map
import Data.Set as Set
import SemanticErrors
\end{code}
%endif
We create the IR for a program as a list of the sizes of the global variables,
then two lists of functions and statements. This is a multi pass process in
that we do the following in order:
%if false
 TODO - update this with what really happens.
%endif
\bi
\item{} Update the symboltable with the types that are declared. 
(\haskfunc{updateSymTab})
\item{} Get the sizes of all variables. 
\item{} Update the symbol table with the variables. (\haskfunc{updateSymTab} and 
\haskfunc{updateSymTabList})
\item{} Create the list of functions that are declared. 
\item{} Finally, create the intermediate representation
 for each of the statments. (\haskfunc{stmtListIr})
\ei

All of the transformation functions use the \hasktype{SemStateMonad} to 
output. See \fullref{incsec:semantic}
\incsubsec{Tranformation of the program type}
\label{incsec:transprogram}
\incsubsubsec{\haskfuncnoref{block progIr} and \haskfuncnoref{progIr}}
\label{haskellfunction:blockprogIr}
\index{Compiler Functions!semantics!blockprogIr}\label{haskellfunction:progIr}
\index{Compiler Functions!semantics!progIr}
This function will transform the parsed form of the program into an IR.

\CodeResetNumbers
\begin{code}

progIr :: Program->SemStateMonad  Iprog
progIr  (Program gds)
     = do --liftIO $ print $ "iring prog"
          addTypesToSt gds
          addProcsToSt gds
          --liftIO $ print $ "progir - added procs to st"
          --dumpsts
          let procs = filterProcs gds
          iprocs <- mapM procIr procs
          return $ Iprog $ Map.fromList $ zip (List.map procnm procs) iprocs
\end{code}

Type unifiying of procs:
We want:
   Somehow add output types as type equations "to be", for use when
doing the semantic and ir gen of the statements in the proc.

The type of the input items are unified to the type of the actual args
upon calling. However, we also would want to do unifying at this stage. 
For example, suppose the proc is declared with a List(a) input, but 
we then pass the head of the list to a proc expecting an (Either x y).

We then have a type equation of a == Either x y which could / should be
used for further unification.

\begin{code}


procIr :: Procedure -> SemStateMonad Iproc
procIr (Procedure procnm incparms inqparms outqparms outcparms procstmts)
     = do stl<-getSymTabLinear
          stc<-getSymTabClassical
          tes <- getTypeEquations
          procent <- stgloballookup procnm
          lvl <- getSemLvl
          setSemLvl $ incFunc lvl
          updateStList inqparms 
          pushOffset --Classical stack locns
          updateClassicalListP incparms
          updateClassicalListP outcparms
          outentries <- mapM classicalStlookup $ List.map parmId outcparms
          --dumpst
          stsir <- stmtListIr procstmts 
          setSymTabLinear stl
          setSymTabClassical stc
          setSemLvl lvl
          popOffset
          return $ Iproc procnm (cdlabel procent)
                     (List.map parmType incparms) 
                     (List.map parmType inqparms)
                     (List.map parmType outqparms) 
                     (List.map (\ e -> (coffset e, ctype e)) outentries) stsir

\end{code}
\incsubsec{Transformation of statements}
\label{incsec:transstatement}
\incsubsubsec{\haskfuncnoref{stmtListIr}}
\label{haskellfunction:stmtListIr}
\index{Compiler Functions!semantics!stmtListIr}
We first set up a simple function that will go through a list of statements,
producing the accompanying IR for each of them.
\begin{code}
stmtListIr :: [Statement]->SemStateMonad [Istmt]
stmtListIr [] = do return []
stmtListIr (s:ss) 
     = do stmt <- stmtIr s
	  stmts <- stmtListIr ss
	  return $ stmt ++ stmts

\end{code}
\incsubsubsec{\haskfuncnoref{stmtIr}}
\label{haskellfunction:stmtIr}
\index{Compiler Functions!semantics!stmtIr}
This function pattern matches based upon the \hasktype{QPLstmt} constructors and 
produces the appropriate IR.
\begin{code}
stmtIr :: Statement->SemStateMonad [Istmt]

\end{code}
\incsubsubsec{Assignment Statements.} This will produce a \haskcons{IZero} or
\haskcons{IOne} 
IR, translating the identifier to its offset and level.
\CodeContinueNumbers
\begin{code}

stmtIr stmt@(Assignment id exp)
   = do (irexp, irt, classical) <- exprIr exp
        cvar <- maybeLookupClassical id
        case cvar of 
           Nothing -> 
            do updateSt stmt
               sentry <- stlookup id
               flatirexp <- flattenIrExp irexp
               addTypeEquation (TypeEquation (qtype sentry) irt)
               let ecqexp = if classical 
                              then Left flatirexp 
                              else Right flatirexp
               return ([Iassign (name sentry)  ecqexp])
           Just clsclVar -> --remove form clscl side after
            if (classical) then 
               do flatirexp <- flattenIrExp irexp
                  warnssm $ "Converting classical var to quantum, in " ++ id ++ " = " ++ (show exp) 
                  updateSt stmt
                  sentry <- stlookup id
                  addTypeEquation (TypeEquation (qtype sentry) irt)
                  return ([Iassign (name sentry) (Left flatirexp)])
             else
               fail $ qToCError (show exp) id

\end{code}


\incsubsubsec{Looping.} The \haskcons{QPLwhile} is translated to either an
\haskcons{IWhile} or it fails if the test was not of a \bit.
The identifier is replaced with the offset and level.
While now gone.

stmtIr  (While exp s) 
  = do irs <- stmtListIr s 
       (irexp, irt) <- exprIr exp
       if  BIT == irt
           then return ([Iwhile irexp irs] )
           else fail "While must test a bit." 
\begin{code}
stmtIr  (CaseSt exp clauses) 
  = do (irexp, irt, classical) <- exprIr exp
       if classical 
         then fail "Can not case a classical value"
         else do irc <- clauseListIr irt clauses
                 return ([Icase irexp  irc] )


\end{code}

\incsubsubsec{Conditional statements.} The \haskcons{QPLcond} or \haskcons{QPLmeas}
 is translated to one of 
\haskcons{Icond} or \haskcons{Imeas}. It may fail if the identifier was not of the
correct type.
The identifier is replaced with the offset and level.
Cond now gone.

stmtIr (Cond exp s1 s2) 
     = do  irs1 <- stmtListIr  s1
           irs2 <- stmtListIr  s2 
           (irexp, irt) <- exprIr exp
           if BIT == irt
              then return [Icond irexp irs1 irs2]
              else fail "If expression must be of type BIT."
\begin{code}
          
stmtIr (Measure exp s1 s2) 
     = do (irexp, irt, classical) <- exprIr exp
          copyAndPushOffset
          irs1 <- stmtListIr s1 
          popOffset
          st1 <- getSymTabLinear
          copyAndPushOffset
          irs2 <- stmtListIr  s2 
          popOffset
          st2 <- getSymTabLinear
          (exir1, exir2, st) <- symTabMerge st1 st2
          setSymTabLinear st
          t <- semanticUnify irt
          if QBIT == t
             then return [Imeas irexp (irs1 ++ exir1)  (irs2 ++ exir2)]
             else fail $ measureNotQbit (show exp) $ show irt

\end{code}

\incsubsubsec{The block statement.} Here, we must handle the possiblity of
new identifiers being used in this block. To do this, we save the symbol table
and the amount of storage in use at the beginning of the block.
 We then add the 
statements in the block to the symbol table and compute the
 amount of storage used
in this block. The IR for the statements is computed recursively 
after they have 
been added to the symbol table and both items are returned.
\begin{code}


stmtIr (BlockStatement stmtlist)
     = do --liftIO $ print $ "Iring block statments "++ (showList stmtlist "")
          copyAndPushOffset
          stc <- getSymTabClassical
--	  stg1 <- getStorage
--	  setbits 0
--	  setqbits 0
          istmts <- stmtListIr stmtlist
	  setSymTabClassical stc
          popOffset
          return ([Iblock  istmts])

\end{code}

\incsubsubsec{Local functions.} This combines the complexity of a block with
numerous others. The primary tasks to be done here are:
\bi
\item Compute the storage.
\item Create IR for the statements in the procedure definition.
\item Create IR for the result statement.
\item Determine if there are any \qbit usage errors. That is, are there any
cases where a \qbit is illegally duplicated?
\item Determine if we are a recursive procedure or not.
\ei
The first two items are similar to that done by the block translation. The third 
is nothing new, just another recursive call to \haskfuncnoref{stmtIr}.  The code
for determining illegal duplication is contained in the functions 
\haskfunc{addQbitUsage} and \haskfunc{updateQbitdups}. These work similarly to 
a shadow symbol table where they add the \qbit usage. This can then be inspected 
to determine if there is illegal duplication. This is intended to catch abstruse 
errors such as having a \qbit declared in an outer block, then used in
a prodedure definition in a transformation with a parameter and then the
result statement calling that procedure with the parameter being the global qbit. 
See the examples \texttt{caught.qpl} and \texttt{caught2.qpl} in 
\fullref{sec:invalidsemantics} .

\incsubsubsec{Procedure calling}
We first start with the standards of any language that calls a procedure. We lookup the
name, the names of the arguments. The first difference is that we then look up this 
procedure in our shadow symbol table that is used to determine if parameters are duplicated.
The remainder of the code checks for gross duplications such as calling the procedure with the
same name in the parameter list and determines if this is a recursive call.
\begin{code}

stmtIr (Call nm cexps qexps qids cids)
    = do --liftIO $ print $ "iring call " ++ nm
         fentry <- stgloballookup nm
         ceirt <- mapM exprIr cexps
         qeirt <- mapM exprIr qexps
         let (ceir, cetypes, cclassicals) = unzip3 ceirt
             (qeir, qetypes, qclassicals) = unzip3 qeirt
	 qbentry <- qbulookup nm
--	 let qbitDups = subInArgsList qbentry argst
         if ((and cclassicals) &&
             argTypesOK (qargtypes fentry) qetypes) && 
                (argTypesOK (cargtypes fentry) cetypes)
             then if False -- hasDuplicates (selectQBits argst)
\end{code}
This checks for an immediate duplication.
\begin{code}
                  then fail $ duplicateQbitsInCall nm
                  else --if hasDuplicates qbitDups
                       --then fail $ duplicateQbitsInCall nm
\end{code}
Now we have ensured we are not violating the qbit duplication rules
and we may proceed with the call.
\begin{code}
		      -- else 
                       do addNewLinearEntries qids (qrettypes fentry) (SeVar)
                          lvl <- getSemLvl
                          addNewClassicalEntries [0..] cids (crettypes fentry) (SeCVar lvl )
                          return [Icall (transform fentry)
                                  (callingLabel fentry)
                                  (parmnames fentry)
                                  ceir qeir
				  (zip qids (qrettypes fentry))
                                  (zip cids (crettypes fentry)) ]

             else fail $ procCallTypeError nm (showList (qargtypes fentry) "") $
                              showList qetypes ""

stmtIr (UseAssign id exp)
    = do (irexp, irt,classical) <- exprIr exp
         lvl <- getSemLvl
         offset <- getOffset
         decOffset
         let sec = SeUse id offset lvl irt
         modSymTabClassical (Map.insert id sec)
         flatirexp <- flattenIrExp irexp
         addTypeEquation (TypeEquation (ctype sec) irt)
         --liftIO $ print $ " Doing UseAssign " ++ id ++ " --> " ++ (show exp)
         --dumpst
         let ecqexp = if classical then Left flatirexp else Right flatirexp
         return ([IuseAssign id ecqexp])

stmtIr (UseFromHere [])
       = return []

stmtIr (UseFromHere (id:ids))
    = do first <- stmtIr (UseAssign id (Evar id))
         rest <- stmtIr $ UseFromHere ids
         return $ first ++ rest


stmtIr (Use ids stmts)
    = do ventry <- mapM stlookup ids
         lvl <- getSemLvl
         setSemLvl $ incUse lvl
         copyAndPushOffset
         offset <- getOffset
         let useids = makeUseEntries offset lvl ventry
         decOffsetByN (length ids)
         stc <- getSymTabClassical
         let lindel:: [SymbolTableLinear -> SymbolTableLinear]
             lindel = List.map (Map.delete) ids
         modSymTabLinear (foldl (.) id lindel)
         modSymTabClassical (foldl (.) id $ 
                             List.map (uncurry Map.insert) $ 
                             zip ids useids)
--         dumpst
         stirs <- stmtListIr stmts
         setSymTabClassical stc
         popOffset
         setSemLvl lvl
         return [Iuse [name v |v<-ventry]
                          stirs]



stmtIr (Guard gcs)
    = do gcirs <- gcListIr gcs
         return [Iguard gcirs]
\end{code}

\incsubsubsec{Unitary transformations} A simple translation that only needs to check for
gross duplications in the arguments and to make sure all arguments are
\qbits.


stmtIr (Transform ut  qbits)
     = do --liftIO $ print "About to ir a transform"
          irAndst <- mapM createQbitIfNeeded qbits
          let (ir, argst) = unzip irAndst
              targs = List.map typeOf argst
          tes <- gets typeEquations
          let apmap = unifyTESet Map.empty tes
              unifTypeArgs = List.map (appunify apmap) targs
	  if foldl1 (&&) (List.map (== QBIT) unifTypeArgs)
	     then 
                 if hasDuplicates argst then
                      fail "Not allowed to apply UT to same qbits."
		 else 
		    case ut of 
                      (Rotate (Right id)) ->
                            do ient <- stlookup id
                               if (inUse ient) then
                                    return $ (concat ir)  ++ 
                                               [Itrans (List.map (nmLvl) argst) ut (Just $ offsetOf ient)]
                                               else
                                    fail "Rotate must be applied to id in USE statement"
                      _ -> return $ (concat ir)  ++ 
                            [Itrans (List.map (nmLvl) argst) ut Nothing]
	     else fail $ "Unitary transform allowed for qbits only - "++
                  showList argst ""


\incsubsubsec{Declaration of new \qbits and \bits.}
The IR for new bits and new qbits is actually subsumed by the
storage information at a block or procedure. Hence, these two
statements do not need to return anything. Naturally, a "skip"
statement also does not need any intermediate representation.

We could have return a "Maybe Istmt" from this function, but instead 
chose to include a no data constructor in the IR of statement. The 
skips are removed from the list of statements.

Note that the "new" statements are the equivalent of declarations. However,
we  incremented the storage counters when adding them to the symbol table.
\begin{code}
stmtIr (Discard ids)
    = do --liftIO $ print "In discard"
         --dumpst
         idents <- mapM stlookup ids
         let dels = Map.fromList $ zip ids idents
         --stabStack <- gets stateStab
         -- setSymTabLinear $ fmap  ((flip Map.difference) dels) stabStack
         -- respect stack structure , only discard within current one.
         modSymTabLinear ((flip Map.difference) dels)
         --dumpst
         return [ Idiscard $ List.map name idents ]

stmtIr (ControlledBy stmt ids)
    = do istmt <- stmtIr stmt
         idents <- mapM stlookup ids
         return [ IcontrolledBy istmt $ 
                          List.map name idents ]

stmtIr (Skip) 
        = do return []

stmtIr (ZeroStack) 
        = do return [IzeroStack]

\end{code}

\begin{code}

gcListIr :: [GuardClause] -> SemStateMonad [(IrExpression, [Istmt])]
gcListIr = mapM guardClauseIr

guardClauseIr :: GuardClause -> SemStateMonad (IrExpression, [Istmt])
guardClauseIr (GuardClause e ss)
    = do (ire, gct,classical) <- exprIr e
         if (gct == BOOL && classical) 
            then do copyAndPushOffset
                    irss <- stmtListIr ss
                    popOffset
                    return (ire, irss)
            else error $ "Guard clause must be a classical BOOL type, not " ++ (show gct)

semanticUnify :: Qtype -> SemStateMonad (Qtype)
semanticUnify t
   = do tes <- gets typeEquations
        return $ appunify (unifyTESet Map.empty tes) t


consTypesOK :: Qtype -> [Qtype] ->Bool --Check that first is instance of all snds
consTypesOK _ _ = True

clauseListIr :: Qtype -> 
                [(CaseClause, [Statement])] ->
                SemStateMonad [IrCaseClause]
clauseListIr _ [] = do return []
clauseListIr ctype (cclause:[])
    = do stc <- getSymTabClassical
         irc1 <- clauseIr ctype cclause
         setSymTabClassical stc
         return [irc1]
         
clauseListIr ctype (cclause:clauses)
    = do stc <- getSymTabClassical
         st0 <- getSymTabLinear
         irc1 <- clauseIr ctype cclause
         setSymTabClassical stc
         st1 <- getSymTabLinear
         setSymTabLinear st0
         ircRest <- clauseListIr ctype clauses
         st2 <- getSymTabLinear
         --liftIO $ print "About to merge"
         (exir1, exir2, st) <- symTabMerge st1 st2
         setSymTabLinear st
         let irc1' = appendStatementsToClause exir1 irc1
             ircRest' = List.map (appendStatementsToClause exir2) ircRest
         return $ irc1' : ircRest'
         
clauseIr :: Qtype -> 
            (CaseClause, [Statement])->
            SemStateMonad (IrCaseClause)
clauseIr casetype ((CaseClause cid ids),stmts)
    = do --liftIO $ print $ "In case clause " ++ cid
         ccentry <- stgloballookup cid
         --TODO add casetype check with , head $ rettypes ccentry
         let untable = unify (Map.empty) casetype $ head $ rettypes ccentry
             clstypes = List.map (appunify untable) $ 
                           argtypes ccentry
--             newentries = zip ids $ 
--               List.map (\ (o,(idn,typ)) -> SeCaseId o idn typ) $
--                   zip [0..] $ zip ids $ clstypes

         --liftIO $ print $ "Adding " ++ showList ids ""
         addNewLinearEntries ids clstypes (SeCaseId 0)
         --dumpst
         copyAndPushOffset
         sirs <- stmtListIr stmts
         popOffset
         --dumpst
         --liftIO $ print $ "removing " ++ showList ids ""
         removeIdListLinear ids
         --modSymTabLinear (foldl (.) id $ List.map (Map.delete) ids)
         --liftIO $ print "Popped after case clause"
         --dumpst
         --liftIO $ print "return case clause"
         return (IrCaseClause cid ids sirs)
         


\end{code}

\begin{code}


argTypesOK :: [Qtype] ->[Qtype] -> Bool
argTypesOK _ _ = True

exprIr :: Expression -> SemStateMonad (IrExpression, Qtype, Bool)
exprIr (Enot e) 
     = do (ie, irt, b) <- exprIr e
          if (irt == BIT) then return (IrNot ie , BIT, False)
             else fail "Not must operate on a bit"

exprIr (EQbit q) = do return (IrQbit q , QBIT, False)

exprIr (Ebool b) = do return (IrBool b, BOOL, True)

exprIr (Ebracket e) = exprIr e

exprIr (Econs cid es)
   = do eirs <- mapM exprIr es
        let (expirs, etypes, classicals) = unzip3 eirs
        consent <- stgloballookup cid
        if argTypesOK (argtypes consent) etypes
          then return $ (IrCons cid expirs, head $ rettypes consent, False)
          else fail "Constructor arguments are the wrong type"

exprIr (Ecall nm cexps qexps qids)
     = do fentry <- stgloballookup nm
          ceirt <- mapM exprIr cexps
          qeirt <- mapM exprIr qexps
          let (ceir, cetypes, cclassicals) = unzip3 ceirt
              (qeir, qetypes, qclassicals) = unzip3 qeirt
	  qbentry <- qbulookup nm
          case (qrettypes fentry) of
            [] -> fail $ "Call of " ++ nm ++ 
                          " not allowed in expression as there is no return variable." 
            rtypes ->
             if ((and cclassicals) &&
                 argTypesOK (qargtypes fentry) qetypes) && 
                    (argTypesOK (cargtypes fentry) cetypes) 
              then 
                if False -- hasDuplicates (selectQBits argst)
\end{code}
This checks for an immediate duplication.
\begin{code}
                  then fail $ duplicateQbitsInCall nm
                  else --if hasDuplicates qbitDups
                       --then fail $ duplicateQbitsInCall nm
\end{code}
Now we have ensured we are not violating the qbit duplication rules
and we may proceed with the call.
\begin{code}
		      -- else 
                       do addNewLinearEntries qids (qrettypes fentry) (SeVar)
                          return $ (IrExpCall (callingLabel  fentry)
                                    (parmnames fentry)
                                    ceir qeir $ zip qids rtypes,
                                    (last rtypes), False)
              else fail $ "Invalid argument types in call to " ++ nm

exprIr (Enum i) = do return (IrNum  i, INT, True)
exprIr (Evar id )
        = do sentry <- lookupClassicalOrLinear id
             case sentry of
                 Right (centry) -> 
                     return (IrCvar id (coffset centry)
                                        (level centry)
                                        (ctype centry),
                             (ctype centry), True)
                 Left (lentry) -> 
                     do removeIdLinear id --modSymTabLinear (Map.delete id)
                        return (IrVar (name lentry) (qtype lentry), 
                                      qtype lentry, False)
                    
exprIr (Eapply op exp1 exp2 )
        = do (iexp1, ietype1, b1) <- exprIr exp1
             (iexp2, ietype2, b2) <- exprIr exp2
             unift1 <- semanticUnify ietype1
             unift2 <- semanticUnify ietype2
             if (b1 && b2 && binopTypesOK op unift1 unift2)
                then return (Apply op iexp1 iexp2, (optype op), True)
                else fail $ "Invalid types for operation " ++ 
                         (show op) ++ ('(':(show (fromEnum op))) ++ ") " ++
                      (show ietype1) ++ (':':(show b1)) ++
                         ", " ++ (show ietype2) ++ (':':(show b2)) 

binopTypesOK :: BinOp -> Qtype -> Qtype -> Bool
binopTypesOK b = binopTypesOK' (fromEnum b)

binopTypesOK' :: Int -> Qtype -> Qtype -> Bool
binopTypesOK' b INT INT = b < 9 || b > 12
binopTypesOK' b BOOL BOOL = b >= 9 || b <= 14
binopTypesOK' _ _ _ = False

optype :: BinOp -> Qtype
optype b = optype' (fromEnum b)

optype' i
      | i < 9 = INT
      | otherwise = BOOL

flattenIrExp :: IrExpression ->  SemStateMonad IrExpression
flattenIrExp (IrNot e) 
              = do fe <- flattenIrExp e
                   applyNot fe
flattenIrExp (Apply op e1 e2 )
              = do fe1 <- flattenIrExp e1
                   fe2 <- flattenIrExp e2
                   applyOp op fe1 fe2
flattenIrExp e  = do return e

applyNot :: IrExpression -> SemStateMonad IrExpression
applyNot (IrNum  i) = do return (IrNum  (complement i))
applyNot fe = do return (IrNot fe)

applyOp :: BinOp -> IrExpression -> IrExpression -> SemStateMonad IrExpression
applyOp op (IrNum  i1) (IrNum  i2)
     = do return (case op of
                         Add -> (IrNum (i1 + i2))
                         Sub -> (IrNum (i1 - i2))
                         Mul -> (IrNum (i1 * i2))
                         Div -> (IrNum (div i1  i2))
                         Rem -> (IrNum (rem i1  i2))
                         Mod -> (IrNum (mod i1 i2))
                         And -> (IrNum (i1 .&. i2))
                         Or -> (IrNum ( i1 .|. i2))
                         Xor -> (IrNum (xor i1 i2))
                         Opeq -> (IrNum (if i1 == i2 then 1 else 0))
                         Opneq -> (IrNum (if i1 == i2 then 0 else 1))
                         Oplt -> (IrNum (if i1 < i2 then 1 else 0))
                         Opgt -> (IrNum (if i1 > i2 then 1 else 0))
                         Ople -> (IrNum (if i1 <= i2 then 1 else 0))
                         Opge -> (IrNum (if i1 >= i2 then 1 else 0))
                         Oplshift -> (IrNum (shift i1 i2))
                         Oprshift -> (IrNum (shift i1 (-i2)))
                         _ -> Apply op (IrNum i1) (IrNum i2))

applyOp op e1 e2 = do return (Apply op e1 e2)
                               
\end{code}

 
\incsubsec{\haskfuncnoref{makeIr}}
\label{haskellfunction:makeIr}
\index{Compiler Functions!semantics!makeIr}
This function will actually run the syntax of the program through the translation process.
Its derivative function \haskfuncdef{ioMakeIr}{Compiler}{semantics}
 applies this to a base state with an empty 
symbol table.
\begin{code}
makeIr :: Program ->SemStateMonad Iprog
makeIr prog
     = do emptySymbolTables
	  setSemLvl (Lvl 0 0 0 0)
	  programir <- progIr prog
          te <- getTypeEquations
          --liftIO $ print te
          let pir = subProgTypes  (solve te) programir
	  return (pir )

solve :: Set TypeEquation -> Map String Qtype
solve ste
    = Map.unions $ Set.elems $ Set.map solve1 ste


solve1 :: TypeEquation -> Map String Qtype
solve1 (TypeEquation (TYPEVAR s) t) = Map.singleton s t
solve1 _ = Map.empty


ioMakeIr :: Program -> IO Iprog
ioMakeIr qprog
     = do (a,s) <- (runStateT (makeIr qprog) 
		    (SemState (Lvl 0 0 0 0) 
		     Map.empty Map.empty Map.empty 
		     0 0 0
		     (push  emptyFqbitusage emptyStack)
                     Set.empty [] (push 0 emptyStack) )
                   )
          case (warnings s) of 
             [] -> print "No warnings"
             warns -> mapM_ print $ reverse warns
	  return a

\end{code}
\begin{code}

subProgTypes ::Map String Qtype -> Iprog -> Iprog
subProgTypes mse (Iprog pmap) 
             = Iprog $ Map.map (subptypes mse) pmap
subptypes :: Map String Qtype -> Iproc -> Iproc
subptypes mse (Iproc s lbl cins tins touts couts stmts)
  = Iproc s lbl cins tins touts couts $ List.map (substypes mse) stmts

substypes :: Map String Qtype -> Istmt -> Istmt
substypes mse (IuseAssign nn (Left iexp))
  = IuseAssign nn (Left $ subetypes mse iexp)

substypes mse (IuseAssign nn (Right iexp))
  = IuseAssign nn (Right $ subetypes mse iexp)

substypes mse (Iassign nn (Right iexp))
  = Iassign nn (Right $ subetypes mse iexp)
substypes mse (Iassign nn (Left iexp))
  = Iassign nn (Left $ subetypes mse iexp)
substypes mse (Imeas ie s1s s2s)
  = Imeas  (subetypes mse ie) (List.map (substypes mse) s1s) $ 
              List.map (substypes mse) s2s
substypes mse (Icase ie icc)
  = Icase (subetypes mse ie) $ List.map (subctypes mse) icc
substypes mse (Icall ut s nms ies1 ies2 parms1 parms2)
  = Icall ut s nms (List.map (subetypes mse) ies1) 
      (List.map (subetypes mse) ies2) 
      (List.map (app2of2 $ subtype mse) parms1)
      (List.map (app2of2 $ subtype mse) parms2)
substypes mse (Ialloc nn t)
  = Ialloc nn $ subtype mse t
substypes mse (Iblock stmts)
  = Iblock (List.map (substypes mse) stmts)
substypes mse s = s

subctypes :: Map String Qtype -> IrCaseClause -> IrCaseClause
subctypes mse (IrCaseClause c ids stmts) 
  = IrCaseClause c ids $ List.map (substypes mse) stmts


subetypes :: Map String Qtype -> IrExpression -> IrExpression
subetypes mse (Apply b e1 e2)
          = Apply b (subetypes mse e1) $ subetypes mse e2
subetypes mse (IrNot e)
          = IrNot $ subetypes mse e
subetypes mse (IrVar nn t)
          = IrVar nn $ subtype mse t
subetypes mse (IrExpCall  s nms ices iqes parms)
  = IrExpCall s nms (List.map (subetypes mse) ices) 
        (List.map (subetypes mse) iqes) $
         List.map (app2of2 $ subtype mse)  parms
subetypes mse (IrCons c es)
          = IrCons c $ List.map (subetypes mse) es
subetypes mse e = e

subtype ::  Map String Qtype -> Qtype -> Qtype
subtype mse t@(TYPEVAR s)
        = findWithDefault t s mse

subtype _ t = t

addNewLinearEntries :: [Nodename] -> 
                       [Qtype] -> 
                      (Nodename -> Qtype -> SymEntryLinear) -> 
                      SemStateMonad()
addNewLinearEntries names types f
    = do let entries = List.map (uncurry f) $ zip names types
         mapM_ addEntry $ zip names entries

addNewClassicalEntries :: [Int]->
                          [Nodename] -> 
                          [Qtype] -> 
                         (Int -> Nodename -> Qtype -> SymEntryClassical) -> 
                         SemStateMonad()
addNewClassicalEntries offsets names types f
    = do let entries = List.map (uncurry3 f) $ zip3 offsets names types
         mapM_ addClassicalEntry $ zip names entries


\end{code}
