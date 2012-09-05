\begin{code}
module GenCode where
import Instructions
import IrTypes
import Qtypes
import Data.Tuples
import Control.Monad.State
import Data.Map as Map
import Data.Char(toLower)
import Data.List as List
import Data.Stack
import CompSupport(mkflabel, enumerateName) 

class GenCode a where
 genCode::a->CodeMonad ProgramCode
 genCodeList::[a]->CodeMonad ProgramCode
 genCodeList [] = return Map.empty
 genCodeList (a:as) = do x<- genCode a
		         xs<- genCodeList as
			 return $ combineProgs x xs

procToCode :: ProgramCode -> (Identifier, Iproc) ->
              CodeMonad ProgramCode
procToCode ins (id,proc) = 
    do gp <- genCode proc
       return $ Map.union ins gp

instance GenCode Iprog where
 genCode (Iprog mp) 
    = foldM procToCode Map.empty $ Map.toList mp


instance GenCode Iproc where
 genCode (Iproc nm lbl _ _ _ crets stmts) 
  = do pushProcName $ mkflabel nm lbl
       hcd <- genProcHeader
       cd <- genCode $ Iblock stmts
       creturn <- getClassicalRets $ fst $ unzip crets
       ret <- returnop (length crets)
       tcode <- genProcTrailer
       popProcName
       return $ combineProgs hcd $ 
              combineProgs cd $
              if nm == "main" then tcode
                 else combineProgs creturn $
                      combineProgs ret tcode



instance GenCode Istmt where

 genCodeList [] = return Map.empty
 genCodeList (a:as) = do x<- genCode a
                         doPendingDiscards
		         xs<- genCodeList as
			 return $ combineProgs x xs

 genCode (IClassicalAssign nm off  expr) 
     = do ecode <- genCode expr
          tocstk <- classicalPut off
          return $ combineProgs ecode  tocstk

 genCode (Iassign nm (Left expr)) --Int
     = do ecode <- genCode expr
          tostk <- makeInt nm
          setCurrentTop nm
          return $ combineProgs ecode  tostk

 genCode (Iassign nm (Right expr@(IrQbit qv)))
     = do setCurrentTop nm
          allocQbit nm qv

 genCode (Iassign nm (Right expr@(IrExpCall _ frmlids _ _ _ )))
     = do ecode <- genCode expr
          let oldnm = last $ snd $ snd frmlids
          rn <- rename oldnm nm
          setCurrentTop nm
          return $ if oldnm == nm then ecode
                   else combineProgs ecode  rn

 genCode (Iassign nm (Right expr))
     = do ecode <- genCode expr
          oldnm <- getCurrentTop
          case oldnm of 
             Just oldname ->
                 do rn <- rename oldname nm
                    setCurrentTop nm
                    return $ combineProgs ecode  rn
             Nothing ->
                 fail "Machine exception: Unable to determine top of qstack for assignment"

 genCode (IuseAssign nm (Right exp)) -- todo - see IUse
     = do icode <- genCode (Iassign nm (Right exp))
          unsetCurrentTop
          (startuse,enduse) <- delayedUse nm
          addDelayedCode enduse
          return $ combineProgs icode  startuse 

 genCode (IuseAssign nm (Left exp)) -- todo - see IUse
     = do snm <- getStackName
          ecode <- genCode (Iassign snm (Left exp))
          unsetCurrentTop
          (startuse,enduse) <- delayedUse nm
          addDelayedCode enduse
          return $ combineProgs ecode  startuse 
\end{code}
The new Iuse has expression to eval. The general structure
of the generated code will now be:
\begin{itemize}
\item{} expressionCode
\item{} use lbl
\item{} jump lbl2
\item{} lbl: Qdiscard
\item{} e1eval
\item{} CondJump lbla
\item{} stmts1
\item{} Jump lbl
\item{} lbla: ... Repeat last 5 items for each group of statements.
\item{} lbl: endQC
\item{} lbl2 : next....
\end{itemize}
\begin{code}

 genCode (Iuse [] _) = error "Internal Compiler error - use with no ids"

 genCode (Iuse [nm] stmts)
     = do usebdy <- genCode $ Iblock stmts
          lbl <- getLabel
          endlbl <- getLabel
          qendc <-  qend 
          useStrt <- useStartCode lbl endlbl nm
          bodystart <- labelizeM lbl disc
          endit <- labelizeM endlbl nooperation
          return $ combineProgs useStrt $
                 combineProgs bodystart $
                 combineProgs usebdy $
                 combineProgs qendc endit          


 genCode (Iuse (nm:nms) stmts)
     = do innerUse <- genCode (Iuse nms stmts)
          lbl <- getLabel
          lbl2 <- getLabel
          useStrt <- useStartCode lbl lbl2 nm
          qendc <-  qend 
          bodystart <- labelizeM lbl disc
          endit <- labelizeM lbl2 nooperation
          return $ combineProgs useStrt $
                 combineProgs bodystart $
                 combineProgs innerUse $
                 combineProgs qendc endit          

 genCode (Iguard condstmts)
     = do endlbl <- getLabel
          usebdy <- guardBodyCode endlbl condstmts 
                    (genCode :: IrExpression -> CodeMonad ProgramCode)
                    (genCode :: Istmt -> CodeMonad ProgramCode)
          endit <- labelizeM endlbl nooperation
          return $ combineProgs usebdy endit          

 genCode (Imeas e s1 s2)
     = do ecode <- genCode e
          meascode <- measCode (genCode (Iblock s1)) (genCode (Iblock s2))
          return $ combineProgs ecode  meascode

 genCode (Icase e clauses)
     = do ec <- genCode e
-- TODO - ensure the pending discards does not cause trouble here.
          (cidLblprs,cc) <- caseClauses clauses 
                               (genCode :: Istmt -> CodeMonad ProgramCode)
          lbl <- getLabel
          spl <- splitCode cidLblprs
          jmp <- jumpCode lbl
          endit <- labelizeM lbl nooperation
          return $ combineProgs ec $
                 combineProgs spl $
                 combineProgs jmp $
                 combineProgs cc endit

\end{code}
For |Icall| we need to first calculate any classical expressions
first, in the order presented. This is follwed by the 
quantum expression in \emph{reverse} order so that the first 
expression is on top. The names of each of these expressions must
then be saved as the outputs must be renamed to the output ids. 
This is follwed by the actual application of
the transform. Finally, the outputs are renamed.

\begin{code}

 genCode (Icall (Just ut) nm frmlids inCexps inQexps outQids _) --ut no out classical ids
    = case ut of
         (Ident _) -> return Map.empty
         _ -> do eCcode <- mapM genCode $ reverse inCexps
                 nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
                 let (names,eQcode) = unzip nmsAndEQcode
                 aptran <- applyTransform (length inCexps) ut
                 renames <- doRenames (reverse names) $ List.map fst outQids
                 setCurrentTop $ fst $ head outQids
                 mapM_ removeFromPendingDiscards $ List.map fst outQids
                 return $ combineProgs (combineAllProgs eCcode) $ 
                         combineProgs (combineAllProgs eQcode) $ 
                         combineProgs aptran renames


 genCode (Icall (Nothing) nm frmlids inCexps inQexps outQids outCids)
     = do eCcode <- mapM genCode $ reverse inCexps
          nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
          let (names,eQcode) = unzip nmsAndEQcode
          renamesto <- doRenames (reverse names) $ fst $ snd frmlids
          callcd <- call (length inCexps) nm
          renamesfr <- doRenames (snd $ snd frmlids) $ List.map fst outQids
          unsetCurrentTop
          mapM_ removeFromPendingDiscards $ List.map fst outQids
          return $ combineProgs (combineAllProgs eCcode) $
                 combineProgs (combineAllProgs eQcode) $
                 combineProgs renamesto $
                 combineProgs callcd renamesfr


 genCode (Idiscard []) = return Map.empty
 genCode (Idiscard (nm:nms))
     = do dcode <- fullyDelete  nm
          dcodes <- genCode (Idiscard nms)
          unsetCurrentTop
          return $ combineProgs dcode dcodes

 genCode (Ialloc nn  typ)
     = do acd <- alloc typ [nn]
          setCurrentTop nn
          return acd

 genCode (Iblock stmts)
     = do pushDelayedCode
          stms <- genCodeList stmts
          dc <- popDelayedCode
          return $ combineProgs stms dc

 genCode (IcontrolledBy stmts ids)
     = do cstart <- controlStartCode ids
          stms <- genCode $ Iblock stmts
          cend <- controlDoneCode
          return $ combineProgs cstart $
                 combineProgs stms cend

 genCode IzeroStack
     = zeroStackCode

 genCode Iskip
     = return Map.empty

 
quantAndName' :: IrExpression -> CodeMonad (Nodename, ProgramCode)
quantAndName' e
    = do exp <- genCode e
         nm <- getStackName
         tostk <- makeInt nm
         setCurrentTop nm
         return (nm, 
                 combineProgs exp tostk) 
 
quantifyAndName :: IrExpression -> CodeMonad (Nodename, ProgramCode)
quantifyAndName  e@(Apply _ _ _)
    = quantAndName' e

quantifyAndName  e@(IrBool _)
    = quantAndName' e
quantifyAndName  e@(IrNum _)
    = quantAndName' e
quantifyAndName  e@(IrCvar _ _ _ _)
    = quantAndName' e
quantifyAndName  e
    = codeAndName e
 
codeAndName :: IrExpression -> CodeMonad (Nodename, ProgramCode)
codeAndName e@(IrVar nm _)
    = do exp <- genCode e
         setCurrentTop nm
         return (nm, exp)
codeAndName e@(IrExpCall nm frmlids inCexps inQexps outQids)
    = do exp <- genCode e
         let topnm = last $ snd $ snd frmlids
         setCurrentTop topnm
         return (topnm, exp)
codeAndName e
    = do exp <- genCode e
         lnm <- getCurrentTop
         case lnm of
             Nothing -> do lname <- getLastGenName
                           return (lname,exp)
             Just lname -> return (lname,exp)

instance GenCode IrExpression where
 genCode (Apply binop e1 e2)
     = do ex2 <- genCode e2 -- So second op is second on stack.
          ex1 <- genCode e1
          b <- genCode binop
          return $ combineProgs ex2 $ 
                 combineProgs ex1 b
 
 genCode (IrBool b)
     = cload $ Right b

 genCode (IrNot exp)
     = do ex <- genCode exp
          nt <- notInstruction
          return $ combineProgs ex nt

 genCode (IrVar nm t)
     = do addToPendingDiscards nm
          plnm <- pullup nm
          setCurrentTop nm
          return plnm
                 

 genCode (IrCvar nn off l t)
     = classicalPull off

\end{code}
Can an expcall have classical parms?
\begin{code}

 genCode (IrExpCall nm frmlids inCexps inQexps outQids)
     = do eCcode <- mapM genCode inCexps
          nmsAndEQcode <-  mapM quantifyAndName $ reverse inQexps
          let (names,eQcode) = unzip nmsAndEQcode
          renamesto <- doRenames (reverse names) $ fst $ snd frmlids
          callcd <- call (length inCexps) nm
          renamesfr <- doRenames (snd $ snd frmlids) $ List.map fst outQids
          mapM_ removeFromPendingDiscards $ List.map fst outQids
          return $ combineProgs  (combineAllProgs eCcode) $
                 combineProgs  (combineAllProgs eQcode) $
                 combineProgs renamesto $
                 combineProgs callcd renamesfr

 genCode (IrQbit bv)
     = do nm <- getStackName
          setCurrentTop nm
          allocQbit nm bv

 genCode (IrNum num)
     = cload $ Left num

 genCode (IrCons cid exps)
     = do nmsAndEcode <-  mapM quantifyAndName exps
          let (names,ecode) = unzip nmsAndEcode
          name <- getStackName
          atyp <- allocType name cid
          bnds <- binds $ reverse names
          setCurrentTop name
          return $ combineProgs (combineAllProgs ecode) $
                 combineProgs atyp bnds

instance GenCode BinOp where
    genCode op = classicalOp op

       
ioGenCode :: Iprog -> IO Instructions
ioGenCode qprog
    = do let pc = fst $ runState (genCode qprog) 
		    (CodeState 0 0  [] "" 
                     emptyStack Map.empty emptyStack
                    Nothing )
         return $ progToIns pc

\end{code}
