\begin{code}
module Instructions where
import Qtypes
import Data.Tuples
import IrTypes
import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Stack
import CompSupport(merge, enumerateName)
import InstructionNames

type CodeMonad a =  State CodeState a
type Instruction = String
type Instructions = [String]
type ProgramCode =  Map String [String]
type Label = String

data CodeState 
   = CodeState { stackNameGen :: Int, 
                 labelGen :: Int,
                 pendingDiscards :: [Nodename],
                 currentProcName :: String,
                 procStack :: Stack String,
                 delayedCode :: ProgramCode,
                 delayedCodeStack :: Stack ProgramCode,
                 currentTop :: Maybe Nodename
               }
--Level, [Int], [Int]) --Label gen, Current level, frame locals, blocklocalss

progToIns :: ProgramCode -> Instructions
progToIns = foldWithKey progFolder []

progFolder :: String -> Instructions -> Instructions -> Instructions
progFolder s i1 i2 = i1 ++ i2

combineProgs :: ProgramCode -> ProgramCode -> ProgramCode
combineProgs = Map.unionWith (++) 

combineAllProgs :: [ProgramCode] -> ProgramCode
combineAllProgs  = foldl combineProgs Map.empty

genProcHeader :: CodeMonad ProgramCode
genProcHeader = do
   cpn <- getCurrentProcName
   scode $  assem_directive_startproc cpn

genProcTrailer :: CodeMonad ProgramCode
genProcTrailer = scode assem_directive_endproc

scode :: Instruction -> CodeMonad ProgramCode
scode i = code [i]

code :: Instructions -> CodeMonad ProgramCode
code ins
     = do cpn <- getCurrentProcName
          return $ Map.singleton cpn ins



getCurrentProcName :: CodeMonad String
getCurrentProcName = gets currentProcName

popProcName :: CodeMonad String
popProcName =
    do ps <- gets procStack
       let (pname, ps') = popM ps
       case pname of
           (Just pn) -> 
               do modify (\st -> st{currentProcName = pn, 
                                    procStack = ps'})
                  return pn
           Nothing -> error "Unable to pop proc from stack"

pushProcName :: String -> CodeMonad()
pushProcName proc
    = do cpn <- getCurrentProcName
         modify (\st -> st{currentProcName = proc,
                           procStack = push cpn (procStack st)})



getStackName :: CodeMonad String
getStackName 
    = do lbl <- gets stackNameGen
         modify (\st -> st{stackNameGen = 1+lbl})
         return $ "c" ++ (show lbl)
                 
getLastGenName :: CodeMonad String
getLastGenName = 
   do ng <- gets stackNameGen
      return $ "c" ++ (show (ng - 1))

resetStackName :: CodeMonad ()
resetStackName = setStackName 0

setStackName :: Int -> CodeMonad ()
setStackName i = modify ( \ st -> st{stackNameGen=i})

getCurrentTop :: CodeMonad (Maybe Nodename)
getCurrentTop = gets currentTop

setCurrentTop :: Nodename -> CodeMonad ()
setCurrentTop nm
     = do removeFromPendingDiscards nm
          modify (\st -> st {currentTop = (Just nm)})

unsetCurrentTop :: CodeMonad ()
unsetCurrentTop = modify (\st -> st {currentTop = Nothing})

getLabel :: CodeMonad String
getLabel = 
    do lbl <- gets labelGen
       modify (\st -> st{labelGen = 1+lbl})
       return $ "lbl" ++ (show lbl)
                 
getLastLabel :: CodeMonad String
getLastLabel = 
  do lg <- gets labelGen
     return $ show lg

resetLabel :: CodeMonad ()
resetLabel = setLabel 0

setLabel :: Int -> CodeMonad ()
setLabel i = modify ( \ st -> st{labelGen=i})


getPendingDiscards :: CodeMonad [Nodename]
getPendingDiscards = gets pendingDiscards

clearPendingDiscards :: CodeMonad ()
clearPendingDiscards = modify (\st -> st{pendingDiscards = []})
                               
addToPendingDiscards :: Nodename -> CodeMonad ()
addToPendingDiscards nm 
    =modify (\st -> st{pendingDiscards = 
                           nm : (pendingDiscards st)})
 
removeFromPendingDiscards :: Nodename -> CodeMonad ()
removeFromPendingDiscards nm
   = modify (\st -> st{pendingDiscards = 
                           List.filter ( /= nm ) (pendingDiscards st)})


classicalOp :: BinOp -> CodeMonad ProgramCode
classicalOp bop = scode $ glue2 iname_coperation  (show bop)

guardBodyCode :: Label -> [(IrExpression, [Istmt])] -> 
                (IrExpression -> CodeMonad ProgramCode) ->
                (Istmt -> CodeMonad ProgramCode) ->
                CodeMonad ProgramCode
guardBodyCode _ [] _ _
    = return empty
guardBodyCode endlbl ((exp,stmts):morees) ec ssc
    = do ecode <- ec exp
         lbl <- getLabel
         cj <- condJumpCode lbl
         sc <- ssc (Iblock stmts)
         jmp <- jumpCode endlbl
         bodyRest <- guardBodyCode endlbl morees ec ssc
         bypass <- if (Map.null bodyRest) then labelizeM lbl nooperation
                   else labelize lbl bodyRest
         return $ combineProgs ecode $
                combineProgs cj $
                combineProgs sc $
                combineProgs jmp bypass
         

useStartCode :: Label -> Label -> Nodename -> CodeMonad ProgramCode
useStartCode bdyLabel endLabel nm
    = do pu <- pullup nm
         uc <- scode $ glue2 iname_use bdyLabel
         jc <- jumpCode endLabel
         return $ combineProgs pu $
                combineProgs uc jc

qend :: CodeMonad ProgramCode
qend = scode $ glue2 iname_endqc ""

destroy :: CodeMonad ProgramCode
destroy = scode $ glue2 iname_delete ""

disc  :: CodeMonad ProgramCode
disc  = scode $ glue2 iname_discard ""

delayedUse :: Nodename -> CodeMonad (ProgramCode, ProgramCode)
delayedUse nm
    = do lbl1 <- getLabel
         lbl2 <- getLabel
         pull <- pullup nm
         dc <- labelizeM lbl1 disc
         use <- code [glue2 iname_use lbl1, glue2 iname_jump lbl2]
         endq <- qend
         endit <- labelizeM  lbl2 nooperation
         return (combineProgs pull $ combineProgs use dc, 
                 combineProgs endq endit)


measCode :: CodeMonad ProgramCode -> 
            CodeMonad ProgramCode -> 
            CodeMonad ProgramCode
measCode zbr obr
    = do lbl1 <- getLabel
         lbl2 <- getLabel
         lbl3 <- getLabel
         mscd <- code [glue3 iname_measure lbl1 lbl2, glue2 iname_jump lbl3]
         zbrd <- blockCode zbr
         obrd <- blockCode obr
         disc <- scode iname_discard
         z' <-labelize lbl1  disc
         o' <- labelize lbl2  disc
         endq <- qend
         endit <- labelizeM lbl3 nooperation
         return $ combineProgs mscd $ 
                combineProgs z' $
                combineProgs zbrd $
                combineProgs endq $
                combineProgs o' $
                combineProgs obrd $
                combineProgs endq endit
         
controlDoneCode :: CodeMonad ProgramCode
controlDoneCode = do unsetCurrentTop
                     scode $ glue2 iname_popcontrol ""

controlBeginCode :: CodeMonad ProgramCode
controlBeginCode = scode $ glue2 iname_pushcontrol ""

controlStartCode :: [Nodename] -> CodeMonad ProgramCode
controlStartCode [] = error "Nothing to control by"
controlStartCode nms
    = do sc <- controlBeginCode
         fp <- mapM controlVar nms
         return $ combineProgs sc (combineAllProgs fp)

controlVar :: Nodename -> CodeMonad ProgramCode
controlVar nm = do pu <- pullup nm
                   cc <- scode $ glue2 iname_controlit ""
                   return $ combineProgs pu cc
         
nooperation :: CodeMonad ProgramCode
nooperation = scode iname_noop

zeroStackCode :: CodeMonad ProgramCode
zeroStackCode = scode $ glue2 iname_zerostack ""

returnop :: Int->CodeMonad ProgramCode
returnop n = scode $ glue2 iname_return (show n)

labelize :: String -> ProgramCode -> CodeMonad ProgramCode
labelize lbl pc =
    return $ Map.map (labelizeFirstIns lbl) pc

labelizeM :: String -> CodeMonad ProgramCode -> CodeMonad ProgramCode
labelizeM l pc = do cd <- pc
                    labelize l cd

labelizeFirstIns :: String -> Instructions -> Instructions
labelizeFirstIns lbl [] = [lbl]
labelizeFirstIns lbl (in1:ins) = ((lbl++" "++ in1) : ins)

blockCode :: CodeMonad ProgramCode -> CodeMonad ProgramCode
blockCode cpc =
    do pushDelayedCode
       cd <- cpc
       dc <- popDelayedCode
       return $ combineProgs cd dc

typeToLoadIns :: Qtype -> String
typeToLoadIns QBIT = iname_newqbit
typeToLoadIns INT = iname_newint

notInstruction :: CodeMonad ProgramCode
notInstruction =  scode $ glue2 iname_coperation "~"

qnotTos :: CodeMonad ProgramCode
qnotTos 
  = applyTransform 0 NotGate

{- May not need...
makeSubData :: Int->ConsIdentifier -> 
               CodeMonad [ProgramCode] -> 
               CodeMonad ProgramCode
makeSubData i cid subs
    = interspnms (enumerateName i cid) subs
-}
allocType :: Nodename -> ConsIdentifier -> CodeMonad ProgramCode
allocType nm cid
    = scode $ glue3 iname_newdata nm  ('#':cid)

makeInt :: Nodename -> CodeMonad ProgramCode
makeInt nm = scode $ glue2 iname_newint nm

alloc :: Qtype -> [Nodename]->CodeMonad ProgramCode
alloc _ [] = return Map.empty
alloc (TYPEVAR t) _ = error "Must fix types first"
alloc (DECLTYPE ti qts) _ = error "Need to think about this one"
alloc t (nm:nms) 
    = do allocate <- scode $ glue3 (typeToLoadIns t) nm "|0>"
         rest <- alloc t nms
         return $ combineProgs allocate rest

allocQbit :: Nodename -> Bitvalue -> CodeMonad ProgramCode
allocQbit nm qv
    = do alloccd <- alloc QBIT [nm]
         qnot <- qnotTos
         return $ if qv == Zero then alloccd
                  else combineProgs alloccd qnot

getClassicalRets :: [Int] -> CodeMonad ProgramCode
getClassicalRets [] = return empty
getClassicalRets (o:os)
    = do ret1 <- classicalPull o
         rets <- getClassicalRets os
         return $ combineProgs ret1 rets

classicalPull :: Int -> CodeMonad ProgramCode
classicalPull i
    = scode $ glue2 iname_cget $ show ( i - 1)


classicalPut :: Int -> CodeMonad ProgramCode
classicalPut i
    = scode $ glue2 iname_cput $ show ( i - 1)

popcs :: CodeMonad ProgramCode
popcs = scode $ glue2 iname_cpop "1"

cload ::  (Either Int Bool) -> CodeMonad ProgramCode
cload (Left i)
    =  scode $ glue2 iname_loadi $ show i

cload (Right b)
    =  scode $ glue2 iname_loadi $ show b

applyTransform :: Int -> UnitaryTransform -> CodeMonad ProgramCode
applyTransform sz ut 
    = scode $ glue3 iname_transform (show sz) $"!"++ (show ut)

trans :: UnitaryTransform ->  CodeMonad ProgramCode
trans t 
      =  scode $ glue2 inamepart_put (show t)

pullup :: Nodename ->
          CodeMonad ProgramCode
pullup nm 
    = do mybeNm <- getCurrentTop
         let pull = glue2 iname_pullup nm
         case mybeNm of 
           Nothing -> scode pull
           Just anme -> if (nm == anme) then return empty
                        else scode pull


discard :: Nodename -> CodeMonad ProgramCode
discard nm 
    = do pul <- pullup nm
         dc <-  disc
         return $ combineProgs pul dc



fullyDelete :: Nodename -> CodeMonad ProgramCode
fullyDelete nm 
    = do pul <- pullup nm
         dc <-  destroy
         return $ combineProgs pul dc

doRenames :: [Nodename] -> [Nodename] -> CodeMonad ProgramCode
doRenames [] _ = return Map.empty
doRenames _ [] = return Map.empty
doRenames (f:fs) (t:ts)
    = do r1 <- rename f t
         rest <- doRenames fs ts
         return $ if f == t then rest
                  else combineProgs r1 rest

rename :: Nodename ->
          Nodename -> 
          CodeMonad ProgramCode
rename oldnm nm
    = if oldnm == nm then return Map.empty
      else scode $ glue3  iname_rename oldnm nm

delayed :: CodeMonad ProgramCode -> CodeMonad ProgramCode
delayed cd
    = do pushDelayedCode
         code <- cd
         dc <- popDelayedCode
         return $ combineProgs code dc
{- -- redo with general qcontrol
cond :: CodeMonad ProgramCode -> CodeMonad ProgramCode -> 
        CodeMonad ProgramCode
cond izero ione 
    = do ic <-  iname_cond
         incIndent
         iz <- delayed izero
         sw <-  iname_swap
         incIndent
         io <- delayed ione
         pop <- iname_pop
         decIndent
         decIndent
         return $ combineProgs ic $ combineProgs iz $ 
                combineProgs sw $ combineProgs io pop

-}

{- Redo
generalCall :: String -> [Identifier] -> 
               CodeMonad [ProgramCode] -> 
               [Nodename] ->
               CodeMonad ProgramCode
generalCall nm argids expscode outids 
    = do parmcode <- interspnms argids expscode 
         cc <- delayed $ call nm
         idrenames <- zipWithM generalRename argids  outids
         return $ combineProgs parmcode $ combineProgs cc $
                 combineAllProgs idrenames
-}
call :: Int -> String -> CodeMonad ProgramCode
call cvals nm =  scode $ glue3 iname_call (show cvals) nm
{- May not need...
interspnms :: [Nodename] -> CodeMonad [ProgramCode] ->
               CodeMonad ProgramCode
interspnms nms exps 
    = do expins <- exps
         argnames <- mapM rename nms
         return $ combineAllProgs $ merge expins argnames
-}
{- redo with new qc
meas :: CodeMonad ProgramCode -> CodeMonad ProgramCode -> 
        CodeMonad ProgramCode
meas izero ione
     = do m <-  iname_measure
          c <- cond izero ione
          return $ combineProgs m c
-}

{- Loops done only by recursion
loop :: CodeMonad ProgramCode -> CodeMonad ProgramCode -> 
        CodeMonad ProgramCode
loop ex body
    = do excodeStart <- ex
         lp <-  glue2 iname_startloop "-1"
         incIndent
         b <- delayed body
         excodelp <- ex
         elp <-  iname_endloop
         decIndent
         return $ combineProgs excodeStart $ combineProgs lp $ 
                combineProgs b $ combineProgs excodelp  elp

-}

condJumpCode :: Label -> CodeMonad ProgramCode
condJumpCode lbl = scode $ glue2 iname_cjump lbl

jumpCode :: Label -> CodeMonad ProgramCode
jumpCode lbl = scode $ glue2 iname_jump lbl

splitCode :: [(ConsIdentifier,Label)] -> CodeMonad ProgramCode
splitCode nns = scode $ glue2 iname_split $ showSplitList nns

showSplitList :: [(ConsIdentifier,Label)] -> String
showSplitList [] = ""
showSplitList (nn:nns) = glue2 (showCidLblpr nn) $ showSplitList nns

showCidLblpr :: (ConsIdentifier,Label) -> String
showCidLblpr (cid,lbl) = "(#"++cid++","++lbl++")"


binds :: [Nodename] -> CodeMonad ProgramCode
binds [] = return Map.empty
binds (nn:nns)
    = do b1 <- scode $ glue2 iname_bind nn
         bns <- binds nns
         return $ combineProgs b1 bns


unbinds :: [Nodename] -> CodeMonad (ProgramCode, ProgramCode)
unbinds [] = return (Map.empty, Map.empty)
unbinds (nn:nns)
    = do (delnm, u1) <- 
             if (nn == "_") 
                 then do nm <- getStackName
                         cd <- scode $ glue2 iname_unbind nm
                         pu <- pullup nm
                         dc <- destroy
                         return (combineProgs pu dc, cd)
                 else do cd <- scode $ glue2 iname_unbind nn
                         return (Map.empty, cd)
         (delnms,uns) <- unbinds nns
         return ( combineProgs delnm delnms, 
                  combineProgs u1 uns)

caseClauses :: [IrCaseClause] ->  
                (Istmt -> CodeMonad ProgramCode)->
                CodeMonad ([(ConsIdentifier,Label)],ProgramCode)
caseClauses  [] _ 
    =  return ([],empty)


caseClauses   (cc@(IrCaseClause ci nns stms) :ccs) f
   = do lbl <- getLabel
        (discubs, ubs) <- unbinds nns
        disc <- scode iname_discard
        qend <- scode iname_endqc
        bstart <- if (Map.null ubs) then labelize lbl disc
                  else do lu <- labelize lbl ubs
                          return $ combineProgs lu disc
        b <-  f $ Iblock stms
        (clprs,otherCases) <- caseClauses ccs f
        return $ ((ci,lbl):clprs ,
                  combineProgs bstart $
                  combineProgs discubs $
                  combineProgs b $
                  combineProgs qend otherCases)

doPendingDiscards :: CodeMonad ProgramCode
doPendingDiscards 
    = do discs <- getPendingDiscards
         clearPendingDiscards
         discIns <- mapM fullyDelete discs
         return $ combineAllProgs discIns
         

addDelayedCode :: ProgramCode -> CodeMonad ()
addDelayedCode cd
    = do old <- getDelayedCode
         setDelayedCode $ combineProgs cd old
                        

getDelayedCode :: CodeMonad ProgramCode
getDelayedCode = gets delayedCode

setDelayedCode :: ProgramCode -> CodeMonad ()
setDelayedCode dc = modify (\st -> st{delayedCode = dc})

popDelayedCode :: CodeMonad ProgramCode
popDelayedCode 
    = do dc <- getDelayedCode
         dcs <- gets delayedCodeStack
         let (ndc, ndcs) = popM dcs
             newdc = case ndc of 
                  Nothing -> Map.empty
                  (Just d) -> d
         modify (\st -> st{delayedCode = newdc,
                           delayedCodeStack = ndcs})
         return dc

pushDelayedCode :: CodeMonad ()
pushDelayedCode 
    = modify (\st -> st{delayedCode = Map.empty,
                        delayedCodeStack = push (delayedCode st) 
                                           (delayedCodeStack st) 
                                              })
         

\end{code}
