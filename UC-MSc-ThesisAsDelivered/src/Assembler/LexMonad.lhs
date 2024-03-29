\begin{code}

module Assembler.LexMonad where
import QSM.Transformations
import Data.Basis
import Data.Map as Map
import Data.List as List
import QSM.Instructions
\end{code}
 The input type
\begin{code}


type AlexInput = ([AlexPosn],     -- current position,
                  Char,         -- previous char
                  [String])       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexInputString :: AlexInput -> String
alexInputString (p,c,(s:ss)) = s
                

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar ((p:ps),c,[]) = Nothing
alexGetChar ((p:ps),c,[[]]) = Nothing
alexGetChar ((p:ps),_,(([]):ss))  = 
     alexGetChar (ps,' ',ss)
alexGetChar ((p:ps),_,((c:s):ss))  = 
    let p' = alexMove p c in p' `seq`
         Just (c, ((p':ps), c, (s:ss)))
\end{code}

 Token positions

`Posn' records the location of a token in the input text.  It has three
fields: the address (number of chacaters preceding the token), line number
and column of a token within the file. `start_pos' gives the position of the
start of the file and `eof_pos' a standard encoding for the end of file.
`move_pos' calculates the new position after traversing a given character,
assuming the usual eight character tab stops.
\begin{code}

data AlexPosn = AlexPn !String !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn "" 0 1 1

alexLineNum :: AlexPosn -> Int
alexLineNum (AlexPn _ _ l _) = l

alexCharNum :: AlexPosn -> Int
alexCharNum (AlexPn _ _ _ c) = c

alexCurrFile ::AlexPosn -> String
alexCurrFile (AlexPn f _ _ _) = f

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn f a l c) '\t' = AlexPn f (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn f a l c) '\n' = AlexPn f (a+1) (l+1)   1
alexMove (AlexPn f a l c) _    = AlexPn f (a+1)  l     (c+1)
\end{code}
The Monad type
\begin{code}

data AlexState = AlexState {
        alex_pos :: [AlexPosn],  -- position at current input location
        alex_inp :: [String],     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int,       -- the current startcode
        alex_clvl :: !Int,    -- BGG The comment level depth
        alex_cdir :: !String, -- The current directory - of input file.
        alex_imps :: [String] -- List of imported / read files.
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> IO (Either String a)
runAlex input (Alex f) 
   =  do ares <- f (AlexState {alex_pos = [alexStartPos],
                               alex_inp = [input],       
                               alex_chr = '\n',
                               alex_scd = 0,
                               alex_clvl = 0,
                               alex_cdir = ".",
                               alex_imps = []}) 
         return $ case ares of 
                    Left msg -> Left msg
                    Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> 
                                  IO(Either String (AlexState, a)) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> 
                 do ua <- unAlex m s
                    case ua of 
                            Left msg -> return $ Left msg
                            Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> return $ Right (s,a)

liftio::IO a -> Alex a
liftio f = Alex $ \s ->
                do x<-f
                   return (Right (s,x))
                   

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
        return $ Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp,alex_imps=List.map alexCurrFile pos} of
                  s@(AlexState{}) -> return $ Right (s, ())

alexAddInput :: AlexInput -> Alex ()
alexAddInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos ++(alex_pos s),
                            alex_chr=c,
                            alex_inp=inp++(alex_inp s),
                            alex_imps = (List.map alexCurrFile pos)++(alex_imps s)} of
                  s@(AlexState{}) -> return $ Right (s, ())

--alexEOF :: Alex a

alexError :: String -> Alex a
alexError message = 
    Alex $ \s -> return $
                   Left (" Lexing Character "++
                         (show $ head $ head $ alex_inp s)++ 
                         " at line " ++
                         (show $ alexLineNum $ head $ alex_pos s) ++ 
                         " column " ++
                         (show $ alexCharNum $ head $ alex_pos s) ++  
                         " file " ++
                         (show $ alexCurrFile $ head $ alex_pos s) ++ 
                         " context: " ++
                         (take 30 $ head $ alex_inp s) ++
                         "\n" ++ message)

alexGetInpDir :: Alex String
alexGetInpDir =  Alex $ \s@AlexState{alex_cdir=d} -> 
                          return $ Right (s, d)

alexGetImpFiles :: Alex [String]
alexGetImpFiles  = Alex $ \s@AlexState{alex_imps=imps} -> 
                       return $ Right (s, imps)

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> 
                       return $ Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> return $ Right (s{alex_scd=sc}, ())


alexGetCommentLevel :: Alex Int
alexGetCommentLevel = Alex $ \s@AlexState{alex_clvl=cl} -> return $ Right (s, cl)

alexSetCommentLevel :: Int -> Alex ()
alexSetCommentLevel cl = Alex $ \s -> return $ Right (s{alex_clvl=cl}, ())

alexIncCommentLevel :: Alex ()
alexIncCommentLevel  =
    do cl <- alexGetCommentLevel
       alexSetCommentLevel (cl + 1)

alexDecCommentLevel :: Alex ()
alexDecCommentLevel = 
    do {  cl <- alexGetCommentLevel
        ; if (cl > 0) 
          then alexSetCommentLevel (cl - 1)
          else alexSetCommentLevel 0 }





\end{code}
Useful token actions
\begin{code}

getGate :: String -> UnitaryOp
getGate "Had" = Hadamard
getGate "Not" = NotGate
getGate "T" = Tgate
getGate "Phase" = Phase
getGate "RhoX" = RhoX
getGate "Swap" = Swap
getGate "Toffoli3" = Toffoli
getGate "Rot" = Rotate
getGate "RhoY" = RhoY
getGate "RhoZ" = RhoZ
getGate "UM" = UM
getGate ('C':gate) = Controlled $ getGate gate
getGate ('I':gate) = Inverse $ getGate gate
getGate ('(':n:')':gate) = DefinedOp gate (read $ n:[])
getGate gate = DefinedOp gate 0

data Token 
     =  TkKet String  
     | TkNumber Int
     | TkSymbol String
     | TkBool Bool
     | TkOperator String 
     | TkOpcode String
     | TkStart
     | TkEnd
     | TkLabel String
     | TkTrans 
     | TkTransform UnitaryOp
     | TkCons String 
     | TkCompilerNotes String
     | TkErr | TkEof 
       deriving (Eq,Read,Show)


setlabels :: (Basis a) => [(Maybe String, Instruction a, Maybe [String])] -> 
             [Instruction a]
setlabels sis = ins
    where (lblmap, inslbls) = findlabelspositions sis
          ins = mergelabels lblmap inslbls 


findlabelspositions :: (Basis a) => 
                       [(Maybe String, Instruction a, Maybe [String])] -> 
                       (Map String Int, [(Instruction a, Maybe [String])])
findlabelspositions raw = flp' 0 [] Map.empty raw

flp' :: (Basis a) => Int-> 
        [(Instruction a, Maybe [String])] ->
        Map String Int ->
       [(Maybe String, Instruction a, Maybe [String])] -> 
       (Map String Int, [(Instruction a, Maybe [String])])
flp' n acclist accmap [] = (accmap, reverse acclist)
flp' n acclist accmap ((Nothing, i , mls):raw )
       = flp' (n+1) ((i,mls):acclist) accmap raw
flp' n acclist accmap ( (Just l, i , mls):raw )
       = flp' (n+1) ((i,mls):acclist) (Map.insert l n accmap) raw

					   
mergelabels :: (Basis a) => Map String Int -> 
               [(Instruction a , Maybe [String])] -> 
              [Instruction a]
mergelabels _ [] = []
mergelabels lm ((i,Nothing):ins) = i:mergelabels lm ins
mergelabels lm ((i,Just l):ins) 
     = (setlabel (List.map 
           (flip (findWithDefault 0) lm) l ) i):mergelabels lm ins

setlabel :: (Basis a) => [Int] -> Instruction a -> Instruction a
setlabel (l:_) (Jump _) = Jump l
setlabel (l:_) (CondJump _) = CondJump l
setlabel (l:_) (Use _) = Use l
setlabel (l1:l2:_) (Measure _ _) = Measure l1 l2
setlabel ls (Split consls) = Split $ zip (fst $ unzip consls) ls
setlabel _ i = i

ketToBasis :: String -> OurBasis
ketToBasis "1" = One
ketToBasis _ = Zero


\end{code}

