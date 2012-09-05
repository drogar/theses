
To compile this:
   ghc --make Main.hs -fglasgow-exts -o bp

 or
   make
\begin{code}

module Main where

import BPOpts
import Control.Monad.State
import Data.List as List
import Data.Map as Map
import Data.Char (isSpace)
import System.Environment (getArgs, getProgName)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, hPutStrLn, stderr, hGetContents, Handle(..), hClose)

main = do args <- getArgs
	  (o, hndls) <- compilerOpts args
--	  print hndls
	  mapM (doCount o) hndls
--	  mapM_(hClose $ snd $ unZip hndls)



doCount :: [Flag]->Handle->IO ()
doCount flgs infile =
    do pr <- hGetContents infile
       let count = findparens pr
       print count

findparens :: String -> Map Char (Int,Int)
findparens  = fp' (Map.insert '{' (0,0) $
                     Map.insert '(' (0,0) $
                      Map.singleton '[' (0,0))

fp' :: Map Char (Int,Int) -> String -> Map Char (Int,Int)
fp' m [] = m
fp'  m (c:cs) = updchar c $ fp' m cs 


updchar :: Char -> Map Char (Int,Int) -> Map Char (Int,Int)
updchar '{' = update incfirst '{'
updchar '}' = update incsecond '{'
updchar '(' = update incfirst '('
updchar ')' = update incsecond '('
updchar '[' = update incfirst '['
updchar ']' = update incsecond '['
updchar _ = id     

incfirst ::  (Int,Int) -> Maybe (Int,Int)
incfirst (a,b) = Just ((a+1),b)

incsecond ::  (Int,Int) -> Maybe (Int,Int)
incsecond (a,b) = Just (a,(b+1))

\end{code}
