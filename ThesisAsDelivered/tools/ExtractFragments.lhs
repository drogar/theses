
To compile this:
   alex -gc Tokens.x
   happy -gca Grammar.ly
   ghc --make Main.hs -fglasgow-exts -o ghc.comp

 or
   make
\begin{code}

module Main where

import ExtractOpts
import Control.Monad.State
import Data.List as List
import Data.Char (isSpace)
import System.Environment (getArgs, getProgName)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, hPutStrLn, stderr, hGetContents, Handle(..), hClose)

main = do args <- getArgs
	  (o, hndls) <- compilerOpts args
--	  print hndls
	  mapM (doExtract o) hndls
--	  mapM_(hClose $ snd $ unZip hndls)



doExtract :: [Flag]->Handle->IO ()
doExtract flgs infile =
    do pr <- hGetContents infile
       let frags = findFragments pr
--       print frags
       mapM_ (writeFrag flgs) frags

findFragments :: String -> [[String]]
findFragments input 
   = frags 
      where rrfrags = mapAccumL accFrags [] $ lines input
            frags = reverse $ map reverse $ fst rrfrags


accFrags :: [[String]] -> String -> ([[String]],String)
accFrags xs [] = (xs,[])
accFrags xs line 
            | isFragStart line = (([line]):xs, [])
            | isInFrag xs = ( addToFrag line xs, [])
            | otherwise = (xs, [])

isFragStart ::String -> Bool
isFragStart s = "%%%%Thesis" ==  takeWhile (not . isSpace) s

isInFrag:: [[String]] -> Bool
isInFrag [] = False
isInFrag ([]:xs) = False
isInFrag xs = "%%%%endThesis" /=  ((take 13) . head . head) xs

addToFrag :: String -> [[String]] -> [[String]]
addToFrag s [] = [[s]]
addToFrag s (x:xs) = ((s:x):xs)
      
writeFrag :: [Flag]->[String] -> IO()
writeFrag oflags s
    = do let fname = head $ drop 1 $ words $ head s
             outData = removeLatex  $ drop 2 $ init s
         outfile <- outname oflags fname
         if ('+' == head fname) then
            appendFile outfile $ unlines outData
                                 else
            writeFile outfile $ unlines outData

removeLatex ::[String] -> [String]
removeLatex  = filter (\s -> '\\' /= (head s))

outname :: [Flag] -> String -> IO(String)
outname [] [] = return "Unnamed"
outname [] s | head s == '+' = return $! tail s
             | otherwise = return s
outname ((Output dir):_) s
        = do createDirectoryIfMissing True dir
             fname <- outname [] s
             return $! dir ++ "/" ++ fname
\end{code}
