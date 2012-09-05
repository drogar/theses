\incsec{Compiler driver}\label{incsec:compiler main}
To compile this:
   alex -gc Tokens.x
   happy -gca Grammar.ly
   ghc --make Main.hs -fglasgow-exts -o ghc.comp

 or
   make
\begin{code}

module Main where

import Qparser
import Qtypes
import SymbolTable
import Semantic
import GenCode
import Opts
import Control.Monad.State
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO 
import System.FilePath
import Shared.Version

main = do args <- getArgs
	  (o, spltfps) <- compilerOpts args
	  --print o
	  --print spltfps
          print version
	  mapM (doCompile o) spltfps
--	  mapM_(hClose $ snd $ unZip hndls)

{-
mainf qpltype fname genType
    = do pr <- readFile fname
	 let asyn = if BlockQPL == qpltype
		    then Left $ parseBlockQPL pr
		    else Right $ parseQPL pr
	 putStr ((show asyn)++"\n")
	 ip <- ioMakeIr asyn
	 putStr ("\nIR for the prog is \n" ++ show ip ++"\n")
         proggen <- ioGenCode ip genType
	 putStr proggen
-}

doCompile :: [Flag]->(String, String, String)->IO ()
doCompile flgs (dir, fname, fext) =
    do let infile = joinFileExt fname fext
           inpath = joinFileName dir infile
           outfile = joinFileExt fname "qpo"
           outpath =joinFileName dir outfile       
       pr <- readFile inpath
       printCond stderr EchoCode flgs $ "Input file\n"++ pr
       asyn <- parseQPL dir infile pr (getIncludePath flgs)
       printCond stderr Syntactic flgs $ show asyn
       ip <- ioMakeIr asyn
       printCond stderr IRoptPrint flgs $ show ip
       proggen <- ioGenCode ip
       print proggen
       outh <- openFile outpath WriteMode
       hPutStrLn outh compilerId
       mapM_ (hPutStrLn outh) proggen
       hClose outh

compilerId :: String
compilerId ="Compiler: " ++ version


printCond :: Handle->Flag->[Flag]->String->IO ()
printCond h f flags output =
    do if (foldl (\b s-> (s==f || b)) False flags)
	  then hPutStrLn h (output)
	  else
	       return ()

getIncludePath :: [Flag] -> [String]
getIncludePath ((SearchDirs ss):_) = ss
getIncludePath [] = ["."]
getIncludePath (_:flgs) = getIncludePath flgs

\end{code}
