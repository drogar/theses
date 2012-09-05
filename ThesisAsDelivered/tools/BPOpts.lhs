\begin{code}
    module BPOpts (
		 Flag(..)
		 , compilerOpts)     where
    

    import System.Console.GetOpt
    import System.Exit (exitWith, ExitCode(..))
    import Data.Maybe ( fromMaybe )
    import System.IO 
    import Data.List (isSuffixOf, findIndices)
    
    data Flag 
     =  HelpMe | Output String 
    	deriving (Show, Eq)

    isOutputFlag :: Flag -> Bool
    isOutputFlag (Output _) = True
    isOutputFlag _ = False

    unOutput :: Flag->Maybe String
    unOutput (Output s) = Just s
    unOutput _ = Nothing
    
    options :: [OptDescr Flag]
    options =
     [ Option ['h']     ["help"] (NoArg HelpMe)       "Print this usage info"
     ]
    
    outp :: Maybe String -> Flag
    outp = Output . fromMaybe "stdout"

    noErrhead :: [([a],b)]->[a]
    noErrhead [] = []
    noErrhead ((as,_):_) = as
    
    compilerOpts :: [String] -> IO ([Flag], [Handle])
    compilerOpts argv = 
    	case (getOpt Permute options argv) of
    	   (o,n,[]  ) -> do --print (showList argv " arguments ")
			    _ <- checkForUsage o
			    --print (showList o " flags ")
			    --print ("Files are:"++ showList n "")
                            hndls <- mapM convFileName n
			    return (o, hndls)
    	   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageheader options))

    checkForUsage :: [Flag]->IO ()
    checkForUsage opts =
	do  if (foldl (\b s-> (s==HelpMe || b)) False opts)
	       then  do hPutStr stderr (usageInfo usageheader options)
			exitWith ExitSuccess
	       else
	            return ()

    usageheader :: String
    usageheader = "Usage: extract [OPTION...] files..."

    convFileName :: String->IO Handle
    convFileName  s = 
	do inhandle <- 
               do  case s of 
	             "-" -> return stdin
		     _ -> do print s
                             openFile s ReadMode
	   return inhandle

    

    
\end{code}
