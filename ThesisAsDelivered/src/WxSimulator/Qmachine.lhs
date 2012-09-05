
\begin{code}

module Main where
import Graphics.UI.WX.Attributes as WxAttributes
import Graphics.UI.WX
import Graphics.UI.WXCore
import QSM.QSM
import Data.LazyNum
import Data.Matrix
import Data.Stream
import Data.Map as Map
import InstructionWindow
import QstackWindow
import Control.Exception
import Assembler.AssemParser

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))


main :: IO ()
main 
  = do args <- getArgs
       start (gui args)
\end{code}
Set up data and type aliases.

\begin{code}


type TD = QuantumStack OurBasis LazyNum
type ListData  = [Instruction OurBasis]
type ScreenData = MachineState OurBasis LazyNum 


mState::ScreenData
mState = startMachine (fromInteger 1) noCode


gui :: [String] -> IO ()
gui files
  = do -- main gui elements: frame, panel
       f <- frame [text := "The Qstack browser" ] 
       let errorHandler = handleJust errorCalls
                             (\ err -> do print "In Error Handler"
                                          warningDialog f "Error Occurred!!!" err
                                          print "after w dialog"
                                          gui files
                             )

       errorHandler $ 
         do p <- panel f []
            vMS <- varCreate mState
\end{code}
Create a splitter window. Left side to hold the tree, right side to be
another splitter with the 
action buttons and instruction list.
\begin{code}
       
            sMain <- splitterWindow p []
            qsw <- qsWindow sMain [ qstree := quantumStack $ hd  mState, 
                                    qsdepth := 3 ]
	    sSub <- splitterWindow sMain []

            -- Button panel
            bp <- panel sSub []
            -- IW panel
            lb <- instructionWindow sSub [memDisplay := (codeMem $ hd mState)]
            when ([] /= files) $ do loadFile lb (last files) vMS
            tcstack <- staticText bp []
            stextDump <- staticText bp []
            stepCount <- spinCtrl bp 1 10 [selection := 1,
                                           text := "1"]
            streamDepth <- spinCtrl bp 0 40 [selection := 0, 
                                             text := "0"]
            treeDepth <- spinCtrl bp 1 99 [selection := 99, 
                                           text := "99"]
            cbShowTrace <- checkBox bp [checked := True]
            checkBoxOnCommand cbShowTrace (errorHandler $ 
                                           noStep cbShowTrace qsw lb streamDepth 
                                                  treeDepth tcstack stextDump vMS)
            spinCtrlOnCommand streamDepth (errorHandler $ 
                                           noStep cbShowTrace qsw lb streamDepth 
                                                  treeDepth tcstack stextDump vMS)
            spinCtrlOnCommand treeDepth (errorHandler $ 
                                         noStep cbShowTrace qsw lb streamDepth 
                                                treeDepth tcstack stextDump vMS)
      
            bStep <- button bp [text := "Step", 
			        on command := errorHandler $ 
                                   do doStep cbShowTrace qsw lb stepCount 
                                             streamDepth treeDepth
                                             tcstack stextDump vMS 
	                              resetLayout f p sMain qsw sSub bp lb]

            bReset <- button bp [text := "Reset",
                                 on command := errorHandler $
                                    do resetMachine qsw lb 
                                             streamDepth treeDepth
                                             tcstack stextDump vMS]

--            bins <- button bp [text := "Add Instructions", 
--			       on command := errorHandler $ 
--                                  do showInsDialog  p lb vMS "" mainproglabel True
--                                     resetLayout f p sMain qsw sSub bp lb
--			      ]

            bload <- button bp [text := "Load Instructions", 
			        on command := errorHandler $ 
                                   do loadMemory lb vMS
			              resetLayout f p sMain qsw sSub bp lb
			              refresh f ]
         
            bsave <- button bp [text := "Save Instructions", 
			        on command := 
                                   do ms <- varGet vMS
			              saveInstructions bp $ 
                                         codeMem $ hd ms ] 

            status <- statusField [text := "Quantum Stack Browser/debugger"]

            set bp [layout := margin 5 $ 
                    column 10  [boxed "Execution / Display" 
                                          (grid 5 5 [[widget bStep,
						      widget bReset],
						     [label "Step Count",
                                                      widget stepCount ],
						     [label "Show Trace",
                                                      widget cbShowTrace ]]),
                                boxed "View Controls" 
			         (grid 5 5 [[label "Stream Depth",
                                             widget streamDepth ],
                                            [label "Tree Depth",
                                             widget treeDepth]]),
                                 boxed "Programs" 
                                           (grid 5 5 [ 
						       [widget bload,
						        widget bsave]]),
			         boxed "Classical Stack"  $ minsize (sz 212 131)
			         (widget tcstack),
			         boxed "Top of Dump"  $ minsize (sz 212 131)
			         (widget stextDump)                                
                               ]
	           ]
            set f [layout  := container p $ margin 5 $ fill  $ 
	           vsplit sMain 5 180 (widget qsw) 
	           (vsplit sSub 5 300 (widget bp) (fill $ widget lb))
                  ,statusBar  := [status]
                  ,clientSize := sz 800 500
                  ]
            return ()
\end{code}
|doStep| will apply a single instruction to the machine and
then allow the update of the display window.
\begin{code}

resetLayout f p sMain qsw sSub bp lb 
    = do csize <- WxAttributes.get f clientSize
	 print ("client size = "++(show csize))
	 msp <- splitterWindowGetSashPosition sMain
	 print ("main sp = "++(show msp))
	 sssp <- splitterWindowGetSashPosition sSub
	 print ("sub sp = "++(show sssp))
	 set f [layout  := container p $ margin 5 $ fill  $ 
		vsplit sMain 5 msp (widget qsw) 
		(vsplit sSub 5 sssp
		 (widget bp) (fill $ widget lb))
	    ,clientSize := csize ]
	 splitterWindowSetSashPosition sMain msp True
	 splitterWindowSetSashPosition sSub sssp True

multistep :: (Quantum a b)=>Int -> 
             MachineState a b -> IO(MachineState a b)
multistep 0  ms = return ms
--    = do print "at zero step. return ..."
--         print ms
--         return ms
multistep n  ms 
    = do --print $ "stepping " ++ (show n)
         --print " applying to:"
         --print ms
         let ms' = runMachine ms
         --print $ "Applied --->" ++ (show ms')
         multistep (n-1)  ms'

doStep ::  CheckBox a ->
           QsWindow b -> 
           InsWindow c -> 
	   SpinCtrl d -> 
	   SpinCtrl e -> 
	   SpinCtrl f ->
	   StaticText g ->
	   StaticText h ->
	   Var (MachineState OurBasis LazyNum) -> 
	       IO()
doStep showtr qs lb stepcount sdepth treedepth  cstkreg stextDump vms
    = do ms <- varGet vms
         td <- WxAttributes.get treedepth selection
         scount <- WxAttributes.get stepcount selection
--         print $ "Before step - stepping ("++show scount++")"
--         print ms
         ms' <- multistep scount  ms
--         print "After step"
--	 print  ms'
	 varSet vms ms'                    
         sd <- WxAttributes.get sdepth selection
--         putStr $ case (dump $ hd ms') of
--                               [] -> "Empty Dump"
--                               d:ds -> (show d)++ " ... "
--                    print ""
         st <- WxAttributes.get showtr checked
         let mstate = hd $ dropI sd ms'
         set qs [qsdepth := td, 
                 qsShowTrace := st,
                 qstree := quantumStack mstate]
	 set lb [insList := (currIp mstate,runningCode mstate)]
         set cstkreg [text := showCstack (classicalStack mstate)]
	 set stextDump [text := showDumpTop (dump mstate)]
                    --bpan <- WxAttributes.get treg parent
                    --refit bpan
                    




noStep ::  CheckBox a -> 
           QsWindow b ->  
           InsWindow c -> 
           SpinCtrl d -> 
           SpinCtrl e ->
	   StaticText f ->
	   StaticText g ->
	   Var (MachineState OurBasis LazyNum) -> 
	       IO()
noStep showtr qs lb sdepth treedepth cstkreg stextDump vms
    = do ms <- varGet vms
        -- putStrLn "nostep Before "
        -- print $ takeI 3 $ stack ms
         sd <- WxAttributes.get sdepth selection
         td <- WxAttributes.get treedepth selection
         let mstate = hd $ dropI sd ms	            
	 set cstkreg [text := showCstack (classicalStack mstate)]
	 set lb [insList := (currIp mstate,runningCode mstate)]
         st <- WxAttributes.get showtr checked

	 --print (hd $ stack ms)
         --putStr $ case dump ms of
         --           [] -> "Empty Dump"
          --          d:ds -> (show d)++ " dstk " ++ (show $ dstack d)
         --print ""
	 set stextDump [text := showDumpTop (dump mstate)]
	 set qs [qsdepth := td, 
                 qsShowTrace := st,
                 qstree := quantumStack  mstate]




resetMachine :: QsWindow a -> 
                 InsWindow b -> 
                 SpinCtrl c -> 
	         SpinCtrl d ->
	         StaticText e ->
	         StaticText f ->
	         Var (MachineState OurBasis LazyNum) -> 
	         IO()
resetMachine qs lb sdepth treedepth cstkreg stextDump vms
    = do varSet vms mState
         set sdepth [text:="0", selection:=0]
         set treedepth  [text:="0", selection:=0]
	 set qs [qsdepth := 0, 
                 qstree := quantumStack $ hd  mState ]
	 set lb [insList :=  (currIp $ hd mState,runningCode $ hd mState),
                 memDisplay := (codeMem $ hd mState)]
	 set cstkreg [text := showCstack (classicalStack $ hd  mState)]
	 set stextDump [text := showDumpTop (dump $ hd mState)]

                    




\end{code}

Miscellaneous dialogs....

\begin{code}
filespec =  [("QPL Object File", ["*.qpo"]),("All Files",["*.*"])]

loadqpo :: Window a -> IO (Maybe FilePath)
loadqpo w = fileOpenDialog w True True "Load QPL Object File" filespec "" ""

saveqpo :: Window a -> IO (Maybe FilePath)
saveqpo w = fileSaveDialog w True True "Save QPL Instruction file" filespec
	    "" ""


loadMemory :: InsWindow a -> Var (MachineState OurBasis LazyNum) -> IO ()
loadMemory lb vMS
	   = do newInstructions <- loadInstructions lb
		ms <- varGet vMS
                let ip = (mainproglabel, 0)
                    ms' = do mstate <- ms
                             return mstate{codeMem = newInstructions, 
                                           instructionPointer=ip,
                                           runningCode = 
                                               getCode newInstructions ip }
		set lb [ memDisplay := newInstructions ]
		varSet vMS ms'

loadInstructions :: Window a -> IO (Memory OurBasis)
loadInstructions w = 
    do fp <- loadqpo w
       case fp of
	       Nothing -> return Map.empty
	       Just fpth ->
		   do inss <- readFile fpth
		      (trs,mb) <- parseQPA "" "" inss
                      print mb
                      return mb

loadFile :: InsWindow a -> String -> 
            Var (MachineState OurBasis LazyNum) -> IO ()
loadFile lb fpth vMS
    = do inss <- readFile fpth
         (trs,mb) <- parseQPA "" "" inss
         print mb
	 ms <- varGet vMS
         let ip = (mainproglabel, 0)
             ms' = do mstate <- ms
                      return mstate{codeMem = mb,
                                    instructionPointer=ip,
                                   runningCode = getCode mb ip}
	 set lb [ memDisplay := mb ]
	 varSet vMS ms'


saveInstructions :: Window a -> Memory OurBasis -> IO ()
saveInstructions w ins =
    do fp <- saveqpo w
       case fp of
          Nothing -> return ()
	  Just fpth ->
	      writeFile fpth $ show ins

\end{code}
