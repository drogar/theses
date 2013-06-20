\begin{code}
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Types as Gtypes
import QSM.QSM
import Simulator.SimBase
import Simulator.InstructionWindow
import Simulator.QsWindow
import Simulator.Dialogs.Preferences
import Simulator.Dialogs.VersionMismatch
import Simulator.Dialogs.SimulateResults

import Data.LazyNum
import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Assembler.AssemParser



mState::MachineState OurBasis LazyNum
mState = startMachine (fromInteger 1) noCode

-- $ 
--         Map.insert "sub1" isub $
--         Map.singleton "main" iset


main :: IO ()
  = do initGUI
       ms <- newIORef mState
       qsPreferences <- newIORef defaultControls
       qsXmlM <- xmlNew "qstack.glade"
       qsXML2 <- xmlNew "Simulator/qstack.glade"
--       ioPmap <- newIORef Nothing
       let qsXml =
             case qsXmlM of
	       (Just qsXml) -> qsXml
	       Nothing ->
                       case qsXML2 of
                         (Just q2) -> q2
                         Nothing ->  
                             error "can't find the glade file \"qstack.glade\" in the current directory"
                 
                 
       qs <- xmlGetWidget qsXml  castToWindow "qsMainWindow"
       vmDialog <- qs `setUpVersionMismatchDialog` qsXml
       qscroller <- xmlGetWidget qsXml castToScrolledWindow "qswScrolling"
       qsArea <- xmlGetWidget qsXml castToDrawingArea "qswArea"
       messageDialog <- xmlGetWidget qsXml castToDialog "shortMessageDialog"
       mdMessage <-  xmlGetWidget qsXml castToLabel "dialogMessage"
       simDialog <- setUpSimulateResultsDialog qs qsXml
         
--       qswImage <- xmlGetWidget qsXml castToImage "qswImage"
--       imageClear qswImage
--       qswImage `onRealize`  do
--                print "image event"
--                dw <- widgetGetParentWindow qswImage
--                pmap <- pixmapNew (Just dw) 1000 1000 Nothing
--                print "Created new pixmap"
--                writeIORef ioPmap (Just pmap)
--                set qswImage [imagePixmap := pmap]
--                return ()
--       qswImage `imageSetFromFile` "nofile.png"
--       dw <- widgetGetParentWindow qswImage
--       pmap <- pixmapNew (Nothing::(Maybe DrawWindow)) 1000 1000 (Just 24)
--       pmap <- pixmapNew (Just dw) 1000 1000 (Nothing)
       instructionTabs <- xmlGetWidget qsXml castToNotebook "instructionLists"
       stepSpin <- xmlGetWidget qsXml castToSpinButton "stepCountSpinButton"
       showTraceCB <-xmlGetWidget qsXml castToCheckButton "showTrace"
       showTraceCB `toggleButtonSetActive` (showTrace defaultControls)
       sdSpin <- xmlGetWidget qsXml castToSpinButton "streamDepthSpin"
       tdSpin <- xmlGetWidget qsXml castToSpinButton "treeDepthSpin"
       cstackLabel <- xmlGetWidget qsXml castToLabel "classicalStackLabel"
       dumpTextView <- xmlGetWidget qsXml castToTextView "dumpTextView"
       stepButton <- xmlGetWidget qsXml castToButton "stepButton"
       stepButton `onClicked` step stepSpin qsPreferences ms qscroller qsArea 
                  instructionTabs sdSpin  tdSpin cstackLabel dumpTextView
       goButton <- xmlGetWidget qsXml castToButton "goButton"
       goButton `onClicked` execute qsPreferences ms qscroller qsArea instructionTabs 
                sdSpin  tdSpin cstackLabel dumpTextView

       qsArea `onExpose` exposeQstack qsPreferences ms  qsArea sdSpin  tdSpin
       sdSpin `onValueSpinned` nostep qsPreferences ms qscroller qsArea instructionTabs 
                         sdSpin  tdSpin cstackLabel dumpTextView
       tdSpin `onValueSpinned` nostep qsPreferences ms qscroller qsArea instructionTabs 
                         sdSpin  tdSpin cstackLabel dumpTextView
       showTraceCB `onToggled` do
                  --print "Toggling"
                  val <- toggleButtonGetActive showTraceCB
                  modifyIORef  qsPreferences (\ x -> x{showTrace = val})
                  nostep qsPreferences ms qscroller qsArea instructionTabs 
                         sdSpin  tdSpin cstackLabel dumpTextView
       updateInsTabs ms instructionTabs sdSpin True
       qs `onDestroy` mainQuit
	 
	 -- show everything
       qsAbout  <- xmlGetWidget qsXml  castToAboutDialog "aboutdialog"
       simMenuItem <- xmlGetWidget qsXml castToMenuItem "simulate1"
       simMenuItem `onActivateLeaf` do
                 mstate <- readIORef ms
                 sdepth <- spinButtonGetValueAsInt sdSpin
                 let qstk = quantumStack $ pickIthMS sdepth mstate
                 showSimulateResultsDialog qs qsXml simDialog qstk
  -- the open menu item, opens a file dialog and then loads and displays
  -- the the profile (unless the user cancleled the dialog)
       openMenuItem <- xmlGetWidget qsXml castToMenuItem "open"
       openMenuItem `onActivateLeaf` do
              filename <- openFileDialog qs
              when (isJust filename)
                 (do inss <- readFile $ fromJust filename
                     parseassem <- parseQPA "" "" inss
                     case parseassem of
                        Left error -> do mdMessage `labelSetText` error
                                         messageDialog `afterResponse` (\_ -> widgetHideAll messageDialog)
                                         widgetShowAll messageDialog
                        Right ((cnotes,trs),mb) -> do
                                cont <- condshowVersionMismatchDialog qs qsXml vmDialog cnotes 
                                when cont $ do
	                           writeIORef ms $ startMachine (fromInteger 1) mb
                                   updateInsTabs ms instructionTabs sdSpin True
                                   nostep qsPreferences ms qscroller qsArea instructionTabs 
                                          sdSpin  tdSpin cstackLabel dumpTextView
                                   print "Loading - did noStep")


       quitMenuItem <- xmlGetWidget qsXml castToMenuItem "quit"
       quitMenuItem `onActivateLeaf` mainQuit
       
       aboutMenuItem <- xmlGetWidget qsXml castToMenuItem "about"
       aboutMenuItem `onActivateLeaf` showAboutDialog qs qsAbout
       setUpPreferences qs qsXml qsPreferences qsArea

       widgetHideAll qsAbout
       widgetHideAll messageDialog

       widgetShowAll qs
       mainGUI

-- display a standard file open dialog
openFileDialog :: Window -> IO (Maybe String)
openFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
              (Just "Open QPO file... ")
              (Just parentWindow)
	      FileChooserActionOpen
	      [("gtk-cancel", ResponseCancel)
	      ,("gtk-open", ResponseAccept)]
  qpoFilter <- fileFilterNew
  qpoFilter `fileFilterAddPattern` "*.qpo"
  qpoFilter `fileFilterSetName` "QSM assembly files (*.qpo)"
  dialog `fileChooserAddFilter` qpoFilter
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  case response of
      ResponseAccept -> fileChooserGetFilename dialog
      _ -> return Nothing


execute ::  IORef QsWindowPrefs ->
            IORef (MachineState OurBasis LazyNum) ->
            ScrolledWindow ->
            DrawingArea ->
            Notebook ->
            SpinButton ->
            SpinButton ->
            Gtypes.Label ->
            TextView ->
            IO ()

execute qsprefs  ms  qscroller qsarea instructionTabs sdSpin  tdSpin cstackLbl dumptv
  = do depth <- spinButtonGetValueAsInt sdSpin
       modifyIORef ms (go depth)
       --print "executing"
       refreshSubs qsprefs ms qscroller qsarea instructionTabs 
                  sdSpin tdSpin  cstackLbl dumptv

step ::SpinButton ->
       IORef QsWindowPrefs ->
       IORef (MachineState OurBasis LazyNum) ->
       ScrolledWindow ->
       DrawingArea ->
       Notebook ->
       SpinButton ->
       SpinButton ->
       Gtypes.Label ->
       TextView ->
       IO ()

step stepCountsb qsprefs  ms  qscroller qsarea instructionTabs sdSpin  tdSpin cstackLbl dumptv
  = do count <- spinButtonGetValueAsInt stepCountsb
       ms' <- readIORef ms
       --print $ hd ms'
       runIt count ms
       --print "Ran, look again"
       ms'' <- readIORef ms
       --print $ hd ms''
       refreshSubs qsprefs ms qscroller qsarea instructionTabs 
                  sdSpin tdSpin  cstackLbl dumptv

runIt :: Int -> 
         IORef (MachineState OurBasis LazyNum) ->
         IO()
runIt 0 ms = return ()
runIt n ms 
      = do modifyIORef ms runMachine
           runIt (n-1) ms

nostep ::IORef QsWindowPrefs ->
       IORef (MachineState OurBasis LazyNum) ->
       ScrolledWindow ->
       DrawingArea ->
       Notebook ->
       SpinButton ->
       SpinButton ->
       Gtypes.Label ->
       TextView ->
       IO ()

nostep = refreshSubs

refreshSubs ::IORef QsWindowPrefs -> 
	    IORef (MachineState OurBasis LazyNum) -> 
            ScrolledWindow ->
            DrawingArea ->
            Notebook -> 
	    SpinButton -> 
	    SpinButton -> 
            Gtypes.Label ->
            TextView ->
	    IO ()

refreshSubs qsprefs cms qscroller da instructionTabs sdSpin tdSpin cstackLbl dumptv
    = do  --dw <- drawingAreaGetDrawWindow da
          --(w,h) <- windowGetSize qscroller
          widgetQueueDrawArea qscroller  0 0 1500 1000
          updateInsTabs cms instructionTabs sdSpin False
          setDumpOut cms sdSpin dumptv
          setCstackOut cms sdSpin cstackLbl

setDumpOut cms sdSpin dumptv
   = do mstate <- readIORef cms
        sdepth <- spinButtonGetValueAsInt sdSpin
        dtb <- textViewGetBuffer dumptv
        dtb `textBufferSetText` (showDumpTop $ dump $  pickIthMS  sdepth mstate)
        dumptv `textViewSetBuffer` dtb

setCstackOut ms sdSpin cslbl
   = do mstate <- readIORef ms
        sdepth <- spinButtonGetValueAsInt sdSpin
        cslbl `labelSetText` (showCstack (classicalStack $ pickIthMS sdepth mstate))

       
\end{code}
