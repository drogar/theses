\begin{code}
module Simulator.Dialogs.SimulateResults where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Types as Gtypes
import QSM.QSM
import QSM.Simulate
import Simulator.SimBase

import Data.LazyNum
import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)





setUpSimulateResultsDialog :: forall parent.
                              (WindowClass parent) =>
		              parent -> GladeXML -> IO (Dialog)
setUpSimulateResultsDialog  qs qsXml
    = do simDialog <- xmlGetWidget qsXml castToDialog "simulateDialog"
         widgetHideAll simDialog
         return simDialog

showSimulateResultsDialog :: forall self parent.
		  (DialogClass self, WindowClass parent) =>
		  parent -> 
                  GladeXML -> 
                  self -> 
                  QuantumStack OurBasis LazyNum ->
                  IO ()
showSimulateResultsDialog qs qsXml simDialog qstack
    = do (f,vals) <- chooseIt (canonicalize qstack)
         windowSetTransientFor simDialog qs
         rvText <- xmlGetWidget qsXml castToLabel "randomValText"
         resText <- xmlGetWidget qsXml castToLabel "resultsText"
         rvText `labelSetText` (show f)
         resText `labelSetText` (show vals)
         widgetShowAll simDialog
         resp <- dialogRun simDialog
         widgetHideAll simDialog
         case resp of
           ResponseClose -> return ()
           _ -> showSimulateResultsDialog qs qsXml simDialog qstack
         

\end{code}
