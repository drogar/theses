\begin{code}
module Simulator.Dialogs.VersionMismatch where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Types as Gtypes
import QSM.QSM
import Simulator.SimBase

import Data.LazyNum
import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Assembler.AssemParser
import Shared.Version



setUpVersionMismatchDialog :: forall parent.
                              (WindowClass parent) =>
		              parent -> GladeXML -> IO (Dialog)
setUpVersionMismatchDialog  qs qsXml
    = do vmDialog <- xmlGetWidget qsXml castToDialog "versionWarningDialog"
         widgetHideAll vmDialog
         return vmDialog

condshowVersionMismatchDialog :: forall self parent.
		  (DialogClass self, WindowClass parent) =>
		  parent -> 
                  GladeXML -> 
                  self -> 
                  [String] ->
                  IO (Bool)
condshowVersionMismatchDialog qs qsXml vmDialog []
    = showVmDialog qs qsXml vmDialog (Version 0 0 0 Unknown)
condshowVersionMismatchDialog qs qsXml vmDialog (cv:_)
    = do  let cversion = unVers cv
          if (cversion == currentVersion) then return True
              else showVmDialog qs qsXml vmDialog cversion




showVmDialog :: forall self parent.
                (DialogClass self, WindowClass parent) =>
		parent -> GladeXML -> 
                self -> Version ->
                IO (Bool)
showVmDialog qs qsXml vmDialog cversion
    = do windowSetTransientFor vmDialog qs
         cvText <- xmlGetWidget qsXml castToLabel "compilerVersionLabel"
         svText <- xmlGetWidget qsXml castToLabel "simulatorVersionLabel"
         cvText `labelSetText` (show cversion)
         svText `labelSetText` (show currentVersion)
         widgetShowAll vmDialog
         resp <- dialogRun vmDialog
         widgetHideAll vmDialog
         case resp of
           ResponseOk -> return True           
           _ -> return False
         

\end{code}
