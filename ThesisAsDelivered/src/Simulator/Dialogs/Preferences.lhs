\begin{code}
module Simulator.Dialogs.Preferences where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Types as Gtypes
import QSM.QSM
import Simulator.SimBase
import Simulator.InstructionWindow

import Data.LazyNum
import Data.Map as Map
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Assembler.AssemParser

setPreferenceLayouts :: IORef QsWindowPrefs ->
                         DrawingArea -> IO()
setPreferenceLayouts qsPrefRef da
    = do qsPref <- readIORef qsPrefRef
         pcbl <- widgetCreatePangoContext da
         fdbl <- fontDescriptionNew
         fdbl `fontDescriptionSetFamily` fontName qsPref
         fdbl `fontDescriptionSetSize` (fromInteger $ toInteger $ branchLabelSize qsPref)
         eng <- languageFromString "en"
         pcbl `contextSetLanguage` eng
         pcbl `contextSetFontDescription` fdbl
         layoutbl <- widgetCreateLayout da ""
         layoutbl `layoutSetFontDescription` (Just fdbl)
         fdel <- copyTheFont da fdbl (fromInteger $ toInteger $ elisionSize qsPref)
         layoutel <- widgetCreateLayout da ""
         layoutel `layoutSetFontDescription`  (Just fdel)
         fdtr <- copyTheFont  da fdbl (fromInteger $ toInteger $ traceSize qsPref)
         layouttr <- widgetCreateLayout da ""
         layouttr `layoutSetFontDescription`  (Just fdtr)
         fdnm <- copyTheFont da fdbl (fromInteger $ toInteger $ nameSize qsPref)
         layoutnm <- widgetCreateLayout da ""
         layoutnm `layoutSetFontDescription`  (Just fdnm)
         fdlf <- copyTheFont da fdbl (fromInteger $ toInteger $ leafSize qsPref)
         layoutlf <- widgetCreateLayout da ""
         layoutlf `layoutSetFontDescription`  (Just fdlf)
         writeIORef qsPrefRef (qsPref{branchLabelLayout = Just layoutbl,
                                      elisionLayout = Just layoutel,
                                      traceLayout = Just layouttr,
                                      nameLayout = Just layoutnm,
                                      leafLayout = Just layoutlf})

copyTheFont da fd psize 
  = do pc <- widgetCreatePangoContext da
       eng <- languageFromString "en"
       fd' <- fontDescriptionCopy fd
       fd' `fontDescriptionSetSize` psize
       pc  `contextSetLanguage` eng
       pc `contextSetFontDescription` fd'
       return fd'

setUpPreferences :: forall parent.
		    (WindowClass parent) =>
		    parent -> 
                    GladeXML -> 
                    IORef QsWindowPrefs -> 
                    DrawingArea ->
                    IO ()

setUpPreferences qs qsXml qsPreferences  da
    = do prefDialog <- xmlGetWidget qsXml castToDialog "configQSWindowDialog"
         prefMenuItem <- xmlGetWidget qsXml castToMenuItem "preferences_menu_item"
         prefMenuItem `onActivateLeaf` showPrefDialog qs qsXml prefDialog qsPreferences da
         setPreferenceLayouts qsPreferences da
         widgetHideAll prefDialog

showPrefDialog :: forall self parent.
		  (DialogClass self, WindowClass parent) =>
		  parent -> 
                  GladeXML -> 
                  self -> 
                  IORef QsWindowPrefs -> 
                  DrawingArea ->
                  IO ()

showPrefDialog qs qsXml prefDialog qsPreferences da
    = do windowSetTransientFor prefDialog qs
         nsSB <- xmlGetWidget qsXml castToSpinButton "dotsizeSpinButton"
         msSB <- xmlGetWidget qsXml castToSpinButton "minSeparationSpinButton"
         nmXoffsetSB <-  xmlGetWidget qsXml castToSpinButton "nameXoffsetSpinButton"
         nmYoffsetSB <-  xmlGetWidget qsXml castToSpinButton "nameYoffsetSpinButton"
         showTraceCB <-  xmlGetWidget qsXml castToCheckButton "showTraceCheckButton"
         trXoffsetSB <-  xmlGetWidget qsXml castToSpinButton "traceXoffsetSpinButton"
         trYoffsetSB <-  xmlGetWidget qsXml castToSpinButton "traceYoffsetSpinButton"
         fontButtonFB <-  xmlGetWidget qsXml castToFontButton "fontbutton"
         nameSizeSB <-  xmlGetWidget qsXml castToSpinButton "nameFontSizeSpinButton"
         brLblSizeSB <-  xmlGetWidget qsXml castToSpinButton "branchLabelFontSizeSpinButton"
         traceSizeSB <-  xmlGetWidget qsXml castToSpinButton "traceFontSizeSpinButton"
         elisionSizeSB <-  xmlGetWidget qsXml castToSpinButton "elisionFontSizeSpinButton"
         leafSizeSB <-  xmlGetWidget qsXml castToSpinButton "leafFontSizeSpinButton"
         tracePrecSB <-  xmlGetWidget qsXml castToSpinButton "traceShowDigitsSpinButton"
         leafPrecSB <-  xmlGetWidget qsXml castToSpinButton "leafShowDigitsSpinButton"
         prefs <- readIORef qsPreferences
         nsSB `spinButtonSetValueAsInt` nodeSize prefs
         msSB `spinButtonSetValueAsInt`  nodeSeparation prefs
         nmXoffsetSB  `spinButtonSetValueAsInt` nodeNameXOffset prefs
         nmYoffsetSB  `spinButtonSetValueAsInt` nodeNameXOffset prefs
         showTraceCB  `toggleButtonSetActive` showTrace prefs
         trXoffsetSB  `spinButtonSetValueAsInt` traceXOffset prefs
         trYoffsetSB  `spinButtonSetValueAsInt` traceYOffset prefs
         fontButtonFB  `fontButtonSetFontName` fontName prefs
         nameSizeSB  `spinButtonSetValueAsInt` nameSize prefs
         brLblSizeSB  `spinButtonSetValueAsInt` branchLabelSize prefs
         traceSizeSB  `spinButtonSetValueAsInt` traceSize prefs
         elisionSizeSB  `spinButtonSetValueAsInt` elisionSize prefs
         leafSizeSB  `spinButtonSetValueAsInt` leafSize prefs
         tracePrecSB  `spinButtonSetValueAsInt` tracePrecision prefs
         leafPrecSB  `spinButtonSetValueAsInt` leafPrecision prefs
         widgetShowAll prefDialog
         resp <- dialogRun prefDialog
         case resp of
           ResponseOk ->
               do ns <- spinButtonGetValueAsInt nsSB
                  ms <- spinButtonGetValueAsInt msSB
                  nmX <- spinButtonGetValueAsInt nmXoffsetSB
                  nmYo <- spinButtonGetValueAsInt nmYoffsetSB
                  showtr <-  toggleButtonGetActive showTraceCB
                  trXo  <- spinButtonGetValueAsInt trXoffsetSB
                  trYo  <- spinButtonGetValueAsInt trYoffsetSB
                  font  <- fontButtonGetFontName fontButtonFB
                  name  <- spinButtonGetValueAsInt nameSizeSB
                  brLb  <- spinButtonGetValueAsInt brLblSizeSB
                  tracsz <-  spinButtonGetValueAsInt traceSizeSB
                  elis  <- spinButtonGetValueAsInt elisionSizeSB
                  leafsz <-  spinButtonGetValueAsInt leafSizeSB
                  tracpr <-  spinButtonGetValueAsInt tracePrecSB
                  leafpr <-  spinButtonGetValueAsInt leafPrecSB
                  let newprefs = QsWindowPrefs  ns  ms  nmX  nmYo  showtr  
                                 trXo  trYo  font  name  brLb  elis tracsz
                                 leafsz  tracpr  leafpr Nothing Nothing Nothing
                                 Nothing Nothing
                  writeIORef qsPreferences newprefs
                  setPreferenceLayouts qsPreferences da
                  print "Wrote the qsprefs"
                  widgetHideAll prefDialog
                  print "hid dialog"
           _ -> do widgetHideAll prefDialog
         

\end{code}
