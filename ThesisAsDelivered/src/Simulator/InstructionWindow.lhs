\begin{code}
module Simulator.InstructionWindow where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Mogul
import Graphics.UI.Gtk.Gdk.Events

import QSM.QSM
import Data.LazyNum
import Data.Map as Map
import Data.List as List
import Data.IORef
import Simulator.SimBase
import Text.Printf


updateInsTabs :: (IORef (MachineState OurBasis LazyNum)) ->
                 Notebook -> 
                 SpinButton ->
                 Bool ->
                 IO ()
updateInsTabs ms nb sb reset =
    do cmstate <- readIORef ms
       sdepth <- spinButtonGetValueAsInt sb
       let current = hd $ dropI sdepth cmstate
           cm = cmsCodeMem current
           rcode = cmsRunningCode current
           ip = snd $ cmsInstructionPointer current
       -- print $ "About to set tabs data, reset=" ++ (show reset)
       setTabData nb cm rcode reset ip
       nb `notebookSetCurrentPage` 0

setTabData :: Notebook -> 
              Memory OurBasis -> 
              [Instruction OurBasis] -> 
              Bool ->
              Int->
              IO()
setTabData  nb cm current reset ip = 
    if reset
    then do -- print "Removing pages"
            removePages nb
            -- print $ "Adding pages "++(show $ Map.size cm) ++" Pages."
            addPages nb $ ("Current",current): (Map.toList cm)
            -- print "showing all widgets in the nb" 
            widgetShowAll nb
    else do resetCurrentPage nb ip current


removePages :: Notebook -> IO()
removePages nb
  = do np <- notebookGetNPages nb
       if np == 0 then return()
          else do notebookRemovePage nb (-1)
                  removePages nb


resetCurrentPage :: Notebook -> 
                    Int ->
                    [Instruction OurBasis] -> 
                    IO ()
resetCurrentPage  nb ip ins
    = do tv <- setPageData ins ip
         -- print "rcp, Creating a new scrolled window"
         sw <- scrolledWindowNew Nothing Nothing
         -- print "rcp, setting SW policy to automatic"
         scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
         -- print "rcp, Adding tree view (page data) to sw"
         sw `containerAdd` tv
         notebookRemovePage nb 0
         notebookInsertPage nb sw "Current" 0
         widgetShow tv
         widgetShow sw
         widgetShow nb
         return ()


addPages :: Notebook -> 
            [(String, [Instruction OurBasis])] -> 
            IO ()
addPages nb [] = return ()
addPages nb ((label,ins):code) =
    do -- print $ " Adding the page " ++ label
       addSinglePage nb label ins
       -- print "looping add pages"
       addPages nb code


addSinglePage :: Notebook -> 
                 String -> 
                 [Instruction OurBasis] -> 
                 IO ()
addSinglePage nb label ins 
    = do -- print "ASP, setting page data"
         tv <- setPageData  ins 0
         -- print "ASP, Creating a new scrolled window"
         sw <- scrolledWindowNew Nothing Nothing
         -- print "ASP, setting SW policy to automatic"
         scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
         -- print "ASP, Adding tree view (page data) to sw"
         sw `containerAdd` tv
         -- print $ "ASP, Appending page " ++ label
         notebookAppendPage nb sw label
         -- print "ASP, done, returnin"
         return ()


setPageData :: [Instruction OurBasis] -> Int-> IO TextView
setPageData ins ip
    = do txtv <- textViewNew
         tb <- textViewGetBuffer txtv
         tb `textBufferSetText` ( concat $ ( zipWith (printf " %3d  %s\n") [ip..]  (List.map show ins)))
         return txtv

oldsetPageData :: [Instruction OurBasis] -> 
               Int -> 
               IO TreeView
oldsetPageData  ins ip
    = do -- print " SPD: Creating tree model"
         (tv, store, setLineno, setIns) <- createTreeAndStore
         -- print " SPD: Setting headers to visible"
         tv `treeViewSetHeadersVisible` True
         -- print " SPD: Getting selection from tv"
         sel <- treeViewGetSelection tv
         -- print " SPD: Setting selection mode to none"
         sel `treeSelectionSetMode` SelectionNone
-- fill the list with some entries
         -- print " SPD: Adding data to the tree model"
-- TODO - This inner loop seems to crash at random times. but especially with long strings.
         mapM_ (\(ln, txt) -> 
                    do -- print $ "Spd inner: ("++ln++", "++txt++");"
                       iter <- listStoreAppend store
                       setLineno iter ln 
                       setIns iter txt
                       return ()
                       ) $
                 zip (List.map show [ip..])  (List.map show ins)
         -- print " SPD: Created and returning tree model"
         return tv



 -- create the tree model
createTreeAndStore = do
  mainView <- treeViewNew
  skel <- emptyListSkel
  
  let createTextColumn name = do
        (attr, _, set) <- listSkelAddAttribute skel cellText
        column <- newTreeViewColumn
        column `treeViewColumnSetTitle` name
        mainView `treeViewAppendColumn` column
        renderer <- treeViewColumnNewText column True True
        renderer `treeViewColumnAssociate` [attr]
        return set

  -- create the various columns in both the model and view
  setLineNo <- createTextColumn "Line"
  setIns    <- createTextColumn "Instruction"
  
  store <- newListStore skel
  mainView `treeViewSetModel` store
  return (mainView, store, setLineNo, setIns)



\end{code}
