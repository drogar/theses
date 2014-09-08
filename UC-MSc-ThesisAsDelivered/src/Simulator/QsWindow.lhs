\subsection{Control to show the |Qstack|}
\begin{code}
module Simulator.QsWindow where

import Graphics.UI.Gtk
import System.Glib.Types
import QSM.QSM
import Data.Basis
import Data.ClassComp
import Data.Map as Map
import Data.List as List
import Data.LazyNum
import Data.IORef
import Simulator.SimBase
import Control.Monad
import Simulator.QstackToNodes
import Simulator.NodesAndLines

     
\end{code}
                           
dotsize=7
trXoffset = 10
trYoffset = -10
ndX = 20 
ndY = 20
ndSep = 15
mdoffset=15
txty=(-2)
txtx=(-2)
clientsize = 500
lblFontsize = 8
stdFontsize = 10
trcFontsize = 10
dddFontsize = 8

\begin{code}


exposeQstack :: IORef (QsWindowPrefs) ->
              IORef (MachineState OurBasis LazyNum) ->
              DrawingArea ->
              SpinButton ->
              SpinButton ->
              Event ->
              IO Bool
exposeQstack qsPrefRef msRef  da sd td (Expose {eventArea=rect})  
    = do print "Expose qswindow"
         dw <- drawingAreaGetDrawWindow da
         gc <- gcNew dw         
         (w,h) <- drawQstack qsPrefRef msRef dw sd td rect
         widgetSetSizeRequest da w h
         return True

drawQstack :: (DrawableClass d,  DrawWindowClass d) =>
              IORef (QsWindowPrefs) ->
              IORef (MachineState OurBasis LazyNum) ->
              d ->
              SpinButton ->
              SpinButton ->
              Rectangle ->
              IO (Int,Int)
drawQstack qsPrefRef msRef dw sdSpin tdSpin rect
    = do gc <- gcNew dw
         qsPref <- readIORef qsPrefRef
         gcSetValues gc $ newGCValues {
                                     foreground = Color 0 0 0,
                                     background = Color 0 0 65535,
                                     capStyle = CapRound,
                                     lineWidth = 1,
                                     joinStyle = JoinRound}

         mstate <- readIORef msRef
         td <- spinButtonGetValueAsInt tdSpin
         sd <- spinButtonGetValueAsInt sdSpin
         let qs = quantumStack $ pickIthMS  sd mstate
             showTr = showTrace qsPref
         graph <- makeNodesAndLines 1 td Nothing qsPref qs
         let ((_,startx), graph') = setX (showTr,20) graph
             (w,h) = getFullSize showTr graph'
         print $ "Starting left size is "++(show startx)
         print $ "Top conn pt is "++ (show $ connectionPt $ node graph')
         --print $ "Tree is " ++ (show qs)
         print $ "Tree depth spin is " ++ (show td)
         print $ "Size returned is " ++ (show w) ++ ", "++show h
         let fullrect = (Rectangle 0 0 (w+1500) (3 * h `div` 2 +240))
             dregion = intersectRect fullrect rect
             (Rectangle xdr ydr wdr hdr) = dregion
         print $ "Region for dwindow is" ++ (show (xdr, ydr, wdr, hdr))
         dw `drawWindowBeginPaintRect` dregion

         drawWindowClear dw
         drawIt dw gc showTr  graph'        
         drawWindowEndPaint dw
         
         return ((w+1500),3 * h `div` 2+240)


\end{code}
