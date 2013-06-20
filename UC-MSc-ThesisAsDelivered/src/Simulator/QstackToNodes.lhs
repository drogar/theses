\begin{code}
module Simulator.QstackToNodes where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Mogul
import Graphics.UI.Gtk.Gdk.Events

import QSM.QSM
import QSM.ControlStack
import Data.LazyNum
import Data.List as List
import Data.Map as Map
import Data.IORef
import Simulator.NodesAndLines
import Simulator.SimBase
import Control.Monad
import Data.Tuples
import Data.ClassicalData
import Data.Basis

bufferbb = 5

makeNodesAndLines :: Int->Int -> Maybe Graph -> QsWindowPrefs ->
                     QuantumStack OurBasis LazyNum -> 
                     IO Graph
makeNodesAndLines depth level above prefs qsnd@(StackZero)
   = makeNodesAndLines depth level above prefs (StackData SZero)
makeNodesAndLines depth level above prefs qsnd@(StackData _)
    = do let (Just layout) = leafLayout prefs
             nodeText = nodevalue qsnd
             nodeSz = nodeSize prefs
             nsep = nodeSeparation prefs
         tsize <- getTextExtent layout nodeText
         --print $ "sdata node, text=" ++ (show tsize)
         let tstart = datavalMove tsize (nodeSz,nodeSz)
             tend = pointMove tsize tstart
             drawme :: forall d. DrawableClass d => d-> GC-> Bool -> Point -> IO()
             drawme dc gc (b::Bool) pt  =
                      do reg <- drawableGetClipRegion dc
                         b <- bbIntersectsRegion reg (ndbb b pt)
                         when b $ do (pickDrawType  qsnd) dc gc pt nodeSz
                                     drawText dc gc layout nodeText (datavalMove tsize pt)
             ndbb :: Bool -> Point -> BoundingBox
             ndbb t pt = let dotbb = makeBBFromCenter pt nodeSz
                             textbb = makeBBFromTextSize (datavalMove tsize pt) tsize
                         in  expandBB bufferbb $ combineBB dotbb textbb
         return $ GraphNode (Node ndbb  drawme (nodeSz,depth * nsep))
                    above [] [] []

makeNodesAndLines depth 0 above prefs qsnd =
    do let (Just layout) = elisionLayout prefs
           nodeText = "[...]"
           nodeSz = (0,0)
           nsep = nodeSeparation prefs
       tsize <- getTextExtent layout nodeText
       let tstart = (0,0)
           tend = pointMove tsize tstart
           drawme :: forall d. DrawableClass d => d-> GC-> Bool -> Point -> IO()
           drawme dc gc strc pt  =
               do reg <- drawableGetClipRegion dc
                  b <- bbIntersectsRegion reg (ndbb strc pt)
                  when b $ do drawText dc gc layout nodeText pt
                              when strc $ drawTrace dc gc prefs qsnd pt
           ndbb :: Bool -> Point -> BoundingBox
           ndbb t pt = let textbb = makeBBFromTextSize  pt tsize
                       in  expandBB bufferbb  textbb
           gnode =  GraphNode (Node ndbb drawme (0,depth * nsep))
                    above [] [] []
       return gnode

makeNodesAndLines depth level above prefs qsnd
    = mdo let (Just layout) = nameLayout prefs
              (Just trLayout) = traceLayout prefs
              nodeText = nodevalue qsnd
              nodeSz = nodeSize prefs
              nsep = nodeSeparation prefs
          tsize <- getTextExtent layout nodeText
          trcsize <- getTextExtent trLayout ( "(" ++ (showl $ trace qsnd) ++ ")")
          let tstart = txtMove prefs tsize (nodeSz,nodeSz)
              tend = pointMove tsize tstart
              drawme :: forall d. DrawableClass d => d-> GC-> Bool -> Point -> IO()
              drawme dc gc strc pt  =
                  do reg <- drawableGetClipRegion dc
                     b <- bbIntersectsRegion reg (ndbb strc pt)
                     when b $ do (pickDrawType qsnd) dc gc pt nodeSz
                                 drawText dc gc layout nodeText (txtMove prefs tsize pt)
                                 when strc $ drawTrace dc gc prefs qsnd pt
              ndbb :: Bool -> Point -> BoundingBox
              ndbb t pt = 
                  let dotbb = makeBBFromCenter pt nodeSz
                      textbb = makeBBFromTextSize (txtMove prefs tsize pt) tsize
                      trcbb = makeBBFromTextSize (trMove prefs pt) trcsize
                  in  expandBB bufferbb $ combineBB dotbb $ 
                      if t then combineBB textbb trcbb else textbb
              gnode =  GraphNode (Node ndbb drawme (nodeSz,depth * nsep))
                           above llines cline rlines
              nmnds = namesAndNodes qsnd
          (llines, cline, rlines) <- makeLines depth (level - 1) gnode prefs nmnds
          --print $ "For node "++nodeText++", count is ("++ 
          --          (show $ length llines) ++", " ++
          --          (show $ length cline) ++", " ++
          --          (show $ length rlines) ++"), nms = "++showList nmnds "."
          --print $ " lwidth(t) = " ++ (show $ leftWidth $ ndbb True) ++
          --         ", rwidth(t) = " ++ (show $ rightWidth $ ndbb True)
          --print $ " bbox at t,(0,0) = " ++ (show $ ndbb True (0,0))
          --print $ " Ndbb = " ++ (show $ makeBBFromCenter (0,0) nodeSz)
          --print $ " txtbb = " ++ (show $ makeBBFromTextSize (txtMove prefs tsize (0,0)) tsize)
          --print $ " trbb = " ++ (show $ makeBBFromTextSize (trMove prefs (0,0)) trcsize)

          return gnode


pickDrawType :: (QuantumStack OurBasis LazyNum) -> 
                (forall d.  DrawableClass d => 
                d -> GC -> 
                Point ->
                Int ->
                IO())
pickDrawType (StackInt _ _) = filledCircle (Color 0 65535 0) -- Green
pickDrawType (StackQbit _ _) = filledCircle (Color 65535 0 0) -- Red
pickDrawType (StackCons _ _) = filledRect (Color 65535 0 65535) -- Magenta
pickDrawType (StackData _) = filledCircle (Color 0 0 65535) -- Blue
pickDrawType (StackZero) = filledCircle (Color 0 0 0) -- Black

makeLine :: Int -> Int->Graph -> QsWindowPrefs ->
             (String, QuantumStack OurBasis LazyNum) -> 
             IO Graph 
makeLine depth level above prefs (name,qs)
    = mdo let blsize = branchLabelSize prefs
              Just blLayout = branchLabelLayout prefs
          labelSize <- getTextExtent blLayout name
          let topC = (0,depth * nsep)
              nsep = nodeSeparation prefs
              depth' = depth+1
              bottomC = (0, depth' * nsep)
              tstart = branchLblPt topC bottomC labelSize
              tend = pointMove (fst labelSize,0) tstart
              lnbb :: Point -> Point -> BoundingBox
              lnbb tpt bpt = 
                  let lblPt = branchLblPt tpt bpt labelSize
                      linebb = makeBBFromPoints tpt bpt
                      textbb = makeBBFromTextSize lblPt labelSize
                  in  expandBB bufferbb $ combineBB linebb  textbb 
              drawMe:: forall d. DrawableClass d => d-> GC-> Point -> Point -> IO()
              drawMe dc gc from to =
                  do reg <- drawableGetClipRegion dc
                     b <- bbIntersectsRegion reg (lnbb from to)
                     when b $ do blackLine dc gc from to
                                 print $ "Line: from, to: " ++ (show from) ++ (show to)++ " midpoint is " ++ (show $ midPoint from to)
                                 let lblPt = branchLblPt from to labelSize
                                 print $ "Line label: "++name ++ "Size is" ++ (show labelSize) ++" at "++ (show lblPt)
                                 drawText dc gc blLayout name lblPt
              gnode = GraphLine (Line lnbb topC bottomC drawMe) above below
          below <- makeNodesAndLines depth' level (Just gnode) prefs qs
          return gnode

makeLines :: Int->Int -> Graph -> QsWindowPrefs ->
             [(String, QuantumStack OurBasis LazyNum)] -> 
             IO ([Graph],[Graph],[Graph]) 
makeLines depth level above prefs pairs
    = do lines <- mapM (makeLine depth level above prefs) pairs
         return $ splitThreeWays lines


splitThreeWays :: [a] -> ([a],[a],[a])
splitThreeWays [] = ([],[],[])
splitThreeWays [a] = ([],[a],[])
splitThreeWays [a,b] = ([a],[],[b])
splitThreeWays [a,b,c] = ([a],[b],[c])
splitThreeWays aas = let (a,b) = (head aas, last aas)
                         aas' = (tail . init) aas
                         (l,c,r) = splitThreeWays aas'
                     in (a:l, c, r++[b])
                    



namesAndNodes :: QuantumStack OurBasis LazyNum ->
                 [(String,QuantumStack OurBasis LazyNum)]
namesAndNodes (StackZero) = []
namesAndNodes (StackData _) = []
namesAndNodes (StackInt _ cvs)
              = List.map (app1of2 showCv) $ Map.toList cvs
namesAndNodes (StackQbit _ qvs)
              = List.map (app1of2 showQv) $ Map.toList qvs
namesAndNodes (StackCons _ dvs)
              = List.map (uncurry showSplitdv) $ Map.toList dvs

showSplitdv :: Constructor -> ([StackAddress],QuantumStack OurBasis LazyNum)
               -> (String, QuantumStack OurBasis LazyNum)
showSplitdv cons (sas,qs)
            = (cons ++ (showList sas ""),qs)







drawTrace :: (DrawableClass d, Quantum a LazyNum) =>
             d
             -> GC
             -> QsWindowPrefs
             -> QuantumStack a LazyNum
             -> Point
             -> IO ()

drawTrace da gc qsprefs qsnd pt
    = do let text = "(" ++ (showl $ trace qsnd) ++ ")"
             trpoint = (trMove qsprefs pt)
             Just tl = traceLayout qsprefs
--         print $ "Trace" ++ text ++ " at " ++ (show trpoint) ++ 
--                   " node at " ++ (show pt)
--         drawTextColour white da gc pc text trpoint
--         print "Drew white"
         drawText da gc tl text trpoint
--         print "Drew black"


\end{code}

