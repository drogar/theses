\subsection{Control to show the |Qstack|}
\begin{code}
module Simulator.QstackWindow where

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
         (w,h) <- drawQstack qsPrefRef msRef dw sd td
         widgetSetSizeRequest da w h
         return True

drawQstack :: (DrawableClass d,  DrawWindowClass d) =>
              IORef (QsWindowPrefs) ->
              IORef (MachineState OurBasis LazyNum) ->
              d ->
              SpinButton ->
              SpinButton ->
              IO (Int,Int)
drawQstack qsPrefRef msRef dw sdSpin tdSpin 
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
         (startx,_) <- leftSizeOfTree qsPref td gc (Just qs)
         (w,h) <- sizeOfTree qsPref td gc (Just qs)
         print $ "Starting left size is "++(show startx)
         print $ "Tree is " ++ (show qs)
         print $ "Tree depth spin is " ++ (show td)
         print $ "Size returned is " ++ (show w) ++ ", "++show h
         dw `drawWindowBeginPaintRect` (Rectangle 0 0 (w+100) (3 * h `div` 2 +240))

         drawWindowClear dw
         pntNode dw qsPref gc ((50+startx), 30) 0 td qs
         
         drawWindowEndPaint dw
         
         return ((w+100),3 * h `div` 2+240)


\end{code}
|lftMove00| and |lftMove01| compute the distance for the left subtrees.
|lftMove00| moves over enough for the width of the 01 tree (empty in
bits) and for the righthalf of the 0 / 00 tree.
|lftMove01| moves over enough for the right side of it's tree.

We have corresponding right moves.
\begin{code}

movePoints ::QsWindowPrefs ->
             Int -> GC -> 
             QuantumStack OurBasis LazyNum ->
             Point ->IO [Point]
movePoints qspref  maxDepth gc nd@(StackCons nm dvals) pt 
    = do let strees = List.map (snd . snd) $ Map.toList dvals
             (leftStrees, midTree, rightStrees) = splitSubs strees 
             ndepth = depth nd
         lms <- leftMoves qspref  maxDepth gc pt leftStrees midTree ndepth
         midmv <- midMove qspref  maxDepth gc pt midTree ndepth
         rms <- rightMoves qspref  maxDepth gc pt rightStrees midTree ndepth
         return (lms ++ midmv ++ rms)
movePoints qspref  maxDepth gc nd@(StackInt nm cvals) pt 
    = do let strees = List.map snd $ Map.toList cvals
             (leftStrees, midTree, rightStrees) = splitSubs strees 
             ndepth = depth nd
         lms <- leftMoves qspref  maxDepth gc pt  leftStrees midTree ndepth
         midmv <- midMove qspref  maxDepth gc pt midTree ndepth
         rms <- rightMoves qspref maxDepth gc pt rightStrees midTree ndepth
         return (lms ++ midmv ++ rms)

leftMoves ::QsWindowPrefs ->
            Int->GC -> Point -> 
            [QuantumStack OurBasis LazyNum]  ->
            [QuantumStack OurBasis LazyNum]  -> 
            Int ->IO [Point]
leftMoves qspref  maxDepth gc pt  [] _ _ = return []
leftMoves qspref  maxDepth gc pt  nds midtree ndepth
    = do (rwt,_) <- rightSizeOfTree qspref  maxDepth gc 
                    (Just (head nds))
	 wt <- mapM (sizeOfTree qspref  maxDepth gc ) 
               (List.map Just (tail nds))
         (midwt,_) <- case midtree of 
                      [] -> return (0,0)
                      (t:ts) -> 
                             leftSizeOfTree qspref  maxDepth gc 
                                                (Just t) 
	 let offst = (nodeSeparation qspref) * (1 + (min maxDepth ndepth))
             wts = addToLast midwt (rwt : List.map fst wt)
	     loffsts = scanr1 (+) wts
	 return (List.map (\loff -> (pointMove ((-loff), offst) pt)) loffsts)


midMove ::QsWindowPrefs ->
          Int->GC -> Point -> 
          [QuantumStack OurBasis LazyNum] -> 
          Int -> IO [Point]
midMove  qspref maxDepth gc pt [] _ = return []
midMove  qspref maxDepth gc pt  _ ndepth
    = do let offst =  (nodeSeparation qspref) * (1 + (min maxDepth ndepth))
	 return ([pointMove (0, offst) pt])


rightMoves :: QsWindowPrefs ->
              Int->GC -> Point -> 
              [QuantumStack OurBasis LazyNum]  ->
              [QuantumStack OurBasis LazyNum] ->
              Int -> IO [Point]
rightMoves qspref maxDepth gc pt  [] _ _ = return []
rightMoves qspref maxDepth gc pt  nds midtree ndepth
    = do (lwt,_) <- leftSizeOfTree qspref maxDepth gc (Just (last nds))
	 wt <- mapM (sizeOfTree qspref maxDepth gc ) 
               (List.map Just (init nds))
         (midwt,_) <- case midtree of 
                   [] -> return (0,0)
                   (t:ts) -> 
                       rightSizeOfTree qspref maxDepth gc 
                                   (Just t) 
	 let offst =  (nodeSeparation qspref) * (1 + (min maxDepth ndepth))
             wts = (List.map fst wt) ++ [midwt + lwt]
	     roffsts = scanr1 (+) wts
	 return (List.map (\roff -> (pointMove (roff, offst) pt)) roffsts)

lftMove00 ::QsWindowPrefs ->
            Int->GC -> 
            QuantumStack OurBasis LazyNum  -> 
            Point ->IO Point
lftMove00 qspref maxDepth gc nd pt 
    = do (rwt,_) <- rightSizeOfTree qspref maxDepth gc $ subTree00 nd
	 (wt,_) <- sizeOfTree qspref maxDepth gc $ subTree01 nd
	 let offst = (nodeSeparation qspref) * (1 + (min maxDepth (depth nd)))
	     loffst = (rwt + wt)
	 return (pointMove ((-loffst), offst) pt)


lftMove01 ::QsWindowPrefs ->
            Int->GC ->
            QuantumStack OurBasis LazyNum  -> 
            Point ->IO Point
lftMove01 qspref maxDepth gc nd pt
    = do (wt,_) <- rightSizeOfTree qspref maxDepth gc $ subTree01 nd
	 let offst = (nodeSeparation qspref) * (1 + (min maxDepth (depth nd)))
	     loffst =  wt
	 return (pointMove ((-loffst), offst) pt)

rtMove11  ::QsWindowPrefs ->
            Int->GC ->
            QuantumStack OurBasis LazyNum -> 
            Point ->IO Point
rtMove11 qspref  maxDepth gc nd pt 
    = do (lwt,_) <- leftSizeOfTree qspref maxDepth gc $ subTree11 nd
	 (wt,_) <- sizeOfTree qspref maxDepth gc $ subTree10 nd
	 let offst = (nodeSeparation qspref) * (1 + (min maxDepth (depth nd)))
	     roffst = max (nodeSeparation qspref) (wt+lwt)
	 return (pointMove (roffst, offst) pt)


rtMove10  ::QsWindowPrefs ->
            Int-> GC ->
            QuantumStack OurBasis LazyNum -> 
            Point ->IO Point
rtMove10 qspref maxDepth gc nd pt
    = do (wt,_) <- leftSizeOfTree qspref maxDepth gc $ subTree10 nd
	 let offst = (nodeSeparation qspref) * (1 + (min maxDepth (depth nd)))
	     roffst = max (nodeSeparation qspref) wt
         return (pointMove (roffst, offst) pt)



pntNode :: DrawableClass d =>
           d -> 
           QsWindowPrefs ->
           GC -> Point -> 
           Int-> Int ->
           QuantumStack OurBasis LazyNum -> 
           IO ()

pntNode _ _ _ _ i maxDepth qs
    | i > maxDepth 
        = do print $ "Reached maxdepth of " ++ (show maxDepth)
             return()

pntNode da qsprefs gc point i maxDepth StackZero
    =  pntNode da qsprefs gc point i maxDepth  (StackData SZero)

pntNode da qsprefs gc point _  _ qsnd@(StackData _)
        = do filledCircle (nodeColour qsnd) da gc point 
                              (nodeSize qsprefs)
	     let ndtxt = nodevalue qsnd
                 Just ll = leafLayout qsprefs
	     sz <- getTextExtent ll ndtxt
             drawText da gc ll ndtxt (datavalMove sz point) 

pntNode da  qsprefs gc point i maxDepth qsnd@(StackInt _ cvals)
        = do branchPoints <- movePoints qsprefs (maxDepth - i) gc qsnd point
             let delems = Map.toList cvals
                 cons = List.map fst delems
                 strees = List.map snd delems
	     paintBranches da qsprefs gc 
                               (List.map showCstackElem cons) 
                               point branchPoints i maxDepth strees
             filledCircle (nodeColour qsnd) da gc point (nodeSize qsprefs)
             pntNodeTxt da gc qsprefs qsnd point


pntNode da  qsprefs gc point i maxDepth qsnd@(StackCons _ dvals)
        = do branchPoints <- movePoints qsprefs  (maxDepth - i) gc qsnd point
             let delems = Map.toList dvals
                 cons = List.map fst delems
                 bvars = List.map (fst . snd) delems
                 strees = List.map (snd . snd) delems
	     paintBranches da  qsprefs gc 
                               (zipWith (++) cons $ 
                                        List.map ((flip showList) "") bvars) 
                               point branchPoints i maxDepth strees 
             let dotsize = nodeSize qsprefs
                 rectOrg = pointMove ((-dotsize ), 0) point
             filledRect (nodeColour qsnd) da gc rectOrg dotsize
             pntNodeTxt da gc qsprefs  qsnd point


pntNode da qsprefs gc point i maxDepth qsnd@(StackQbit _ _)
        = do lft00 <- lftMove00 qsprefs (maxDepth - i) gc qsnd point
	     paintBranch da qsprefs gc "00" point lft00  (i+1) maxDepth $ subTree00 qsnd
	     lft01 <- lftMove01 qsprefs (maxDepth - i) gc qsnd point
             paintBranch da qsprefs gc "01" point lft01  (i+1) maxDepth $ subTree01 qsnd
	     rt10 <- rtMove10 qsprefs (maxDepth - i) gc qsnd point
             paintBranch da qsprefs gc "10" point rt10  (i+1) maxDepth $ subTree10 qsnd
	     rt11 <- rtMove11 qsprefs (maxDepth - i) gc qsnd point
             paintBranch da qsprefs gc "11" point rt11  (i+1) maxDepth $ subTree11 qsnd
             filledCircle (nodeColour qsnd) da gc point (nodeSize qsprefs)
             pntNodeTxt da gc qsprefs qsnd point

pntNodeTxt :: (Quantum a LazyNum, DrawableClass d) =>
              d
              -> GC
              -> QsWindowPrefs
              -> QuantumStack a LazyNum
              -> Point
              -> IO ()

pntNodeTxt da gc qsprefs qsnd point
    = do let ndtxt = nodevalue qsnd
             Just nl = nameLayout qsprefs
         sz <- getTextExtent nl ndtxt
         drawText da gc nl ndtxt (txtMove qsprefs sz point)
	 when (showTrace qsprefs) $ drawTrace da gc qsprefs qsnd point 

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
         print $ "Trace" ++ text ++ " at " ++ (show trpoint) ++ 
                   " node at " ++ (show pt)
--         drawTextColour white da gc pc text trpoint
--         print "Drew white"
         drawText da gc tl text trpoint
         print "Drew black"

paintBranch :: forall d.
               (DrawableClass d) =>
               d
               -> QsWindowPrefs
               -> GC
               -> String
               -> Point
               -> Point
               -> Int
               -> Int
               -> Maybe (QuantumStack OurBasis LazyNum)
               -> IO ()

paintBranch da qsprefs gc lbl point left i maxDepth  mqsnd
          = case mqsnd of 
               Nothing -> return ()
               Just qsnd -> 
                   if i > maxDepth then 
                      do print "greater than md"
                         drawElision da qsprefs gc point
                   else
			do drawBranch da gc qsprefs lbl 
                                      point left i maxDepth qsnd
drawElision :: (DrawableClass d) =>
               d -> 
               QsWindowPrefs -> GC -> 
               Point -> IO ()
drawElision da qsprefs gc point 
    = do let pt = pointMove ((-10), 20) point
             Just el = elisionLayout qsprefs
         colourLine red da gc pt point
         (szw,szh) <- getTextExtent el "..."
	 let tp = pointMove (0 , szh) $
		  midPoint pt $ 
		  pointMove ((-szh),(-szw)) pt
         drawTextColour red da gc el "..." tp

paintBranches ::  (DrawableClass d) =>
                 d
                 -> QsWindowPrefs
                 -> GC
                 -> [String]
                 -> Point
                 -> [Point]
                 -> Int
                 -> Int
                 -> [QuantumStack OurBasis LazyNum]
                 -> IO ()

paintBranches _ _ _ [] _ _ _ _ _ = return ()
paintBranches  da qsprefs gc (lbl:lbls) point (sub:subs) i maxDepth (qsnd:qsnds) 
          = if i > maxDepth then 
		do  drawElision da qsprefs gc point
            else
		do drawBranch da gc qsprefs lbl point sub i maxDepth qsnd
                   paintBranches da qsprefs 
                                 gc lbls point subs (i+1) maxDepth qsnds

drawBranch da gc qsprefs lbl point sub i maxDepth qsnd
   = do blackLine da gc point sub
        let Just bll = branchLabelLayout qsprefs
        ls <- getTextExtent bll lbl
        let lpt = branchLblPt point sub ls
	drawText da gc bll lbl lpt
	pntNode da qsprefs gc sub (i+1) maxDepth qsnd




--subTree :: (Quantum a b) => a -> a -> QuantumStack a b -> QuantumStack a b
--subTree a1 a2 (StackInt _ cvls)
--        | a1 == a2 = findWithDefault 1 a1 cvls
--        | otherwise = 1
--subTree a1 a2 (StackQbit _ qvls)
--    = findWithDefault 1 (a1,a2) qvls
--subTree _ _ s = s     

leftSizeOfTree ::(Quantum a LazyNum)=> 
                QsWindowPrefs ->
                Int ->GC -> 
                Maybe (QuantumStack a LazyNum) -> 
                IO (Int,Int)
leftSizeOfTree qsprefs 0 _ _ 
    = return $ (nodeSeparation qsprefs,nodeSeparation qsprefs)
leftSizeOfTree _ _ _ Nothing = return (0,0)
leftSizeOfTree qsprefs _ _ (Just StackZero) 
    = return $ (nodeSize qsprefs,nodeSize qsprefs)
leftSizeOfTree qsprefs _ gc (Just (StackData b)) =
    do let Just ll = leafLayout qsprefs
       sz<-getTextExtent ll (showl b)
       
       return $ addNodeSeparation qsprefs sz
leftSizeOfTree qsprefs i gc (Just tree)
    = do let leftstrees = lefttrees tree
             Just nl = nameLayout qsprefs
         size <- getTextExtent nl (theName tree)
         streesize <- 
             subTreeSizes qsprefs (i-1) gc  leftstrees
         let sz = addNodeSeparation qsprefs $
                  sizeAdd size streesize
 --        print $ "Left size is " ++ (show sz) ++
 --                " of " ++ (show tree) ++ " subtrees size was " ++
 --                       (show streesize)
         return sz

rightSizeOfTree ::(Quantum a LazyNum)=>  
                QsWindowPrefs ->
                Int ->GC -> 
                Maybe (QuantumStack a LazyNum) -> 
                IO (Int,Int)
rightSizeOfTree qsprefs 0 _ _ 
    = return  $ (nodeSeparation qsprefs,nodeSeparation qsprefs)
rightSizeOfTree _ _ _ Nothing = return (0,0)
rightSizeOfTree qsprefs _ _ (Just StackZero)  
    = return $ (nodeSize qsprefs,nodeSize qsprefs)
rightSizeOfTree qsprefs _ gc (Just (StackData b)) =
    do let Just ll = leafLayout qsprefs
       sz <- getTextExtent ll (showl b)
  --     print $ "right size is " ++ (show sz) ++
  --               " of " ++ (showl b)
       return  $ addNodeSeparation qsprefs sz
rightSizeOfTree qsprefs i gc (Just tree)
    = do let rightstrees = righttrees tree
             Just nl = nameLayout qsprefs
         size <- getTextExtent nl (theName tree) 
         streesize <- 
             subTreeSizes  qsprefs (i-1) gc rightstrees
         let sz = addNodeSeparation qsprefs $
                  sizeAdd size streesize
 --        print $ "Size is " ++ (show sz) ++
 --                " of " ++ (show tree) ++ " subtrees size was " ++
 --                       (show streesize)
         return sz

sizeOfTree ::(Quantum a LazyNum)=>   
            QsWindowPrefs ->
            Int -> GC ->
            Maybe (QuantumStack a LazyNum) -> 
            IO (Int,Int)
sizeOfTree qsprefs 0 _ _ 
    = return $ (nodeSeparation qsprefs,nodeSeparation qsprefs)
sizeOfTree _ _ _ Nothing = return (0,0)
sizeOfTree qsprefs _ _ (Just StackZero)  
    = return $ (nodeSize qsprefs,nodeSize qsprefs)
sizeOfTree qsprefs _ gc (Just (StackData b)) =
    do let Just ll = leafLayout qsprefs
       sz <- getTextExtent ll (showl b)
 --      print $ "Size is " ++ (show sz) ++
 --                " of " ++ (showl b)
       return  $ addNodeSeparation qsprefs sz
sizeOfTree qsprefs i gc (Just tree)
    = do let strees = subtrees tree
             Just nl = nameLayout qsprefs
         size <- getTextExtent nl (theName tree)
         streesize <- 
             subTreeSizes  qsprefs (i-1) gc  strees
         let sz = addNodeSeparation qsprefs $
                  sizeAdd size streesize
  --       print $ "Size is " ++ (show sz) ++
  --               " of " ++ (show tree) ++ " subtrees size was " ++
  --                      (show streesize)
         return sz
 

subTreeSizes ::(Quantum a LazyNum)=> 
                QsWindowPrefs ->
                Int ->GC -> 
                [QuantumStack a LazyNum] -> 
                IO (Int,Int)
subTreeSizes qsprefs i gc strees
    = liftM sizeSum $ 
      sequence 
      [sizeOfTree qsprefs i gc $ Just  elem |
       elem <- strees]

\end{code}
