\subsection{Control to show the |Qstack|}
\begin{code}
module QstackWindow where

import Graphics.UI.WXCore
import Graphics.UI.WX as WX
import QSM.QuantumStack
import QSM.ClassicalStack(showCstackElem)
import Data.Basis
import Data.ClassComp
import Data.Map as Map
import Data.List as List
import Data.LazyNum
import Control.Monad

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

instance Quantum OurBasis LazyNum

data QsWData = QsWData {
    showTrace :: Bool,
    treeDepth :: Int,
    qswQstack :: QuantumStack OurBasis LazyNum}
       deriving Show

type QsWindow a  = ScrolledWindow (CQsWindow a)
data CQsWindow a = CQsWindow
  
getQtreeData::EvtHandler a -> IO QsWData
getQtreeData w = do t <- unsafeEvtHandlerGetClientData w
		    return $ case t of 
		                Just x -> x
				Nothing -> QsWData False 99 $ fromInteger 1

setQtreeData::EvtHandler a -> QsWData -> IO ()
setQtreeData w = evtHandlerSetClientData w (return ())

qsAllData :: Attr (QsWindow a) (QsWData)
qsAllData =
    newAttr "qsAllData" getter setter
    where
    getter qsw = getQtreeData qsw
    setter qsw newdata = do setQtreeData qsw newdata
                            refresh qsw

qsShowTrace :: Attr (QsWindow a) (Bool)
qsShowTrace =
    newAttr "qsShowTrace" getter setter
    where
    getter qsw        = liftM showTrace $ getQtreeData qsw

    setter qsw b  = do qd <- getQtreeData qsw
		       setQtreeData qsw qd{showTrace = b}
		       refresh qsw


qsdepth :: Attr (QsWindow a) (Int)
qsdepth
  = newAttr "qsdepth" getter setter
  where
    getter qsw        = liftM treeDepth $ getQtreeData qsw

    setter qsw d  = do qd <- getQtreeData qsw
		       setQtreeData qsw qd{treeDepth = d}
		       set qsw [ virtualSize := framesize d $ qswQstack qd ]
		       refresh qsw


qstree :: Attr (QsWindow a) (QuantumStack OurBasis LazyNum)
qstree 
  = newAttr "qstree" getter setter
  where
    getter qsw        = liftM qswQstack  $ getQtreeData qsw


    setter qsw qtr = do qd <- getQtreeData qsw
 			setQtreeData qsw qd{qswQstack = qtr}
                        qd' <- getQtreeData qsw
			set qsw [ virtualSize := framesize (treeDepth qd) qtr]
			refresh qsw

framesize ::Int -> QuantumStack OurBasis LazyNum -> Size
framesize a qt 
	  = sz  frameszx frameszy
    where frameszx = 150 + approxWidthTree a (Just qt)
	  frameszy = 150 + (ndX * (min a $ depth qt))

qsWindow :: Window a -> [Prop (QsWindow ())] -> IO (QsWindow ())
qsWindow parent props 
    = do let defaults  = [scrollRate := sz 20 20
                         , bgcolor := white
                         , border := BorderSunken
			 , fullRepaintOnResize := False]
	     qsprops = castProps cast props
	 sw <- scrolledWindow parent (defaults ++ qsprops)
	 let qsw = cast sw
         tree <- get qsw qstree
         mdp <- get qsw qsdepth
         set qsw [ on paint := qspaint (get qsw qsAllData)
		 ,  virtualSize := framesize mdp tree]
	 return qsw
    where
    -- our casting operator: type signature is necessary!
         cast :: Window a -> QsWindow ()
	 cast = objectCast

qspaint :: IO (QsWData) -> DC a -> Rect -> IO ()
qspaint  qswd dc viewArea
    = do qswdata <- qswd
         set dc [fontFace := "Courier New", fontSize := stdFontsize, 
                 fontWeight := WeightBold ]
	 startx <- leftWidthTree (treeDepth qswdata) dc (Just $ qswQstack qswdata)
	 pntNode dc (pt (startx+25) 50) 1 qswdata

nodeColour ::(Quantum a b)=> QuantumStack a b -> Color
nodeColour StackZero  = black
nodeColour (StackData _)  = blue
nodeColour (StackInt _ _)  = green
nodeColour (StackQbit _ _)  = red
nodeColour (StackCons _ _) = magenta

\end{code}
|lftMove00| and |lftMove01| compute the distance for the left subtrees.
|lftMove00| moves over enough for the width of the 01 tree (empty in
bits) and for the righthalf of the 0 / 00 tree.
|lftMove01| moves over enough for the right side of it's tree.

We have corresponding right moves.
\begin{code}

movePoints ::Int -> DC d -> 
             QuantumStack OurBasis LazyNum ->
             Point ->IO [Point]
movePoints maxDepth dc nd@(StackCons nm dvals) pt 
    = do let strees = List.map (snd . snd) $ Map.toList dvals
             (leftStrees, midTree, rightStrees) = splitSubs strees 
             ndepth = depth nd
         lms <- leftMoves maxDepth dc pt  leftStrees midTree ndepth
         midmv <- midMove maxDepth dc pt midTree ndepth
         rms <- rightMoves maxDepth dc pt rightStrees midTree ndepth
         return (lms ++ midmv ++ rms)
movePoints maxDepth dc nd@(StackInt nm cvals) pt 
    = do let strees = List.map snd $ Map.toList cvals
             (leftStrees, midTree, rightStrees) = splitSubs strees 
             ndepth = depth nd
         lms <- leftMoves maxDepth dc pt  leftStrees midTree ndepth
         midmv <- midMove maxDepth dc pt midTree ndepth
         rms <- rightMoves maxDepth dc pt rightStrees midTree ndepth
         return (lms ++ midmv ++ rms)

leftMoves ::Int->DC d -> Point -> [QuantumStack OurBasis LazyNum]  ->
          [QuantumStack OurBasis LazyNum]  -> Int ->IO [Point]
leftMoves maxDepth dc pt  [] _ _ = return []
leftMoves maxDepth dc pt  nds midtree ndepth
    = do rwt <- rightWidthTree maxDepth dc (Just (head nds))
	 wt <- mapM (widthTree maxDepth dc ) (List.map Just (tail nds))
         midwt <- case midtree of 
                   [] -> return 0
                   (t:ts) -> leftWidthTree maxDepth dc (Just t) 
	 let offst = (ndY + mdoffset * (min maxDepth ndepth))
             wts = addToLast midwt (rwt : wt)
	     loffsts = scanr1 (+) wts
	 return (List.map (\loff -> (pointMove (vec (-loff) offst) pt)) loffsts)


midMove ::Int->DC d -> Point -> 
            [QuantumStack OurBasis LazyNum] -> Int -> IO [Point]
midMove maxDepth dc pt [] _ = return []
midMove maxDepth dc pt  _ ndepth
    = do let offst = (ndY + mdoffset * (min maxDepth ndepth))
	 return ([pointMove (vec 0 offst) pt])


rightMoves ::Int->DC d -> Point -> [QuantumStack OurBasis LazyNum]  ->
          [QuantumStack OurBasis LazyNum] ->Int -> IO [Point]
rightMoves maxDepth dc pt  [] _ _ = return []
rightMoves maxDepth dc pt  nds midtree ndepth
    = do lwt <- leftWidthTree maxDepth dc (Just (last nds))
	 wt <- mapM (widthTree maxDepth dc ) (List.map Just (init nds))
         midwt <- case midtree of 
                   [] -> return 0
                   (t:ts) -> rightWidthTree maxDepth dc (Just t) 
	 let offst = (ndY + mdoffset * (min maxDepth ndepth))
             wts = wt ++ [midwt + lwt]
	     roffsts = scanr1 (+) wts
	 return (List.map (\roff -> (pointMove (vec roff offst) pt)) roffsts)

lftMove00 ::Int->DC d -> QuantumStack OurBasis LazyNum  -> Point ->IO Point
lftMove00 maxDepth dc nd pt 
    = do rwt <- rightWidthTree maxDepth dc $ subTree00 nd
	 wt <- widthTree maxDepth dc $ subTree01 nd
	 let offst = (ndY + mdoffset * (min maxDepth (depth nd)))
	     loffst = (rwt + wt)
	 return (pointMove (vec (-loffst) offst) pt)


lftMove01 ::Int->DC d ->QuantumStack OurBasis LazyNum  -> Point ->IO Point
lftMove01 maxDepth dc nd pt
    = do wt <- rightWidthTree maxDepth dc $ subTree01 nd
	 let offst = (ndY + mdoffset * (min maxDepth (depth nd)))
	     loffst =  wt
	 return (pointMove (vec (-loffst) offst) pt)

rtMove11  ::Int->DC d ->QuantumStack OurBasis LazyNum -> Point ->IO Point
rtMove11 maxDepth dc nd pt 
    = do lwt <- leftWidthTree maxDepth dc $ subTree11 nd
	 wt <- widthTree maxDepth dc $ subTree10 nd
	 let offst = (ndY + mdoffset * (min maxDepth (depth nd)))
	     roffst = max ndX (wt+lwt)
	 return (pointMove (vec roffst offst) pt)


rtMove10  ::Int-> DC d ->QuantumStack OurBasis LazyNum -> Point ->IO Point
rtMove10 maxDepth dc nd pt
    = do wt <- leftWidthTree maxDepth dc $ subTree10 nd
	 let offst = (ndY + mdoffset * (min maxDepth (depth nd)))
	     roffst = max ndX wt
         return (pointMove (vec roffst offst) pt)


txtMove :: Size -> Point ->Point
txtMove siz pt = pointMove (vec sw sh) pt
    where sh = -(dotsize + 2)
	  sw = -(dotsize + (sizeW siz) + 2)

datavalMove :: Size -> Point ->Point
datavalMove siz pt = pointMove (vec sw sh) pt
    where sh = ((sizeH siz)  `div` 2)
	  sw = -((sizeW siz) `div` 2)

trMove :: Point ->Point
trMove pt = pointMove (vec trXoffset trYoffset) pt


pntNode :: DC b -> Point -> Int-> QsWData -> IO ()

pntNode _ _ i qswd@(QsWData _ maxDepth _) 
    | i > maxDepth 
        = return()

pntNode dc point i qswd@(QsWData st maxDepth StackZero) 
    =  pntNode dc point i (QsWData st maxDepth (StackData SZero))

pntNode dc point _ qswd@(QsWData _ _ qsnd@(StackData _)) 
        = do circle dc point dotsize [bgcolor := (nodeColour qsnd)]
	     let ndtxt = nodevalue qsnd
	     sz <- getTextExtent dc ndtxt
             drawText dc ndtxt (datavalMove sz point) []

pntNode dc point i qswd@(QsWData st maxDepth qsnd@(StackInt _ cvals))
        = do branchPoints <- movePoints (maxDepth - i) dc qsnd point
             let delems = Map.toList cvals
                 cons = List.map fst delems
                 strees = List.map snd delems
	     paintBranches dc (List.map showCstackElem cons) point branchPoints (i+1) maxDepth strees st
             circle dc point dotsize [bgcolor := (nodeColour qsnd)]
	     let ndtxt = nodevalue qsnd
	     sz <- getTextExtent dc ndtxt
             drawText dc ndtxt (txtMove sz point) []
	     drawTrace dc st qsnd point



pntNode dc point i qswd@(QsWData st maxDepth qsnd@(StackCons _ dvals))
        = do branchPoints <- movePoints (maxDepth - i) dc qsnd point
             let delems = Map.toList dvals
                 cons = List.map fst delems
                 bvars = List.map (fst . snd) delems
                 strees = List.map (snd . snd) delems
	     paintBranches dc (zipWith (++) cons $ 
                                       List.map ((flip showList) "") bvars) 
                              point branchPoints (i+1) maxDepth strees st
             let shiftRect = vec (-dotsize ) 0
                 rectCoord = rect (pointMove shiftRect point)  
                             (sz (2 * dotsize) (2 * dotsize))
             drawRect dc rectCoord [bgcolor := (nodeColour qsnd)]
	     let ndtxt = nodevalue qsnd
	     sz <- getTextExtent dc ndtxt
             drawText dc ndtxt (txtMove sz point) []
	     drawTrace dc st qsnd point




pntNode dc point i qswd@(QsWData st maxDepth qsnd@(StackQbit _ _))
        = do lft00 <- lftMove00 (maxDepth - i) dc qsnd point
	     paintBranch dc "00" point lft00  (i+1) st maxDepth $ subTree00 qsnd
	     lft01 <- lftMove01 (maxDepth - i) dc qsnd point
             paintBranch dc "01" point lft01  (i+1) st maxDepth $ subTree01 qsnd
	     rt10 <- rtMove10 (maxDepth - i) dc qsnd point
             paintBranch dc "10" point rt10  (i+1) st maxDepth $ subTree10 qsnd
	     rt11 <- rtMove11 (maxDepth - i) dc qsnd point
             paintBranch dc "11" point rt11  (i+1) st maxDepth $ subTree11 qsnd
             circle dc point dotsize [bgcolor := (nodeColour qsnd)]
	     let ndtxt = nodevalue qsnd
	     sz <- getTextExtent dc ndtxt
             drawText dc ndtxt (txtMove sz point) []
	     drawTrace dc st qsnd point

drawTrace dc st qsnd pt
    = if st then do let text = "[" ++ (show $ trace qsnd) ++ "]"
                    drawText dc text (trMove pt) [color := white, 
                                                  fontSize := trcFontsize ]
                    drawText dc text (trMove pt) [color := black,
                                                  fontSize := trcFontsize ]
            else do print "Not Drawing trace text"
                    return ()

paintBranch dc lbl point left i st maxDepth  mqsnd
          = case mqsnd of 
               Nothing -> return ()
               Just qsnd -> 
                   if i > maxDepth then 
			do let pt = pointMove (vec (-10) 30) point
			   line dc pt point [ pen := penColored red 2]
			   szddd <- getTextExtent dc "[...]"
			   let tp = pointMove (vec 0 $ sizeH szddd) $
				    midPoint pt $ 
				    pointMove (vecNegate $ vecFromSize szddd) pt
                           drawText dc "[...]" tp
                                        [fontSize := dddFontsize, color := red]
                   else
			do line dc point left [color := black]
			   drawText dc lbl (midPoint point left) [fontSize := lblFontsize]
			   pntNode dc left i (QsWData st maxDepth qsnd)



paintBranches _ [] _ _ _ _ _ _ = return ()
paintBranches dc (lbl:lbls) point (sub:subs) i maxDepth (qsnd:qsnds) st
          = if i > maxDepth then 
		do let pt = pointMove (vec (-10) 30) point
		   line dc pt point [ pen := penColored red 2]
	           szddd <- getTextExtent dc "[...]"
		   let tp = pointMove (vec 0 $ sizeH szddd) $
			    midPoint pt $ 
			    pointMove (vecNegate $ vecFromSize szddd) pt
                   drawText dc "[...]" tp
                                [fontSize := dddFontsize, color := red]
            else
		do line dc point sub [color := black]
                   --let label = (fst lbl) ++ showList (snd lbl) ""
                   set dc [fontSize := lblFontsize]
                   lsize <- getTextExtent dc lbl --abel
                   let lpoint = branchLblPt point sub lsize
		   drawText dc lbl lpoint []
                   set dc [fontSize := stdFontsize] 
		   pntNode dc sub i (QsWData st maxDepth qsnd) 
                   paintBranches dc lbls point subs i maxDepth qsnds st

branchLblPt :: Point -> Point -> Size -> Point
branchLblPt top bot txtsize
    = let vert = (pointX top) == (pointX bot)
          shiftsize = if vert then vec 0 0
                      else vec (- ((sizeW txtsize) `div` 2)) 0
      in pointMove shiftsize (midPoint top bot)

subTree00  = subTree Zero Zero
subTree01  = subTree Zero One
subTree10  = subTree One Zero
subTree11  = subTree One One

subTree :: (Quantum a b) => a -> a -> QuantumStack a b 
        -> Maybe (QuantumStack a b)
subTree a1 a2 (StackQbit _ qvls)
    = valmaybe (a1,a2) qvls
subTree _ _ s = Just s  


--subTree :: (Quantum a b) => a -> a -> QuantumStack a b -> QuantumStack a b
--subTree a1 a2 (StackInt _ cvls)
--        | a1 == a2 = findWithDefault 1 a1 cvls
--        | otherwise = 1
--subTree a1 a2 (StackQbit _ qvls)
--    = findWithDefault 1 (a1,a2) qvls
--subTree _ _ s = s     

leftWidthTree ::(Quantum a b)=> Int ->DC d -> 
                Maybe (QuantumStack a b) -> IO Int
leftWidthTree 0 _ _ = return ndX
leftWidthTree _ _ Nothing = return 0
leftWidthTree _ _ (Just StackZero) = return dotsize
leftWidthTree _ dc (Just (StackData b)) =
    do sz<- getTextExtent dc (show b)
       return (ndSep + (sizeW sz))
leftWidthTree i dc (Just (StackCons nm dvals))
    = do sz <- getTextExtent dc nm
         let strees = Map.toList dvals
             leftstrees = take ( leftcount strees) strees
         subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $ snd $ snd elem |
                      elem <- leftstrees]
         return ((sizeW sz) + ndX * 2 + subtrees)
leftWidthTree i dc (Just (StackInt nm cvals))
    = do sz <- getTextExtent dc nm
         let strees = Map.toList cvals
             leftstrees = take ( leftcount strees) strees
         subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $ snd elem |
                      elem <- leftstrees]
         return ((sizeW sz) + ndX * 2 + subtrees)
leftWidthTree i dc (Just stk) 
    = do subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ 
                      subTree (head basis) b stk | 
		      b<- basis]
         return (ndX * 2 + subtrees)


rightWidthTree ::(Quantum a b)=> Int ->DC d -> 
                 Maybe (QuantumStack a b) -> IO Int
rightWidthTree 0 _ _ = return ndX
rightWidthTree _ _ Nothing = return 0
rightWidthTree _ _ (Just StackZero)  = return dotsize
rightWidthTree _ dc (Just (StackData b)) =
    do sz<- getTextExtent dc (show b)
       return (ndSep + (sizeW sz))
rightWidthTree i dc (Just (StackCons nm dvals))
    = do sz <- getTextExtent dc nm
         let strees = Map.toList dvals
             rightstrees = drop ( leftcount strees) strees
         rsubtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $ snd $ snd elem |
                      elem <- rightstrees]
         return ((sizeW sz) + ndX * 2 + rsubtrees)
rightWidthTree i dc (Just (StackInt nm dvals))
    = do sz <- getTextExtent dc nm
         let strees = Map.toList dvals
             rightstrees = drop ( leftcount strees) strees
         rsubtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $ snd elem |
                      elem <- rightstrees]
         return ((sizeW sz) + ndX * 2 + rsubtrees)
rightWidthTree i dc (Just stk) 
    = do subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ 
                      subTree (head $ tail basis) b stk | 
		      b<- basis]
         return (ndX * 2 + subtrees)


widthTree ::(Quantum a b)=> Int -> DC d ->
            Maybe (QuantumStack a b) -> IO Int
widthTree 0 _ _ = return ndX
widthTree _ _ Nothing = return 0
widthTree _ _ (Just StackZero) = return dotsize
widthTree _ dc (Just (StackData b)) 
    = do sz<- getTextExtent dc (show b)
	 return (ndSep + (sizeW sz))
widthTree i dc (Just (StackCons nm dvals))
    = do sz <- getTextExtent dc nm
         subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $ snd $ snd elem |
                      elem <- Map.toList dvals]
         return ((sizeW sz) + ndX * 2 + subtrees)
widthTree i dc (Just (StackInt nm dvals))
    = do sz <- getTextExtent dc nm
         subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ Just $  snd elem |
                      elem <- Map.toList dvals]
         return ((sizeW sz) + ndX * 2 + subtrees)
widthTree i dc (Just stk)  
    = do subtrees <- liftM sum $ sequence 
                     [widthTree (i-1) dc $ 
                                subTree a b stk |
				a<-basis, b<- basis]
	 return (ndX * 2 + subtrees)



approxWidthTree ::(Quantum a b)=>Int-> Maybe(QuantumStack a b) -> Int
approxWidthTree 0 _ = ndX
approxWidthTree _ Nothing = 0
approxWidthTree _ (Just StackZero) = dotsize
approxWidthTree _ (Just (StackData b) )
    = ndSep + 50
approxWidthTree mdp (Just (StackCons _ dvals) ) 
    = ndX * 2 + (sum [approxWidthTree (mdp - 1)  $ Just $ snd $ snd elem |
                       elem <- Map.toList dvals]) 
approxWidthTree mdp (Just (StackInt _ dvals) ) 
    = ndX * 2 + (sum [approxWidthTree (mdp - 1)  $ Just $ snd elem |
                       elem <- Map.toList dvals]) 
approxWidthTree mdp (Just stk)  
    = ndX * 2 + ( sum  [approxWidthTree (mdp - 1) $ subTree a b stk |
			   a<-basis, b<- basis])



nodevalue::(Quantum a b)=>QuantumStack a b -> String
nodevalue (StackData a) = show a
nodevalue (StackCons s _)
           | s == "" = "Cons"
           | otherwise = s
nodevalue (StackInt s _) 
           | s == "" = "int"
           | otherwise = s
nodevalue (StackQbit s _) 
           | s == "" = "qbit"
           | otherwise = s
nodevalue StackZero = "ZERO"


midPoint :: Point -> Point -> Point
midPoint p1 p2 = p3
    where p3x = ((pointX p1) + (pointX p2)) `div` 2
	  p3y = ((pointY p1) + (pointY p2)) `div` 2
	  p3 = pt p3x p3y

leftcount :: [a] -> Int
leftcount = (`div` 2) . length

rightcount  :: [a] -> Int
rightcount  elems 
    = let le = length elems
      in le - (leftcount elems)



splitSubs :: [a] -> ([a], [a], [a])
splitSubs [] = ([],[],[])
splitSubs elems 
    | even (length elems) = (lft, [], rgt)
    | (0 == length rgt) = (lft, [], [])
    | otherwise = (lft, [head rgt], tail rgt)
    where (lft, rgt) = splitAt ((leftcount elems)) elems

addToLast :: (Num a) => a -> [a] ->[a]
addToLast _ [] = []
addToLast a ([el]) = [el + a]
addToLast a (el:els) = el: addToLast a els

\end{code}
