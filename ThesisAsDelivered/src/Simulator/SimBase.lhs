\begin{code}
module Simulator.SimBase where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Mogul
import Graphics.UI.Gtk.Gdk.Events

import QSM.QSM
import QSM.ControlStack
import Data.LazyNum
import Data.Map as Map
import Data.List as List
import Data.IORef
import Shared.Version

instance Quantum OurBasis LazyNum


data QsWindowPrefs =
    QsWindowPrefs {
          nodeSize :: Int,
          nodeSeparation :: Int,
          nodeNameXOffset :: Int,
          nodeNameYOffset :: Int,
          showTrace :: Bool,
          traceXOffset :: Int,
          traceYOffset :: Int,
          fontName :: String,
          nameSize :: Int,
          branchLabelSize :: Int,
          elisionSize :: Int,
          traceSize :: Int,
          leafSize :: Int,
          tracePrecision :: Int,
          leafPrecision :: Int,
          branchLabelLayout :: Maybe PangoLayout,
          elisionLayout :: Maybe PangoLayout,
          traceLayout :: Maybe PangoLayout,
          nameLayout :: Maybe PangoLayout,
          leafLayout :: Maybe PangoLayout
}
--                                 7 15 (-9)
defaultControls = QsWindowPrefs 10 45 (-9) (-9) True 10 (-6) "Courier"
                  10 8 8 8 10 2 5 Nothing Nothing Nothing Nothing Nothing


spinButtonSetValueAsInt :: SpinButtonClass self => self -> Int -> IO ()
spinButtonSetValueAsInt me val
                        = spinButtonSetValue me (fromInteger $ toInteger val)

newColour :: GC -> Color -> IO (Color, GC)
newColour gc clr 
    = do gcvals <- gcGetValues gc
         let fg = foreground gcvals
         gc `gcSetValues` newGCValues { foreground=clr}
         return (fg, gc)

resetColour :: GC -> Color -> IO ()
resetColour gc clr
    = do newColour gc clr
         return ()

filledCircle :: DrawableClass d => Color ->
                d -> GC -> 
                Point ->
                Int ->
                IO()
filledCircle clr d gc (x, y) w =
    do (fg, gc) <- newColour gc clr       
       drawArc d gc True (x-(w `div` 2)) (y-(w `div` 2)) w w 0 (360*64)
       (_, gc) <- newColour gc black
       drawArc d gc False (x-(w `div` 2)) (y-(w `div` 2)) w w 0 (360*64)
       resetColour gc fg

filledRect :: DrawableClass d => 
              Color ->
              d -> GC -> 
              Point ->
              Int ->
              IO()
filledRect clr d gc (x, y) w =
    do (fg, gc) <- newColour gc clr
       drawRectangle d gc True   (x-w2) (y-w2) w w
       (_, gc) <- newColour gc black
       drawRectangle d gc False  (x-w2) (y-w2) w w
       resetColour gc fg
                   where w2 = w `div` 2


colourLine :: DrawableClass d => 
              Color ->
              d -> GC -> 
              Point ->
              Point ->
              IO()
colourLine c d gc pt1 pt2 =
    do (fg, gc) <- newColour gc c
       drawLine d gc pt1 pt2
       resetColour gc fg

blackLine :: DrawableClass d => d -> GC -> 
             Point ->
             Point ->
             IO()
blackLine = colourLine (Color 0 0 0)

drawText  :: (DrawableClass d) => 
             d -> GC -> 
             PangoLayout ->
             String ->
             Point ->
             IO()
drawText  d gc playout txt pt@(x,y) =
    do --getTextExtent pcontext txt 
       playout `layoutSetText` txt
--       print $ "Printing at " ++ (show pt)
       drawLayout d gc x y playout 
        
drawTextColour  :: (DrawableClass d) => 
                   Color ->
                   d -> GC -> 
                   PangoLayout ->
                   String ->
                   Point ->
                   IO()
drawTextColour  c d gc playout txt pt@(x,y) =
    do playout `layoutSetText` txt
       --print " set layout"
       drawLayoutWithColors d gc x y playout (Just c) (Nothing)
       --print "Drew the layout"
        

pointMove :: Point -> Point -> Point
pointMove (x1,y1) (x,y) = (x+x1, y+y1)

sizeAdd :: Point -> Point -> Point
sizeAdd = pointMove

sizeSum :: [Point] -> Point
sizeSum = foldl sizeAdd (0,0)

addNodeSeparation :: QsWindowPrefs -> Point -> Point
addNodeSeparation prefs 
    = sizeAdd (nodeSeparation prefs,nodeSeparation prefs)

showAboutDialog parent aboutDialog 
    = do  windowSetTransientFor aboutDialog parent
          aboutDialog `aboutDialogSetVersion` simulatorVersion
-- make the dialog non-modal. When the user closes the dialog hide it.
          aboutDialog `afterResponse` (\_ -> widgetHideAll aboutDialog)
          widgetShowAll aboutDialog



midPoint :: Point -> Point -> Point
midPoint (p1x,p1y) (p2x,p2y) = (p3x,p3y)
    where p3x = (p1x + p2x) `div` 2
	  p3y = (p1y + p2y) `div` 2

leftcount :: [a] -> Int
leftcount = (`div` 2) . length

rightcount  :: [a] -> Int
rightcount  elems 
    = let le = length elems
      in le - (leftcount elems)

lefttrees :: (Quantum a b) =>
             QuantumStack a b ->
             [QuantumStack a b]
lefttrees (StackQbit _ qvals)
          = [(val (head basis,b) qvals) | b<-basis]
lefttrees (StackInt _ cvals)
 = let cvs = Map.elems cvals
   in take (max 1 $ leftcount cvs) cvs
lefttrees (StackCons _ dvals)
 = let cvs = Map.elems dvals
   in take (max 1 $ leftcount cvs) $ List.map snd cvs
lefttrees _ = []

righttrees :: (Quantum a b) =>
             QuantumStack a b ->
             [QuantumStack a b]
righttrees (StackQbit _ qvals)
          = [(val (head $ tail basis,b) qvals) | b<-basis]
righttrees (StackInt _ cvals)
 = let cvs = Map.elems cvals
   in take (max 1 $ rightcount cvs) cvs
righttrees (StackCons _ dvals)
 = let cvs = Map.elems dvals
   in take (max 1 $ rightcount cvs) $ List.map snd cvs
righttrees _ = []

subtrees :: (Quantum a b) =>
             QuantumStack a b ->
             [QuantumStack a b]
subtrees (StackQbit _ qvals)
          = [(val (a,b) qvals) | a<-basis, b<-basis]
subtrees (StackInt _ cvals)
 = Map.elems cvals
subtrees (StackCons _ dvals)
 =   List.map snd $ Map.elems dvals
subtrees _ = []
                 

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



nodevalue::(Quantum a LazyNum)=>QuantumStack a LazyNum -> String
nodevalue (StackData a) = showl a
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




approxWidthTree ::(Quantum a b)=>
                  QsWindowPrefs ->
                  Int-> 
                  Maybe(QuantumStack a b) -> Int
approxWidthTree qsprefs 0 _ = nodeSeparation qsprefs
approxWidthTree _ _ Nothing = 0
approxWidthTree qsprefs _ (Just StackZero) = nodeSize qsprefs
approxWidthTree qsprefs _ (Just (StackData b) )
    = (nodeSeparation qsprefs) + 50
approxWidthTree qsprefs mdp (Just (StackCons _ dvals) ) 
    = (nodeSeparation qsprefs) * 2 + 
      (sum [approxWidthTree qsprefs (mdp - 1)  $ Just $ snd $ snd elem |
            elem <- Map.toList dvals]) 
approxWidthTree qsprefs mdp (Just (StackInt _ dvals) ) 
    = (nodeSeparation qsprefs) * 2 + 
      (sum [approxWidthTree qsprefs (mdp - 1)  $ Just $ snd elem |
            elem <- Map.toList dvals]) 
approxWidthTree qsprefs mdp (Just stk)  
    = (nodeSeparation qsprefs) * 2 + 
      ( sum  [approxWidthTree  qsprefs (mdp - 1) $ subTree a b stk |
	      a<-basis, b<- basis])


subTree00 :: (Quantum OurBasis b) => 
             QuantumStack OurBasis b ->
             Maybe (QuantumStack OurBasis b)
subTree00  = subTree Zero Zero

subTree01 :: (Quantum OurBasis b) => 
             QuantumStack OurBasis b ->
             Maybe (QuantumStack OurBasis b)
subTree01  = subTree Zero One

subTree10 :: (Quantum OurBasis b) => 
             QuantumStack OurBasis b ->
             Maybe (QuantumStack OurBasis b)
subTree10  = subTree One Zero

subTree11 :: (Quantum OurBasis b) => 
             QuantumStack OurBasis b ->
             Maybe (QuantumStack OurBasis b)
subTree11  = subTree One One

subTree :: (Quantum a b) => 
           a -> 
           a -> 
           QuantumStack a b ->
           Maybe (QuantumStack a b)
subTree a1 a2 (StackQbit _ qvls)
    = valmaybe (a1,a2) qvls
subTree _ _ s = Just s  



nodeColour ::(Quantum a b)=> QuantumStack a b -> Color
nodeColour StackZero  = Color 0 0 0 -- Black
nodeColour (StackData _)  = Color 0 0 65535 --Blue 
nodeColour (StackInt _ _)  = Color 0 65535 0 --green
nodeColour (StackQbit _ _)  = Color 65535 0 0 --red
nodeColour (StackCons _ _) = Color 65535 0 65535 --magenta


getTextExtent :: PangoLayout ->
                 String ->
                 IO Point
getTextExtent playout txt 
    = do playout `layoutSetText` (hackMinusToSpace txt)
--         print $ " getting text extent for '"++txt++"'" 
--         (PangoRectangle pxl pyl pwl phl, PangoRectangle px py pw ph) <- layoutGetExtents playout
--         print $ "Got Pangorectangles: (ink/log)"++(show (px,py,pw,ph)) ++
--                   " , " ++ (show (pxl,pyl, pwl, phl))
         (Rectangle xl yl wl hl, Rectangle x y w h) <- layoutGetPixelExtents playout
--         print $ "Got rectangles: (ink/log)"++(show (x,y,w,h)) ++
--                    " , " ++ (show (xl,yl, wl, hl))
         let ext = (abs (w-x) ,abs (h-y))
--         print $ " text extent - "++txt++" - "++(show (x,y,w,h)) ++
--                   " equals "++(show ext)++ " was " ++ (show (xl,yl, wl, hl))
         return ext

setpcSize :: PangoContext -> Int -> IO FontDescription
setpcSize pc size
    = do fd <- contextGetFontDescription pc
         fdc <- fontDescriptionCopy fd
         fdc `fontDescriptionSetSize` (fromInteger $ toInteger size)
         pc `contextSetFontDescription` fdc
         return fd


rectWHToPoint :: Rectangle -> Point
rectWHToPoint (Rectangle _ _ w h) = (w,h)
         

txtMove :: QsWindowPrefs -> Point -> Point ->Point
txtMove  qsprefs  (w,_) = pointMove (cw, ch) 
    where ch = nodeNameYOffset qsprefs
	  cw = (nodeNameXOffset qsprefs) - w

datavalMove :: Point -> Point ->Point
datavalMove (w,h) = pointMove (sw, sh) 
    where sh = (h  `div` 2)
	  sw = -(w `div` 2)

trMove :: QsWindowPrefs ->
          Point ->Point
trMove qsprefs = pointMove (traceXOffset qsprefs, traceYOffset qsprefs)


red = Color 65536 0 0 
black = Color 0 0 0 
white = Color 65536 65536 65536

showl :: LazyNum ->String
showl = display 3


clearDrawable :: DrawableClass d =>
                 d ->
                 GC ->
                 IO()
clearDrawable pm gc
    = do (w,h) <- drawableGetSize pm
         gcv <- gcGetValues gc
         gcSetValues gc $ newGCValues{foreground = white,
                                     background = white,
                                     function = Set,
                                     fill = Solid}
         drawRectangle pm gc True 0 0 w h
         gcSetValues gc gcv



pickIthMS :: (Quantum a b) => Int -> MachineState a b -> BMS a b
pickIthMS i = collapse . hd . (dropI i)

\end{code}

The function |collapse| is used in the visualization.


{\begin{singlespace}
\begin{code}
collapse:: (Quantum a b) => CMS a b -> BMS a b
collapse (CMS cm ctls ip rc cscqds)
   = BMS qs s rc cm ip d ns
      where s = (snd . fst .  head) cscqds
            ns = (fst . fst . head) cscqds
            d = snd $ snd $ head cscqds
            qs = (fst . splitcontrolled) $ 
                 removeAllControl ctls $ 
                 (fst . unzip . snd . unzip) cscqds


\end{code}
\end{singlespace}
}

\begin{code}

branchLblPt :: Point -> Point -> Point -> Point
branchLblPt top@(topx, topy) bot@(botx,boty) (txtx, txty)
    = let vert = (topx == botx)
          yshift = -(txty `div` 2)
          shiftsize = if vert then (0, yshift)
                      else (-(txtx `div` 2), yshift)
      in pointMove shiftsize (midPoint top bot)


hackMinusToSpace :: String -> String
hackMinusToSpace [] = []
hackMinusToSpace ('-':ss) = 'a':hackMinusToSpace ss
hackMinusToSpace (a:ss) = a:hackMinusToSpace ss


intersectRect :: Rectangle -> Rectangle -> Rectangle
intersectRect (Rectangle x1 y1 w1 h1)  (Rectangle x2 y2 w2 h2)
    = Rectangle (max x1 x2) (max y1 y2) (min w1 w2) (min w2 h2)


combineBB :: BoundingBox ->BoundingBox ->BoundingBox 
combineBB ((xo,yo),(xt,yt)) ((xo',yo'),(xt',yt')) =
    ((min xo xo', min yo yo'), (max xt xt', max yt yt'))


bbToRect :: BoundingBox -> Rectangle
bbToRect ((x1,y1),(x2,y2)) = 
    Rectangle (min x1 x2) (min y1 y2) (abs (x2 - x1)) (abs (y2 - y1))
rectToBB :: Rectangle -> BoundingBox 
rectToBB (Rectangle x y w h) =
   ((x,y),(x+w,y+h))

bbIntersectsRect :: BoundingBox -> Rectangle -> Bool
bbIntersectsRect ((x1,y1),(x2,y2)) r
    = let ((x1',y1'),(x2',y2')) = rectToBB r
      in (x1' `between` (x1,x2) && y1' `between` (y1,y2)) ||
             (x2' `between` (x1,x2) && y2' `between` (y1,y2)) 

bbIntersectsRegion :: Region -> BoundingBox -> IO Bool
bbIntersectsRegion r bb =
    do let rect = bbToRect bb
       overlap <- r `regionRectIn` rect
       return $ case overlap of
                  OverlapRectangleOut -> False
                  _ -> True

between :: Int -> (Int, Int) -> Bool
between x (x1,x2) = x1 <= x && x <= x2

type Sizexy = Point

type BoundingBox = (Point, Point)




translateBBx :: Int->BoundingBox -> BoundingBox
translateBBx x (p1,p2) = (pointMove (x,0) p1, pointMove (x,0) p2)

bbToWidth :: BoundingBox -> Int
bbToWidth ((x1,_),(x2,_)) = abs (x2 - x1)

bbToSize :: BoundingBox -> Sizexy
bbToSize ((ox,oy),(sx,sy)) = (sx - ox, sy - oy)

translate :: Point -> Sizexy ->BoundingBox
translate (xo,yo) (sx,sy) = ((xo,yo),(xo+sx,yo+sy))

addSizes :: Sizexy -> Sizexy -> Sizexy
addSizes (x,y) (x',y') = (x+x', y+y')

expandBB :: Int -> BoundingBox -> BoundingBox
expandBB n ((x,y),(w,h)) = ((x,y),(n+w,n+h))


makeBBFromCenter :: Point -> Int -> BoundingBox
makeBBFromCenter (x,y) w
                 = normalizeBB ((x-w2,y-w2), (x+w2,y+w2))
                   where w2 = w `div` 2

makeBBFromTextSize :: Point -> Sizexy -> BoundingBox
makeBBFromTextSize org wh = normalizeBB (org, pointMove wh org)

makeBBFromPoints :: Point -> Point -> BoundingBox
makeBBFromPoints (x1,y1) (x2,y2) 
    = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

normalizeBB :: BoundingBox -> BoundingBox
normalizeBB = uncurry makeBBFromPoints

combineBBs :: [BoundingBox] -> BoundingBox
combineBBs = foldl1 combineBB

leftWidth :: (Point -> BoundingBox) -> Int
leftWidth bb = (abs . fst . fst . bb) (0,0)

rightWidth :: (Point -> BoundingBox) -> Int
rightWidth bb = (abs . fst . snd . bb) (0,0)
\end{code}
