\subsection{Control for displaying instructions.}
\begin{code}
module InstructionWindow where
import Graphics.UI.WX.Attributes as WxAttributes
import Graphics.UI.WXCore
import Graphics.UI.WX
import QSM.QSM
import Data.Basis
import Data.Map as Map
import Data.List as List
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)


type InsWindow a  = Panel (CInsWindow a)
data CInsWindow a = CInsWindow
  
\end{code}
Define various attributes required for the window. All of these attributes
are held as window client data. We currently keep track of four pieces of
information:
\begin{description}
\item{Memory} This |Map|, keyed by |EntryPoint| holds the instructions
for each proc we have in the machine.
\item{CodeMemory} This is a |List| of |Instruction|s which we are currently 
working with and / or executing.
\item{EntryPoint} The current procedure of interest, primarily used when 
editing instructions.
\items{insertLvls} This |Map| contains the current insert level for each
procedure. See below for further description of insert level.
\end{description}

To assist in the handling of the window |Attr|s, we define two
helper functions to get and set the client data.
\begin{code}

  
getIwData::EvtHandler a -> IO ((Memory OurBasis, 
                                EntryPoint),
                               (Int, [Instruction OurBasis]))

getIwData w = do t <- unsafeEvtHandlerGetClientData w
		 return $ case t of 
		           Just ((a,b),(c,d)) ->  ((a,b),(c,d))
			   Nothing -> 
                               ((Map.singleton mainproglabel [], 
                                    mainproglabel) 
                                  ,(0,[]))

setIwData::EvtHandler a ->  ((Memory OurBasis, EntryPoint),
                             (Int,[Instruction OurBasis])) -> 
           IO ()
setIwData w = evtHandlerSetClientData w (return ())
\end{code}

The attribute |memDisplay| is the |Memory| we currently have in the machine.
Note that setting this attribute will also reset the |insList| attribute, 
based on the current |EntryPoint|
\begin{code}


memDisplay :: Attr (InsWindow a) (Memory OurBasis)
memDisplay 
  = newAttr "memDisplay" getter setter
  where
    getter qsw        = liftM  (fst . fst) $ getIwData qsw

    setter qsw mm =  do ((_,ep),_) <- getIwData qsw
                        let ins = getCode mm (ep,0)
			setIwData qsw ((mm,ep),(0,ins))
			panelSetSubs qsw mm (0,ins)
			refresh qsw

\end{code}

The attribute |insList| is the |CodeMemory| we currently are
working with in the machine. Setting of this attribute does
not affect any others.

\begin{code}

insList :: Attr (InsWindow a) ((Int,[Instruction OurBasis]))
insList
  = newAttr "insList" getter setter
  where
    getter qsw        = liftM  snd  $ getIwData qsw

    setter qsw ins  =  do ((mm,ep),_) <- getIwData qsw
			  setIwData qsw ((mm,ep), ins)
			  panelSetSubs qsw mm ins
			  refresh qsw


\end{code}

The attribute |entryPt| is the current |EntryPoint| we  are
working with in the machine. Setting of this attribute will 
display this tab on the window. Additionaly, if 
this is set to a new |EntryPoint|, it will add a 
new key value pair to the |insertLvls| |Map|.

\begin{code}

entryPt :: Attr (InsWindow a) (EntryPoint)
entryPt 
  = newAttr "entryPt" getter setter
  where
    getter qsw        = liftM  (snd . fst) $ getIwData qsw

    setter qsw newep 
        = do ((mm,_), ins) <- getIwData qsw
             case (hasProc mm newep) of
                True ->
		    do setIwData qsw ((mm,newep), ins)
                       setSelectionToEp qsw newep
		       refresh qsw
                False ->
                    error "Can not select nonexisting ep"
			     

\end{code}

Define a standard creation command.
\begin{code}

instructionWindow :: Window a -> [Prop (InsWindow ())] -> IO (InsWindow ())
instructionWindow parent props 
    = do let defaults  = []
	     qsprops = castProps cast props
--         p <- panel parent  (defaults ++ qsprops)
         p <- panel parent defaults
         nb <- notebook p [style := wxNB_LEFT]         
	 let inpanel = cast p
         set inpanel qsprops
         return inpanel
    where
    -- our casting operator: type signature is necessary!
         cast :: Window a -> InsWindow ()
	 cast = objectCast



setSelectionToEp :: InsWindow a -> EntryPoint -> IO ()
setSelectionToEp iw ep
	     = do (nbp:nbs) <- WxAttributes.get  iw children
                  let nb = case (safeCast nbp classNotebook) of
                             Just n -> n
                             Nothing -> unsafePerformIO (notebook iw [])
                  cnt <- notebookGetPageCount nb
                  setPageSel cnt nb ep

setPageSel :: Int -> Notebook b -> EntryPoint -> IO ()
setPageSel i nb ep
           = do pname <- notebookGetPageText nb i
                case (ep == pname) of
                 True ->
                     do notebookSetSelection  nb i
                        return ()
                 False -> if (i == 0) then return ()
                          else setPageSel (i-1) nb ep


panelSetSubs :: InsWindow a -> Memory OurBasis -> 
                (Int, [Instruction OurBasis]) -> IO ()
panelSetSubs iw mem (count, ins)
	     = do (nbp:nbs) <- WxAttributes.get  iw children
                  let nb = case (safeCast nbp classNotebook) of
                             Just n -> n
                             Nothing -> unsafePerformIO (notebook iw [])
                  --print "Deleting pages"
                  --notebookDeleteAllPages nb
                  --print "Checking notebook" 
                  --pc <- notebookGetPageCount nb
                  currPages <- getPages nb --pc
                  tabpages <- checkAndAddPages nb currPages count $ 
                                        ("Current" ,ins): 
                                        (Map.toList $
                                             mem)
--		  tabpage1 <- addPane nb ("Current", ins)
                  --print "Doing layout"
		  --tabpages <- getPagesForLayout nb (1 + Map.size mem)
                  --print "Got tab pages"
                  set iw [layout := 
                            fill $ tabs nb $  tabpages
                            ]
type TabPage = (String, Bitmap (), Layout)
--getPagesForLayout :: Notebook a -> Int -> IO ([(String, Bitmap (), Layout)])
--getPagesForLayout nb n
--    = do pmap <- getPages nb n
--         print $ "got page map for "++ (show n) ++ " pages"
        -- dbmap <- bitmapCreateDefault
         --print "created default bm"
--         return (List.map ( \ (a,b) -> tab a (widget b)) $ Map.toList pmap)
--(a,dbmap, widget b)) $ Map.toList pmap)
         
getPages :: Notebook a ->  IO (Map String ( Window ()))
getPages nb 
    = do print "getting pages "
         pnls <- WxAttributes.get nb children
         --print $ "getting text of  "++ (show $ List.length pnls)
         mnms <- mapM unsafeEvtHandlerGetClientData pnls
         let nms = List.map unjust mnms
         --print $ "Got text "++ (concat nms)
         return $ Map.fromList $ zip nms pnls

checkAndAddPages :: Notebook a -> Map String (Window ()) -> Int ->
		    [(EntryPoint, [Instruction  OurBasis])] -> IO ([ TabPage])
checkAndAddPages nb pgmap _ [] = return ([])
checkAndAddPages nb pgmap count ((ep,mem):code)
    = do let cw = Map.lookup ep pgmap
         --print $ "Current ep,mem is "++ep ++", "++ (show mem)
         case cw of
            Nothing -> 
                do --print $ "Will add a new pane for "++ep
                   newpg <- addPane nb  (ep,mem) count
                   pgs <- checkAndAddPages nb pgmap count code
                   return (newpg:pgs)
                       
            Just pn -> 
                do case   (safeCast pn classPanel) of
                    Nothing -> error "Invalid child of notebook"
                    Just pnl -> 
                       do lbs <- WxAttributes.get pn children
		          case  (safeCast (head lbs) classListBox) of
                            Nothing -> error "Invalid child of notebook panel"
                            Just lb -> do lbSetItems count ep pnl lb mem
                                          checkAndAddPages nb pgmap count code
                              

addPane :: Notebook a -> 
           (EntryPoint, [Instruction OurBasis]) -> 
           Int ->
           IO (TabPage)
--           
addPane nb (ep,ins) count
	= do p <- panel nb []
             evtHandlerSetClientData p (return ()) ep
	     lb <- singleListBox p []
             --print ("Added pane, lb for "++ep)
	     lbSetItems count ep p lb ins
             return $ tab ep $ fill $ container p $ fill $ widget lb 


lbSetItems :: Int -> 
              EntryPoint->Panel b -> 
              ListBox a -> 
              [Instruction OurBasis] -> IO ()
lbSetItems count ep p lb cm
    = do --itemsDelete lb
         let bareitems = concat $ List.map (lines . show) cm
             nums = if ("Current" == ep) then [count ..] else [0..]
             newitems = zipWith (++) (List.map lblshow nums) bareitems
         set lb [items := newitems ]

--		lbSetInstructions lb  cm


lblshow :: Int -> String
lblshow n 
  | n < 10 = ' ':' ':(show n) ++ ": "
  | n < 100 = ' ':(show n) ++ ": "
  | otherwise = (show n) ++ ": "

--lbSetInstructions :: ListBox a -> CodeMemory OurBasis -> IO ()
--lbSetInstructions lb [] = do return ()
--lbSetInstructions lb (i:ins)
--    = do itemAppend lb (show i)
--	 lbSetInstructions lb ins


unjust :: Maybe b -> b
unjust (Just b) = b
unjust (Nothing) = error "unjust: Expected Just, but got Nothing"
\end{code}
