\incsec{Symbol Table}
\label{incsec:symboltable}
Our symbol table structure is comprised of an associative array, matching 
the variables hash value with an index and an array. 
%if false
\begin{code}
module SymbolTable where
import Qtypes
import IrTypes
import SemTypes
import Data.List as List  (map, concat)
import Control.Monad.State
import Data.Map as Map
import Data.Char(toUpper, toLower)
import CompSupport(find,uncapitalize)
import SymbolTableGlobals
import SemanticErrors

\end{code}
%endif
\incsubsubsec{\haskfuncnoref{makeAnEmptySymbolTable}}
\label{haskellfunction:makeAnEmptySymbolTable}
\index{Compiler Functions!Symbol table!makeAnEmptySymbolTable}
Our first function creates an empty symbol table and sets our state to it.

\begin{code}
emptySymbolTables :: SemStateMonad ()
emptySymbolTables = 
    do setSymTabLinear (Map.empty)
       setSymTabClassical (Map.empty)
       addGlobalTransforms
\end{code}
\incsubsubsec{Standard Symbol Table functions.}
We follow up with the standard symbol table functions. The symbol table
lookup is encapsulated in \haskfuncdef{stlookup}{Compiler}{symbol table}, which uses the helper function 
\haskfunc{find} and in turn is used by 
\haskfuncdef{idlook}{Compiler}{symbol table}. Entries are stored as a
list of entries in a  map, with the head of the list being
the one currently in scope.

\begin{code}


removeIdLinear :: String -> SemStateMonad ()
removeIdLinear id 
    = do ste <- maybeLookup id
         case ste of 
             (Just _) -> modSymTabLinear (Map.delete id)
             Nothing -> return ()

removeIdListLinear :: [String] -> SemStateMonad ()
removeIdListLinear [] = return ()
removeIdListLinear (id:ids)
    = do removeIdLinear id
         removeIdListLinear ids

maybeLookup :: String->SemStateMonad (Maybe SymEntryLinear)
maybeLookup name
  = do symtab <- getSymTabLinear
       return $ Map.lookup name symtab


maybeLookupClassical :: String->SemStateMonad (Maybe SymEntryClassical)
maybeLookupClassical name
  = do symtab <- getSymTabClassical
       return $ Map.lookup name symtab

lookupClassicalOrLinear :: String -> 
                           SemStateMonad (Either SymEntryLinear 
                                                 SymEntryClassical)
lookupClassicalOrLinear name
    = do llin <- maybeLookup name
         case llin of
           (Just e) -> return $ Left e
           Nothing ->
               do lclassical <- maybeLookupClassical name
                  case lclassical of
                       (Just c) -> return $ Right c
                       Nothing -> fail $ notfound name
dropIfNeeded ::String->SemStateMonad ([Istmt], SymEntryLinear)
dropIfNeeded id
   = do tv <- newTypeVar
        dropWithType (TYPEVAR tv) id
        
dropWithType ::Qtype-> String->SemStateMonad ([Istmt], SymEntryLinear)
dropWithType tv id
   = do mentry <- maybeLookup id
        case mentry of 
          Just ent -> return ([Idiscard [name ent]],ent)
          Nothing -> do lvl <- getSemLvl
                        let ent =  SeVar id tv
                        addEntry (id, ent)
                        return ([], ent) --Will be created by rename.

{-
createQbitIfNeeded ::String->SemStateMonad ([Istmt], SymEntryLinear)
createQbitIfNeeded id
   = createwithTypeIfNeeded QBIT id


createIfNeeded ::String->SemStateMonad ([Istmt], SymEntry)
createIfNeeded id
   = do tv <- newTypeVar
        createwithTypeIfNeeded (TYPEVAR tv) id

createwithTypeIfNeeded ::Qtype-> String->SemStateMonad ([Istmt], SymEntry)
createwithTypeIfNeeded tv id
   = do mentry <- maybeLookup id
        case mentry of 
          Just ent -> return ([],ent)
          Nothing -> do lvl <- getSemLvl
                        let ent =  SeVar lvl id tv
                        addEntry (id, ent)
                        return ([Ialloc id lvl tv], ent)
                        
-}


classicalStlookup :: String->SemStateMonad SymEntryClassical
classicalStlookup name 
  = do sentry <- maybeLookupClassical name
       case sentry of 
	 (Just sent) -> return sent
         Nothing -> fail $ notfound name


stlookup :: String->SemStateMonad SymEntryLinear
stlookup name 
  = do sentry <- maybeLookup name
       case sentry of 
	 (Just sent) -> return sent
         Nothing -> fail $ notfound name

idlook :: String->SemStateMonad (Nodename,Qtype)
idlook id 
    = do sentry <- stlookup id
         return (name sentry, qtype sentry)

stgloballookup :: String->SemStateMonad SymEntryGlobal
stgloballookup name 
  = do symtab <- getSymTabGlobal
       let sentry = Map.lookup name symtab
       case sentry of 
	 (Just sent) -> return sent
         Nothing -> 
           let modAndNames = splitTransformMods name
               (mods, name') = (\f ->(init f, last f)) modAndNames
               sentry' = Map.lookup name' symtab
           in  case sentry' of
                  (Just sent') -> return $ appTMods mods sent'
                  Nothing -> fail $ glblNotFound name

splitTransformMods :: String -> [String]
splitTransformMods s
   = if ('-' `elem` s)
      then let (f, '-':s') = span (\ c -> c /= '-') s
               s'' = splitTransformMods s'
           in (f:s'')
      else [s]

appTMods :: [String] -> SymEntryGlobal -> SymEntryGlobal
appTMods [] se = se
appTMods ("Inv":rest) se
   = let se' = appTMods rest se
     in appInvMod se
appTMods ("C":rest) se
   = let se' = appTMods rest se
     in appCMod se

appInvMod :: SymEntryGlobal -> SymEntryGlobal
appInvMod (SeFun nm dc1 (Just g) dc2 dc3 inqb outqb dc4)
          = SeFun ("Inv-" ++ nm) dc1 (Just (Inverse g)) dc2 dc3 outqb inqb dc4

appCMod :: SymEntryGlobal -> SymEntryGlobal
appCMod (SeFun nm dc1 (Just g) dc2 dc3 inqb outqb dc4)
          = SeFun ("C-" ++ nm) dc1 (Just (Controlled g)) dc2 dc3 
                (QBIT:inqb) (QBIT:outqb) dc4



\end{code}
\incsubsubsec{Standard Symbol Table functions for bit duplication.}
The bit duplication functions \haskfuncdef{qbulookup}{Compiler}{symbol table}
 and \haskfuncdef{dumpqb}{Compiler}{symbol table} are 
based on the familiar finite map structure as the symbol table. 
The difference being that we want all of the entries
that are possibly duplicated at the moment.

\begin{code}
qbulookup :: String->SemStateMonad [SymEntryLinear]
qbulookup name 
	  = do qbitusage <- getQbitUsage
	       let qbs = find qbitusage name
	       return qbs

dumpqb :: String->SemStateMonad ()
dumpqb nm
     = do qbs <- qbulookup nm
	  liftIO $ putStrLn ("Name is '"++nm++"'")
	  liftIO $ putStrLn $ showList ( qbs) ""
	  return ()

\end{code}
\incsubsec{Adding to the symbol table.}
We add all of the entries for a scope to the symbol table at once. 
\incsubsubsec{\haskfuncnoref{addToSymTab}}
\label{haskellfunction:addToSymTab}
\index{Compiler Functions!symbol table!addToSymTab}
Modifies a symbol table by adding the given list of symbol table entries
to the current entry for an identifier.

\begin{code}
--addToSymTab :: SymbolTable->String->SymEntry->SymbolTable
--addToSymTab st key entries 
--     =Map.unionWith (++) st (Map.singleton key entries)
\end{code}
\incsubsubsec{\haskfuncnoref{addEntriesList}}
\label{haskellfunction:addEntriesList}
\index{Compiler Functions!symbol table!addEntriesList}
Working within the monad, takes a list of pairs of identifiers and corresponding
symbol table entries and adds them to the current symbol table.

\begin{code}
{-
addKeyValPairToMapC :: Ord k => (a -> a -> a) ->  Map.Map k a -> (k,a)
              -> Map.Map k a
addKeyValPairToMapC f mp (key,val) = Map.insertWith f key val mp 

addListToMapC :: Ord k => (a -> a -> a) ->  Map.Map k a -> [(k,a)]
              -> Map.Map k a
addListToMapC f mp [] = mp
addListToMapC f mp (entry:entries)
     =  addListToMapC f (addKeyValPairToMapC f mp entry) entries 
-}
addEntriesList :: [(String,SymEntryLinear)]->SemStateMonad()
addEntriesList keys
	    = modSymTabLinear (Map.union $ Map.fromList keys)

addClassicalEntriesList :: [(String,SymEntryClassical)]->SemStateMonad()
addClassicalEntriesList keys
	    = modSymTabClassical (Map.union $ Map.fromList keys)
\end{code}
\incsubsubsec{\haskfuncnoref{addEntry}}
\label{haskellfunction:addEntry}
\index{Compiler Functions!symbol table!addEntry}
Again, working withing the monad, adds a single entry to the symbol table.

\begin{code}
addEntry :: (String,SymEntryLinear)->SemStateMonad()
addEntry (key, entry) = modSymTabLinear (Map.insert key entry)

addClassicalEntry :: (String,SymEntryClassical)->SemStateMonad()
addClassicalEntry (key, entry) = modSymTabClassical (Map.insert key entry)



\end{code}

\begin{code}
addNewType ::Statement ->SemStateMonad()
addNewType (DataDeclaration dd)
    = addTypeEntry dd

addNewType _ = return ()
         

\end{code}

\incsubsec{\haskclassnoref{SymTabEntry}}
\label{haskellclass:SymTabEntry}
\index{Compiler Classes!Symbol table!SymTabEntry}
We create a class to encapsulate the chores of being able to add entries 
for each of the
syntactic data types.

\begin{code}
class SymTabEntry a where
\end{code}
\incsubsubsec{\haskfuncnoref{updateSymTab}}
\label{haskellfunction:updateSymTab}
\index{Compiler Functions!symbol table!updateSymTab}
Makes and adds a new entry.

\begin{code}
     updateSt :: a->SemStateMonad ()
     updateSt a = makeEntry a >>= addEntry

\end{code}
\incsubsubsec{\haskfuncnoref{updateSymTabList}}
\label{haskellfunction:updateSymTabList}
\index{Compiler Functions!symbol table!updateSymTabList}
Make and add entries for a list of syntactic elements.

\begin{code}
     updateStList :: [a]->SemStateMonad ()
     updateStList elts = makeEntriesList elts >>=  addEntriesList

\end{code}
\incsubsubsec{\haskfuncnoref{makeEntry}}
\label{haskellfunction:makeEntry}
\index{Compiler Functions!symbol table!makeEntry}
Actually create the entry information and the string that will 
be used to key on it.

\begin{code}
     makeEntry :: a->SemStateMonad (String, SymEntryLinear)

\end{code}
\incsubsubsec{\haskfuncnoref{makeEntries}}
\label{haskellfunction:makeEntries}
\index{Compiler Functions!symbol table!makeEntries}
Make multiple entries for a single syntactic entry. 
This can be used for those items,
such as blocks, which are containers of multiple items.


Then the process is repeated for the scope 2, where |x| will be placed in scope.
The calling of these for this program can be traced as follows:
\begin{verbatim}
makeEntries on the program
...makeEntriesList on the statements
...makeEntries on the first statement (proc)
.....makeEntry on the proc
\end{verbatim}

\begin{code}
     makeEntries ::  a->SemStateMonad [(String,SymEntryLinear)]
     makeEntries a = do (k,x) <- makeEntry a
			return [(k,x)]
\end{code}
\incsubsubsec{\haskfuncnoref{makeEntriesList}}
\label{haskellfunction:makeEntriesList}
\index{Compiler Functions!symbol table!makeEntriesList}
Default processing for  a list of entries.

\begin{code}
     makeEntriesList ::  [a]->SemStateMonad [(String,SymEntryLinear)]
     makeEntriesList [] = return []
     makeEntriesList (x:xs) = do y <-makeEntries x 
				 ys<-makeEntriesList xs
				 return (y++ys)
\end{code}
\incsubsubsec{\haskfuncnoref{qbitsUsed}}
\label{haskellfunction:qbitsUsed}
\index{Compiler Functions!symbol table!qbitsUsed}
Work with the "shadow" symbol table to set up the usage
 of \qbits for duplication searches. Similar
to the symbol tables \haskfuncnoref{makeEntry}

\begin{code}
     qbitsUsed :: a ->SemStateMonad (String, [SymEntryLinear])
\end{code}
\incsubsubsec{\haskfuncnoref{qbitsUsedList}}
\label{haskellfunction:qbitsUsedList}
\index{Compiler Functions!symbol table!qbitsUsedList}
Default processing for a list of synactic entries.

\begin{code}
     qbitsUsedList :: [a] ->SemStateMonad (String, [SymEntryLinear])
     qbitsUsedList [] = return ("",[])
     qbitsUsedList (x:xs) = do (s,y)<- qbitsUsed x
			       (ss,ys) <- qbitsUsedList xs
			       return (s++ss,y++ys)

\end{code}
\incsubsubsec{\haskfuncnoref{updateQbitdups}}
\label{haskellfunction:updateQbitdups}
\index{Compiler Functions!symbol table!updateQbitdups}
Similar to the symbol tables \haskfuncnoref{updateSt}.

\begin{code}
     updateQbitdups :: a->SemStateMonad ()
     updateQbitdups a = do (nm,ses) <- qbitsUsed a
			   addQbitUsage nm ses

\end{code}
Now, we create some useful functions for dealing with creation
of symbol tables and the IR.
\begin{code}
{-
newBlockScope :: (SymTabEntry a ) => a->(a->SemStateMonad b) 
		-> SemStateMonad (Storage, b)
newBlockScope = newScope incBlock

newProcScope :: (SymTabEntry a ) => a->(a->SemStateMonad b) 
		-> SemStateMonad (Storage, b)
newProcScope = newScope incFunc


newScope :: (SymTabEntry a ) => (Level->Level)-> a->(a->SemStateMonad b) 
		-> SemStateMonad (Storage, b)
newScope lvlup ste f 
      = do lvl <- getSemLvl
	   setSemLvl $ lvlup lvl
	   stg1 <- getStorage
	   setbits 0
	   setqbits 0
	   stab <- getSymTab
           pushSymTab stab --Copy to top of stack
	   updateSt ste
	   res <- f ste
	   setSemLvl lvl
	   --popSymTab    -- Restore old symtab -- No sense...
	   stg2 <- getStorage
	   setStorage stg1
	   return (stg2,res)

 -}
\end{code}
\incsubsec{Instances of the \haskclass{SymTabEntry}}
\label{incsec:symtabentry:instances}
We declare instances for each of the syntactic entries.

\begin{code}

addTypeEntry:: DataDefinition -> SemStateMonad ()
addTypeEntry (DD td cons)
    = do modSymTabGlobal (Map.insert (typename td) $ 
                               SeData (typename td) (typevars td)
                                          (List.map consname cons))
         addConsEntries td cons 0


addConsEntries ::TypeDefinition->[Constructor]->
                 Int->SemStateMonad ()
addConsEntries _ [] _ = return ()               
addConsEntries td (c:cs) offset
    = do addConsEntry td c offset
         addConsEntries td cs (offset+1)

addConsEntry ::TypeDefinition->Constructor->
               Int->SemStateMonad ()
addConsEntry  (TypeDefinition tid vids) (Constructor cid tvars) offset
    = modSymTabGlobal (Map.insert cid $ 
                          SeCons cid offset  tvars 
                                     [(DECLTYPE tid (List.map TYPEVAR vids))])
            
       
addTypesToSt :: [GlobalDefinition] -> SemStateMonad ()
addTypesToSt [] = return ()
addTypesToSt (gd:gds)
    = do addTypeToSt gd
         addTypesToSt gds

addTypeToSt :: GlobalDefinition -> SemStateMonad ()
addTypeToSt (ProcDef _) = return ()
addTypeToSt (DataDef dd) = addTypeEntry dd

addProcsToSt :: [GlobalDefinition] -> SemStateMonad ()
addProcsToSt [] = return ()
addProcsToSt (gd:gds)
    = do addProcToSt gd
         addProcsToSt gds
pnames :: (Procedure -> [ParameterDefinition]) ->
          Procedure -> [Nodename]
pnames f p = List.map parmId $ f p

addProcToSt :: GlobalDefinition -> SemStateMonad ()
addProcToSt (DataDef _) = return ()
addProcToSt (ProcDef pd) 
    = do cdl <- newfunclbl
         catypes <- gettypes $ inclassicalparms pd
         qatypes <- gettypes $ inquantumparms pd
         qrtypes <- gettypes $ outquantumparms pd
         crtypes <- gettypes $ outclassicalparms pd
         let  se = SeFun {cdlabel = cdl , 
			  gname = (procnm pd),
                          transform = Nothing,
                          parmnames = ((pnames inclassicalparms pd ,
                                        pnames outclassicalparms pd),
                                       (pnames inquantumparms pd,
                                        pnames outquantumparms pd)),
			  cargtypes=catypes,
			  qargtypes=qatypes,
			  qrettypes=qrtypes,
			  crettypes=crtypes}
         modSymTabGlobal (Map.insert (procnm pd) se)



instance SymTabEntry Procedure where
     makeEntry (Procedure _ _ _ _ _ _)
	 = error addingProcToLinSt

     qbitsUsed (Procedure nm _ _ _ _ stmtlist) 
	       = do (_,ses)<-qbitsUsedList stmtlist
		    return (nm,ses)

     makeEntries (Procedure nm _ qfroms _ _ _)
	 = do setargs 0
              makeEntriesList qfroms 

instance SymTabEntry ParameterDefinition where
     makeEntry (ParameterDefinition id typ)
         = do lvl <- getSemLvl
              nargs <- gets numArgs
              incargs
              return (id, SeLArg (-nargs) id typ)

     qbitsUsed (ParameterDefinition id typ)
               = error checkQbitUsageInParms

updateClassicalParm :: ParameterDefinition -> SemStateMonad ()
updateClassicalParm (ParameterDefinition id typ)
  = case typ of 
      INT -> addCparm id typ
      BOOL -> addCparm id typ
      _ -> do error "Classical parms must be of type Int or Bool only"


addCparm :: Identifier -> Qtype -> SemStateMonad()
addCparm id typ 
         =  do off <- getOffset
               decOffset
               lvl <- getSemLvl
               addClassicalEntry (id, SeCArg off lvl id typ)
updateClassicalListP :: [ParameterDefinition] -> SemStateMonad()
updateClassicalListP = mapM_ updateClassicalParm
instance SymTabEntry Statement where

		
\end{code}
Responsible for counting the number of bits as well.

|makeEntries| will add to the storage as needed and then prepare the actual
symbol table entries for addition to the symbol table. |makeEntry| will get
the information needed to build the entry and return those values.

These should actually only return something for bit and qbit allocations. 
No other statements actually affect the symbol table of any statements 
following them. \emph{Within} the statement, (e.g. and proc) there may
be changes to the symbol table, but that is to be taken care of by the
creation of the IR.
\begin{code}

     makeEntries _ = do return []
     makeEntry (Assignment id exp)
         = do lvl <- getSemLvl
              sentry <- maybeLookup id
              tv <- newTypeVar
              return $ case sentry of
                         Just se -> (id, se)
                         Nothing -> (id, 
                                       SeVar id (TYPEVAR tv)) --(gettypes exp))


     makeEntry s 
	 = fail $ illegalMakeEntStatement $ show s

     qbitsUsed (Discard _) 
	 = do return ("",[])
     qbitsUsed (CaseSt _ cc) 
	 = qbitsUsedList $ List.concat $ List.map snd cc
     qbitsUsed (DataDeclaration _) 
	 = do return ("", [])
     qbitsUsed (Measure _ s1 s2) 
	 = qbitsUsedList $  s1 ++ s2
     qbitsUsed (UseAssign  _ e) 
	 = qbitsUsed e
     qbitsUsed (Assignment  _ e) 
	 = qbitsUsed e
     qbitsUsed (BlockStatement stmtlist) 
	 = qbitsUsedList stmtlist
     qbitsUsed (Skip) 
	 = do return ("",[])
     qbitsUsed (Call nm _ args inouts _)
	 = do ses <- Prelude.mapM stlookup inouts
              eses <- qbitsUsedList args --TODONew and do what wiht this.
	      procSe <- qbulookup nm
	      return ("",subInArgsList procSe ses)

instance SymTabEntry Expression where
     makeEntry e = fail $ illegalMakeEntExp $ show e
     qbitsUsed (Eapply bo e1 e2)
        = do (_, es1) <- qbitsUsed e1
             (_, es2) <- qbitsUsed e2
             return ("", es1 ++es2)
     qbitsUsed (Enot e) = qbitsUsed e
     qbitsUsed (Evar id)
         = do se <- stlookup id
              return ("", [se])
     qbitsUsed (Ecall id ces qes ids)
         = do sids <- mapM stlookup ids
              (_,ses) <- qbitsUsedList qes
              return ("", sids++ses)
     qbitsUsed (Ebracket e) 
         = qbitsUsed e
     qbitsUsed (Econs cid es)
         = qbitsUsedList es
     qbitsUsed e = do return ("", [])


\end{code}
\incsubsubsec{\haskfuncnoref{procDefCalls}}
\label{haskellfunction:procDefCalls}
\index{Compiler Functions!Symbol Table!procDefCalls}
NOTUSED

Was used to decide upon recursive calling - now we call everything 
recursively

procDefCalls ::  [Statement] -> String-> Bool
procDefCalls  block name 
	     = foldl (\b s->b || (statementCalls name s)) False block


\incsubsubsec{\haskfuncnoref{statementCalls}}
\label{haskellfunction:statementCalls}
\index{Compiler Functions!Symbol Table!statementCalls}


statementCalls :: String->Statement -> Bool
statementCalls name (ProcResult _ nm _) = name == nm
statementCalls name (Call nm _ _) = name == nm
statementCalls name (BlockStatement blk) = procDefCalls blk name
statementCalls name (Measure _ s1 s2) =
     statementListCalls name s1 || (statementListCalls name s2)
statementCalls name (Proc (Procedure _ _ _ blk)) =
     procDefCalls blk name
statementCalls _ _ = False

statementListCalls :: String -> [Statement] -> Bool
statementListCalls nm 
   = (foldl (||) False) . (List.map (statementCalls nm))

 
\incsubsubsec{\haskfuncnoref{newfunclbl}}
\label{haskellfunction:newfunclbl}
\index{Compiler Functions!Symbol Table helper functions!newfunclbl}

\begin{code}
newfunclbl :: SemStateMonad String
newfunclbl 
     = do s <- getlabel
	  inclabel
	  return ("fcdlbl"++show s)

\end{code}
\incsubsubsec{\haskfuncnoref{gettype}}
\label{haskellfunction:gettype}
\index{Compiler Functions!Symbol Table helper functions!gettype}

\begin{code}

gettypes :: [ParameterDefinition] -> SemStateMonad [Qtype]
gettypes = mapM gettype

gettype :: ParameterDefinition ->SemStateMonad Qtype
gettype (ParameterDefinition _ t) = maketypevar t

maketypevar :: Qtype -> SemStateMonad Qtype
maketypevar (TYPEVAR _) 
    =  do tv <- newTypeVar
          return $ TYPEVAR tv

maketypevar (DECLTYPE tid ts)
    = do tvs <- mapM maketypevar ts
         return $ DECLTYPE tid tvs

maketypevar t = return t

\end{code}
