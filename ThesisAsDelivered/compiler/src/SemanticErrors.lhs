\begin{code}
module SemanticErrors where

semanticerr = "Semantic Error: "

qToCError :: String -> String -> String
qToCError ex id =
    semanticerr ++ "Can not assign quantum data " ++ ex ++
                " to classical variable " ++ id

notfound :: String -> String
notfound i = semanticerr ++ "Variable "++i ++ " not found."

glblNotFound :: String -> String
glblNotFound nm = semanticerr ++ "Type, Cons or Proc " ++
                     nm ++ " not found."

measureNotQbit :: String -> String -> String
measureNotQbit nm typ 
     = semanticerr ++ " Measure requires a Qbit. The measure expression, '"++
         nm ++ "' is of type "++ typ

controlNotQbit :: String -> String 
controlNotQbit clist
     = semanticerr ++ " All controls must be a Qbit. The control list was " ++
         "found" ++ clist


duplicateQbitsInCall :: String ->String
duplicateQbitsInCall nm = semanticerr ++
      " The same Qbit was used in more than one place in the call to "++nm


procCallTypeError :: String -> String ->String -> String
procCallTypeError nm prmtypes exptypes
   = semanticerr ++ "Call to procedure "++nm++" with non-unifiable types." ++
       " Expected Types:" ++ prmtypes ++ "; Got types "++exptypes

semanticwarn = "Semantic Warning: " 

unbalancedCreation :: String -> String -> String
unbalancedCreation id qtype 
   = semanticwarn ++ "Unbalanced creation, discarding " ++ id ++
          " of type " ++ qtype

internalerror = "Internal Compiler Error: "

callinglblOnCons = internalerror ++ 
     "Tried to create a code label for a Constructor or Type entry"

addingProcToLinSt = internalerror ++
     "Tried to add a procedure to the linear symbol table."

illegalMakeEntStatement :: String -> String
illegalMakeEntStatement s = internalerror ++ 
     "Tried to call MakeEntry on statement : " ++ s

illegalMakeEntExp :: String -> String
illegalMakeEntExp s = internalerror ++ 
     "Tried to call MakeEntry on expression : " ++ s

notyetimp = "Feature not yet implemented: "

checkQbitUsageInParms = notyetimp ++ "Checking Qbit usage in parameters."

\end{code}
