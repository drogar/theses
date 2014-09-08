\incsec{Types for Intermediate representation}
\label{inscec:irtypes}
%if false
\begin{code}
 module IrTypes where
 import Qtypes 
 import Data.Map as Map



\end{code}
\begin{code}

 type Storage = Map Qtype Int
 type Nodename = String

\end{code}

\incsec{Program IR}
\label{incsec:irprogram}
Semantically, a program is either a complete program which can be represented
as an amount of storage and a list of statments, or it is an external
procedure. The external procedure is more complex, requiring a code label
and a type signature in addition to the storage and list of statements.
\incsubsubsec{\hasktypenoref{Iprog}}
\label{haskelltype:Iprog}\index{Compiler Data Types!Intermediate Representation!Iprog}
The \hasktypenoref{Iprog} type will hold the intermediate representation of 
a program. We have three possibilities. An external procedure, constructed by
\haskcons{Iextproc}, a recursive external procedure, constructed by 
\haskcons{Irecextproc} and a standard program, constructed by
\haskcons{IprgBlock}.
\CodeResetNumbers
\begin{code}
 data Iproc = Iproc String String  [Qtype] [Qtype] 
                  [Qtype] [(Int, Qtype)]   [Istmt]
        deriving (Eq, Show)     
 newtype Iprog =  Iprog (Map Identifier Iproc)
      deriving (Eq, Show)


\end{code}
%if false
\begin{code}

\end{code}
%endif
\incsubsubsec{\hasktypenoref{Istmt}}
\label{haskelltype:Istmt}\index{Compiler Data Types!Intermediate Representation!Istmt}
At the bottom level, practically all constructions in a QPL program can
be represented as statements. The IR for statements closely reflects 
the syntax tree for statements. Names of identifiers are replaced by an
offset and level. Assignments are replaced by the \haskcons{Iassign}
constructor. A "new" is replaced by a \haskcons{Iskip} as no 
generated statements or code are needed, just space allocation.

\begin{code}

 data Istmt = Iassign Nodename (Either IrExpression IrExpression)
            | IClassicalAssign Nodename Int IrExpression
            | IuseAssign Nodename (Either IrExpression IrExpression)
            | Iuse [Nodename] [Istmt]
            | Iguard [(IrExpression, [Istmt])]
	    | Imeas  IrExpression  [Istmt]  [Istmt] 
	    | Icase  IrExpression [IrCaseClause]
	    | Icall (Maybe UnitaryTransform) 
                  String  (([Identifier], [Identifier]), 
                           ([Identifier], [Identifier]))
                  [IrExpression]  
                  [IrExpression]  
                  [(Nodename,Qtype)] 
                  [(Nodename,Qtype)]
            | Idiscard [Nodename]
	    | Ialloc Nodename Qtype
	    | Iblock [Istmt]
	    | Iskip
            | IzeroStack
            | IcontrolledBy [Istmt] [Nodename]
	      deriving (Eq, Show)

 data IrCaseClause 
     = IrCaseClause ConsIdentifier [Nodename] [Istmt]
	      deriving (Eq, Show)

 data IrExpression =  Apply BinOp IrExpression IrExpression
         | IrNot IrExpression
         | IrVar Nodename Qtype
         | IrCvar Nodename Int Level Qtype
         | IrExpCall String   (([Identifier], [Identifier]), 
                           ([Identifier], [Identifier]))
                [IrExpression]
                [IrExpression] [(Nodename, Qtype)]
         | IrBool Bool
         | IrQbit Bitvalue
         | IrCons ConsIdentifier [IrExpression]
         | IrNum Int
    deriving (Show,Eq)




\end{code}

\begin{code}

 appendStatementsToClause :: [Istmt] -> IrCaseClause -> IrCaseClause
 appendStatementsToClause stmts (IrCaseClause cid nn s) = IrCaseClause cid nn $ s ++ stmts
 sinc :: Qtype -> Int -> Storage -> Storage
 sinc = Map.insertWith (+)

 bitsOfStorage :: Storage -> Int
 bitsOfStorage = findWithDefault 0 BIT

 qbitsOfStorage :: Storage -> Int
 qbitsOfStorage = findWithDefault 0 QBIT

 setStore :: Qtype -> Int -> Storage -> Storage
 setStore ktype i = update (const (Just i)) ktype
\end{code}
