\incsec{Types used in Syntactical Analysis of QPL}\label{incsec:qtypes}
\CodeResetNumbers
%if false
\begin{code}
 module Qtypes where 
 import Numeric(showInt)
 import Data.Either

\end{code}
%endif
\incsubsec{Unitary Transformations}\label{incsec:unitary}
The following type,
\hasktypedef{UnitaryTransform}{Compiler}{syntax} 
is used in both
the lexing and the parsing stages. Durin lexing, we may create 
occurences of all of the  transformations with the exception of the 
\haskcons{UTTensor}, which is only created during the parse 
phase. Note that if 
a tensor combination of transforms was considered controlled, the 
show would not appear sensible. Currently, 
we do not  use construction of 
\haskcons{Controlled} with anything but the four basic gates.

\CodeResetNumbers
\begin{code}
 data UnitaryTransform
     = Ident Int| NotGate | Tgate | Phase |  Hadamard | RhoX |
       RhoY | RhoZ | Swap | Toffoli | Rotate| UM | -- for order find
       Controlled UnitaryTransform |
       Inverse UnitaryTransform |
       UTTensor UnitaryTransform UnitaryTransform
                deriving Eq
\end{code}
\CodeContinueNumbers
%if false
\begin{code}

 instance Show UnitaryTransform where
     show (Ident 1) = "I"
     show (Ident n) = "I"++show n
     show NotGate   = "Not"
     show Tgate     = "T"
     show Phase     = "Phase"
     show Hadamard  = "Had"
     show RhoX  = "RhoX"
     show RhoY  = "RhoY"
     show RhoZ  = "RhoZ"
     show Swap  = "Swap"
     show Toffoli  = "Toffoli3"
     show Rotate  = "Rotate"
     show UM = "UM"
     show (Controlled u) = "C"++(show u)
     show (Inverse u) = "I"++(show u)
     show (UTTensor u1 u2) = (show u1) ++ "\\o*" ++ show u2
\end{code}
%endif
\incsubsec{Abstract Syntax Types}\label{incsec:absyntypes}
The following are the types primarily used for parsing. These hold the 
information for the syntactic constructions as per each name.
\incsubsubsec{\hasktypenoref{Identifier}}
\label{haskelltype:Identifier}
\index{Compiler Data Types!lexing and parsing!Identifier}
This alias type is for self documentation of this code.
\CodeContinueNumbers
\begin{code}
 type Identifier = String
 type TypeIdentifier = String
 type ConsIdentifier = String
\end{code}
\incsubsubsec{\hasktypenoref{Bitvalue}}
\label{haskelltype:Bitvalue}
\index{Compiler Data Types!lexing and parsing!Bitvalue}
This shows we work only with zeros and ones.
\CodeContinueNumbers
\begin{code}
 data Bitvalue = Zero | One
	       deriving (Show, Eq, Enum)
\end{code}

\incsubsubsec{\hasktypenoref{Program}}
\label{haskelltype:Program}\index{Compiler Data Types!lexing and parsing!Program}
Content of a parsed program.
\CodeContinueNumbers
\begin{code}

 data Program = Program [GlobalDefinition]
		deriving (Show, Eq)

 data GlobalDefinition = DataDef DataDefinition |
       ProcDef Procedure
--  | TransDef TransformDefinition
           deriving (Show, Eq)

 filterProcs :: [GlobalDefinition] -> [Procedure]
 filterProcs [] = []
 filterProcs ((DataDef _):gds) = filterProcs gds
 filterProcs ((ProcDef pd):gds) = pd:(filterProcs gds)

 data DataDefinition = DD TypeDefinition [Constructor]
           deriving (Show, Eq)

 data TypeDefinition = 
     TypeDefinition {typename :: TypeIdentifier,
                     typevars :: [Identifier]}
           deriving (Show, Eq)

 data Constructor = 
     Constructor {consname :: ConsIdentifier,
                  consargtypes :: [Qtype]}
           deriving (Show, Eq, Ord)

 data Procedure = Procedure {procnm :: Identifier,
    inclassicalparms :: [ParameterDefinition],
    inquantumparms :: [ParameterDefinition],
    outquantumparms :: [ParameterDefinition],
    outclassicalparms :: [ParameterDefinition],
    procstmts :: [Statement]}
           deriving (Show, Eq)

 data ParameterDefinition 
     = ParameterDefinition  {parmId :: Identifier,
          parmType :: Qtype}
           deriving (Show, Eq)
\end{code}
\incsubsubsec{\hasktypenoref{Statement}}
\label{haskelltype:Statement}\index{Compiler Data Types!lexing and parsing!Statement}
The type that holds the information about parsed statements. See 
\fullref{incsec:parser:grammar} for how these are created.
they 
\CodeContinueNumbers
\begin{code}
 data Statement 
     =  CaseSt Expression [(CaseClause, [Statement])]
       | Measure Expression [Statement] [Statement]
       | DataDeclaration DataDefinition
       | Assignment Identifier Expression
       | UseAssign Identifier Expression
       | Call Identifier [Expression] [Expression] [Identifier] [Identifier]
       | Discard [Identifier]
       | Use [Identifier] [Statement]
       | Guard [GuardClause]
       | UseFromHere [Identifier]
       | BlockStatement [Statement]
 --      | Proc Procedure
       | Skip
       | ZeroStack
       | ControlledBy Statement [Identifier]
         deriving (Show,Eq)

 data GuardClause = GuardClause Expression [Statement]
                  deriving (Show,Eq)
 data CaseClause = CaseClause ConsIdentifier [Identifier]
           deriving (Show, Eq)

\end{code}

The expression data type contains permissible expressions. 
The types of expressions stored below are:
\bd

\item{Eapply:} An operation such as addition or logical AND.
\item{Evar:} A variable. May be a bit or an int.
\item{Enum:} A constant Integer.
\item{Ebit:} A constant bit value.
\ed

As well, we currently only allow two types of values: Integer and Double.


\begin{code}
 data Expression =  Eapply BinOp Expression Expression
	 | Enot Expression
	 | Evar Identifier
         | Ecall Identifier [Expression] [Expression] [Identifier]
         | Enum Int
         | Ebool Bool
  --       | Ebit Bitvalue
         | EQbit Bitvalue
	 | Ebracket Expression
         | Econs ConsIdentifier [Expression]
    deriving Eq

 instance Show Expression where
  show (EQbit b) = "|"++(show b) ++ ">"
  show (Ebracket e) = "("++ showsPrec 0 e ")"
  show (Econs c es) = c++"("++showList es ")"
  show (Ebool True) = "true"
  show (Ebool False) = "false"
  show (Enum i) = show i
  show (Ecall nm ces qes ids) = 
      nm++"(" ++  (showList ces $ " | " ++ 
                   (showList qes $ " ; " ++ 
                    (showList ids ")")))
  show (Evar id) = id
  show (Enot e) = "~"++(show e)
  show (Eapply bop e1 e2) = showsPrec 0 e1 $
                            showsPrec 0 bop $
                            show e2

  showList [] = id
  showList (e1:[]) = showsPrec 0 e1
  showList (e2:es) = showList es . (showsPrec 0 e2)

                                
\end{code}
The operation type is a set of enumerated values, corresponding to the
possible tokens.

%if false
DO NOT REORDER THESE - semantic checking depends on using the ENUM 
values from these to determine types.
%endif
\begin{code}
 data BinOp = Add | Sub | Mul | Div | Mod | Rem | Negate |
              Oplshift | Oprshift |
              And | Or  | Xor | NotOp |
              Opeq | Opneq | Oplt | Opgt | Ople | Opge  
        deriving (Eq, Enum)


 instance Show BinOp where
     show Add = "+"
     show Sub = "-"
     show Mul = "*"
     show Div = "/"
     show Mod = "%"
     show Rem = "%/"
     show Negate = "--"
     show And = "&&"
     show Or  = "||"
     show Xor = "^"
     show NotOp = "~"
     show Opeq = "=="
     show Opneq = "=/="
     show Oplt = "<"
     show Opgt = ">"
     show Ople = "=<"
     show Opge = ">="
     show Oplshift = "<<"
     show Oprshift = ">>"


\end{code}
\incsubsec{Semantic support}\label{incsec:semsupport}
\incsubsubsec{\hasktypenoref{Qtype}}
\label{haskelltype:Qtype}\index{Compiler Data Types!lexing and parsing!Qtype}
The type \hasktypenoref{Qtype} is used in the semantic analysis phase as the 
description of a variables type. Currently, we only have two basic types in
a QPL program: \bit\ and \qbit.
\CodeResetNumbers
\begin{code}
 data Qtype = BIT | QBIT |INT | BOOL |
            TYPEVAR Identifier |
            DECLTYPE TypeIdentifier [Qtype] |
            Qproc {arguments::[Qtype],
			     results::[Qtype]}
	    deriving (Eq, Show, Ord)


\end{code}
\incsubsec{Declaration Level}\label{incsec:level}
\incsubsubsec{\hasktypenoref{Level}}
\label{haskelltype:Level}\index{Compiler Data Types!lexing and parsing!Level}
Each identifier is at a specific scope level within the program. This
is determined by incrementing the level for each function or The declarations have two different level counts, function and block.
\CodeResetNumbers
\begin{code}
 data Level = Lvl {functionLevel::Int,
		   blockLevel::Int,
                   caseLevel :: Int,
                  useLevel :: Int}
     deriving (Eq,Ord)
\end{code}
\incsubsubsec{\haskfuncnoref{incFunc}}\label{haskellfunction:incFunc}\index{Compiler Functions!lexing and parsing!incFunc}
Increments the function level.
\CodeContinueNumbers
\begin{code}
 incFunc :: Level->Level
 incFunc l = l{functionLevel = 1 + functionLevel l}

\end{code}
\incsubsubsec{\haskfuncnoref{decFunc}}\label{haskellfunction:decFunc}\index{Compiler Functions!lexing and parsing!decFunc}
Decrements the function level.
\CodeContinueNumbers
\begin{code}
 decFunc :: Level->Level
 decFunc  l = l{functionLevel = (functionLevel l) - 1}

\end{code}
\incsubsubsec{\haskfuncnoref{incBlock}}\label{haskellfunction:incBlock}\index{Compiler Functions!lexing and parsing!incBlock}
Increments the block level.
\CodeContinueNumbers
\begin{code}
 incBlock :: Level->Level
 incBlock l = l{blockLevel = (blockLevel l) + 1}

 incCase :: Level->Level
 incCase l = l{caseLevel = (caseLevel l) + 1}


 incUse :: Level->Level
 incUse l = l{useLevel = (useLevel l) + 1}


\end{code}
\incsubsubsec{\haskfuncnoref{decBlock}}\label{haskellfunction:decBlock}\index{Compiler Functions!lexing and parsing!decBlock}
Decrements the block level.
\CodeContinueNumbers
\begin{code}
 decBlock :: Level->Level
 decBlock l = l{blockLevel = (blockLevel l) - 1}

 decCase :: Level->Level
 decCase l = l{caseLevel = (caseLevel l) - 1}

 decUse :: Level->Level
 decUse l = l{useLevel = (useLevel l) - 1}

 insideUse :: Level -> Bool
 insideUse (Lvl _ _ _ u) = u > 0

\end{code}
%if false
\CodeContinueNumbers
\begin{code}

 instance Show Level where
     showsPrec _ (Lvl f b c u) = showString "(f:" . showInt f . 
           showString ", b:" . showInt b .
           showString ", c:" . showInt c .
           showString ", u:" . showInt u . showChar ')' 


\end{code}
%endif