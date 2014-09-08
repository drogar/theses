%include polycode.fmt
\subsection{Definition of the LazyNum type and operations}
\label{subsec:lazynum}
The semantics of the language QPL specify creation of
superoperators over a vector space of Complex numbers. Since computer 
representation of real numbers is fraught with rounding, this module
provides a
symbolic type that will allow certain values to be represented exactly,
together with a function that will compute the value at the end.
%if false
\begin{code}
 module Data.LazyNum(LazyNum(..),
                QOperation(..),
		QFunction(..),
                display,
                approximate
	       ) where
 import Data.Complex
 import Data.ClassComp
 import Text.Printf
\end{code}
%endif

\incsubsec{\hasktypenoref{QOperation}}\label{haskelltype:QOperation}\index{QPL Interpretor Data Types!QOperation}

{\begin{singlespace}
\begin{code}
 data QOperation = Plus | Minus | Times | Div 
		deriving (Enum, Eq)
\end{code}
\end{singlespace}}

%if false
\begin{code}
 instance Show QOperation where
     show Plus = "+"
     show Minus = "-"
     show Times = "*"
     show Div    = "/"

\end{code}
%endif

\incsubsec{\hasktypenoref{QFunction}}\label{haskelltype:QFunction}\index{QPL Interpretor Data Types!QFunction}
Functions used in Quantum values.

{\begin{singlespace}
\begin{code}
 data QFunction = SquareRoot | Exp | AbsoluteValue 
		| Signum |Negate| Conjugate 
		| Log | ExpToThe LazyNum | LogBase LazyNum
		| Sin | Cos | Tan | Asin | Acos | Atan
		| Sinh | Cosh | Tanh | Asinh | Acosh | Atanh
		      
		deriving  Eq
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 instance Show QFunction where
     show SquareRoot = "sqrt"
     show Exp = "exp"
     show AbsoluteValue = "abs"
     show Signum = "signum"
     show Negate = "-"
     show Conjugate  = "~"
     show Log = "log"
     show (ExpToThe a) = "(exp to "++(show a)++")"
     show (LogBase a) = "(log base "++(show a)++")"
     show Sin = "sin"
     show Cos = "cos"
     show Tan = "tan"
     show Sinh = "sinh"
     show Cosh = "cosh"
     show Tanh = "tanh"
     show Asin = "asin"
     show Acos = "acos"
     show Atan = "atan"
     show Asinh = "asinh"
     show Acosh = "acosh"
     show Atanh = "atanh"

\end{code}
%endif

\incsubsec{\hasktypenoref{LazyNum}}\label{haskelltype:LazyNum}\index{QPL Interpretor Data Types!LazyNum}
All possible symbolic values for a computation. A recursive type 
allowing for complex numbers, real numbers, operations and function
application and unknowns.

{\begin{singlespace}
\begin{code}
 data LazyNum = Si
	     | Snum Double
	     | Sbop QOperation LazyNum LazyNum
	     | Sfun QFunction LazyNum
	     | Svar String
	     | SZero
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 instance Eq LazyNum where
     (==) (Snum a) (Snum b) = a == b
     (==) (Snum a) (SZero) = a == 0.0
     (==) (SZero) (Snum a)  = a == 0.0
     (==) (SZero) (SZero) = True
     (==) (Svar a) (Svar b) = a == b
     (==) (Sfun f a) (Sfun g b) = a == b && f == g
     (==) (Sbop op a a') (Sbop op2 b b') = a == b && a' == b' && op == op2
     (==) (Si) (Si) = True
     (==) _ _ = False
     
\end{code}
%endif

%if false
\begin{code}
 instance Show LazyNum where
     show (Snum x) = show x
     show (Si) = "i"
     show (Sbop op p1 p2) = "("++show p1 ++  show op ++ show p2 ++")"
     show (Sfun f p) = show f ++ "("++show p ++")"
     show (Svar s) = s
     show (SZero) = "0"
\end{code}
%endif

%if false
\begin{code}
 instance Num LazyNum where
     (+) (Snum 0.0) p
	 = p
     (+) p (Snum 0.0) 
	 = p
     (+) (SZero) p
	 = p
     (+) p (SZero) 
	 = p
     (+) (Snum x) (Snum y) 
	 = Snum (x+y)
     (+) p1 (Sfun Negate p2)                
	 = p1 - p2
     (+) (Sfun Negate p2) p1                
	 = p1 - p2
     (+) (Sbop Div q1 d1) (Sbop Div q2 d2)
	 | d1 == d2 = (q1+ q2) / d1
	 | otherwise = ((q1*d2)+ (q2*d1)) / (d1*d2)
     (+) p1 p2                
	 = Sbop Plus p1 p2


     (*) (Snum 0.0) _ 
	 = SZero
     (*) _ (Snum 0.0)  
	 = SZero

     (*) (SZero) _ 
	 = SZero
     (*) _ (SZero)
	 = SZero
     (*) (Snum 1.0) p = p
     (*) p (Snum 1.0) = p
     (*) (Snum x) (Snum y) 
	 = Snum (x*y)
     (*) (Sfun Negate x) y
	 = negate (x*y)
     (*) x (Sfun Negate y)
	 = negate (x*y)
     (*) (Sfun SquareRoot x) 
	     (Sfun SquareRoot y) 
	 | x == y = x
	 | otherwise = (Sfun SquareRoot (x*y))

     (*) (Si)  (Si)  
	 = (Snum (-1.0))

     (*) (Sbop Div x1 y1) (Sbop Div x2 y2)
	 = (x1 *x2) / (y1*y2)

     (*) (Sbop Div x1 y1)  x2
	 = (x1 *x2) / y1

     (*)  x2 (Sbop Div x1 y1)
	 = (x1 *x2) / y1

     (*) p1 p2 
	 = Sbop Times p1 p2

     (-) (Snum x) (Snum y) 
	 = Snum (x-y)
     (-) p1 (Sfun Negate p2)                
	 = p1 + p2
     (-) (Sfun Negate p2) p1                
	 = negate (p1 + p2)

     (-) (Sbop Div q1 d1) 
	     (Sbop Div q2 d2)
	 | d1 == d2 = (q1- q2) / d1
	 | otherwise = ((q1*d2)- (q2*d1)) / (d1*d2)

     (-) p1 p2
	 = Sbop Minus p1 p2
	   
     negate (SZero) = SZero
     negate (Snum 0.0) = SZero
     negate (Snum x) 
	    = (Snum (-x))

     negate (Sfun Negate x) 
	    = x
	      
     negate (Sbop Div q1 d1) 
	    =  Sbop Div (negate q1)  d1

     negate (Sbop Times q1 d1) 
	    =  Sbop Times (negate q1)  d1

     negate (Sbop Plus q1 d1) 
	    =  Sbop Plus (negate q1) (negate d1)

     negate (Sbop Minus q1 d1) 
	    =  d1 - q1

     negate p1
	    = (Sfun Negate p1)

  
     abs (SZero) = SZero
     abs (Snum 0.0) = SZero
     abs (Snum x) 
	 = Snum (abs x)
     abs (Sfun Negate x) 
	 = abs x
     
     abs p             
	 = Sfun AbsoluteValue p

     signum (SZero) = signum 0
     signum (Snum x) 
	 = Snum (signum x)
     signum p 
	 = Sfun Signum p

     fromInteger 0 = SZero
     fromInteger i 
	 = Snum (fromInteger i)

 instance Fractional LazyNum where
     (/) (Snum x) (Snum y) 
	 = Snum (x/y)
     (/) (Sfun Negate x) y
	 = negate (x/y)
     (/) y (Sfun Negate x) 
	 = negate (y/x)
     (/) (Sfun SquareRoot x) 
	     (Sfun SquareRoot y)
	 = (Sfun SquareRoot (x / y))
     (/) (Sbop Div x1 y1)  x2
	 =  x1 / (x2 * y1)
     (/)  x2 (Sbop Div x1 y1)
	 =  (y1 * x2) / x1
     (/) (SZero) _ = SZero
     (/) _ (SZero) = error "Division by SZero"

     (/) q1 q2 
	 | q1 == q2 = Snum (1.0)
	 | otherwise =  Sbop Div q1 q2


     fromRational r 
	 = Snum (fromRational r)

 instance Floating LazyNum where
     pi = Snum pi
     exp a = Sfun Exp a
     log a = Sfun Log a
     sqrt a = Sfun SquareRoot a
     (**) a b = Sfun (ExpToThe b) a
     logBase a b = Sfun (LogBase b) a
     sin a = Sfun Sin a
     cos a = Sfun Cos a
     tan a = Sfun Tan a
     asin  a = Sfun Asin a
     acos a = Sfun Acos a
     atan a = Sfun Atan a
     sinh a = Sfun Sinh a
     cosh a = Sfun Cosh a
     tanh a = Sfun Tanh a
     asinh a = Sfun Asinh a
     acosh a = Sfun Acosh a
     atanh a = Sfun Atanh a

 isPosReal :: LazyNum -> Bool
 isPosReal (Snum x) 
     = x >= 0.0
 isPosReal (Si) 
     = False
 isPosReal (Svar _) 
     = False
 isPosReal (Sbop Plus _ _ )
     = False
 isPosReal (Sbop Minus _ _ ) 
     = False
 isPosReal (Sbop Times p q) 
     = (isPosReal p && (isPosReal q)) ||
       not (isPosReal p && (isPosReal q)) 
 isPosReal (Sbop Div p q ) 
     = (isPosReal p && (isPosReal q)) ||
       not (isPosReal p && (isPosReal q))      
 isPosReal (Sfun SquareRoot p ) 
     = isPosReal p 
 isPosReal (Sfun Exp q) 
     = isPosReal q
 isPosReal (Sfun AbsoluteValue q) 
     = True
 isPosReal (Sfun Signum q) 
     = isPosReal q
 isPosReal (Sfun Negate q) 
     = not $ isPosReal q
 isPosReal (Sfun Conjugate q) 
     = isPosReal q

 isPosReal (SZero) = False
 isPosReal (Sfun _ _) = False -- Just for completeness 


 isReal :: LazyNum -> Bool
 isReal (SZero) = True
 isReal (Snum _) 
     = True
 isReal (Si) 
     = False
 isReal (Svar _) 
     = False
 isReal (Sbop _ p q) 
     = (isReal p) && (isReal q)
 isReal (Sfun SquareRoot (Snum x)) 
     = x >= 0.0
 isReal (Sfun SquareRoot p ) 
     = isReal p && (isPosReal p)
 isReal (Sfun Exp q) 
     = isReal q
 isReal (Sfun AbsoluteValue q) 
     = True
 isReal (Sfun Signum q) 
     = isReal q
 isReal (Sfun Negate q) 
     = isReal q
 isReal (Sfun Conjugate q) 
     = isReal q
 isReal (Sfun _ q) = isReal q -- Just for completeness 

 instance Comp LazyNum where
     conjgt (SZero) = SZero
     conjgt  (Snum 0.0) 
	 = SZero
     conjgt  (Snum x) 
	 = (Snum x)
     conjgt (Si) 
	 = (Sfun Negate Si)
     conjgt p | isReal p  =  p
	      | otherwise  
		  = Sfun Conjugate p
     sqrtMinusOne = Si

\end{code}
%endif

\incsubsec{The \haskfuncnoref{approximate} function}\label{insec:approximate}
When completed processing, the results may be approximated. 
Note that  more processing on the symbolic values could first be done
 to come to a 
closer result. For example, squaring a square root value or take the log
of an exponential could be cancelled out.

\index{QPL Interpretor Functions!approximate}
This allows us to compute a numeric value for any 
|LazyNum| that does not contain an unknown. 

{\begin{singlespace}
\begin{code}
 approximate :: LazyNum -> Complex Double
 approximate (SZero)  = 0:+0
 approximate (Snum x) = x:+0
 approximate (Si) = 0:+1
 approximate (Sbop Plus p1 p2) = (approximate p1) + (approximate p2)
 approximate (Sfun (ExpToThe b) p) = (approximate p) ** (approximate b)
 approximate (Sfun (LogBase b) p)  = logBase (approximate p) (approximate b)
 approximate (Sfun Log a)        = log (approximate a)
 approximate (Svar _) = 1.0
 approximate (Sfun SquareRoot p) = sqrt(approximate p)
\end{code}
\end{singlespace}
}

%if false
\begin{code}
 approximate (Sbop Minus p1 p2)= (approximate p1) - (approximate p2)
 approximate (Sbop Times p1 p2)= (approximate p1) * (approximate p2)
 approximate (Sbop Div p1 p2)  = (approximate p1) / (approximate p2)
 approximate (Sfun Exp p)      = exp(approximate p)
 approximate (Sfun AbsoluteValue p) = abs(approximate p)
 approximate (Sfun Signum p)   = signum(approximate p)
 approximate (Sfun Negate p)   = negate(approximate p)
 approximate (Sfun Conjugate p)= conjugate(approximate p)
 approximate (Sfun Sin p)       = sin(approximate p)
 approximate (Sfun Cos p)       = cos(approximate p)
 approximate (Sfun Tan p)       = tan(approximate p)
 approximate (Sfun Asin p)      = asin(approximate p)
 approximate (Sfun Acos p)      = acos(approximate p)
 approximate (Sfun Atan p)      = atan(approximate p)
 approximate (Sfun Sinh p)      = sinh(approximate p)
 approximate (Sfun Cosh p)      = cosh(approximate p)
 approximate (Sfun Tanh p)      = tanh(approximate p)
 approximate (Sfun Asinh p)     = asinh(approximate p)
 approximate (Sfun Acosh p)     = acosh(approximate p)
 approximate (Sfun Atanh p)     = atanh(approximate p)
\end{code}
%endif

\incsubsec{\haskfunctionnoref{qprob}}
\label{haskellfunction:qprob}
\index{QPL Interpretor Functions!qprob}
Convert a number into a |LazyNum|.

{
\begin{singlespace}
\begin{code}
 qprob :: Double -> LazyNum
 qprob x = Snum x


 display::Int -> LazyNum -> String
 display digits ln =
     let c = approximate ln
         re = realPart c
         im = imagPart c
         prec = '.':(show digits)
     in if (abs im < 0.0001) 
        then printf ('%':prec++"g") re
        else printf ('%':prec++"g+"++"%"++prec++"g%s") re im "i"
\end{code}
\end{singlespace}
}

