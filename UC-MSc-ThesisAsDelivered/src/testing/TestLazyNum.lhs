\incsec{A base type usable for interpreting QPL}\label{incsec:interptype}
The semantics of the language specify that we are creating
superoperators over a vector space of Complex numbers. Since computer 
representation of real numbers is fraught with rounding, we provide a
symbolic type that will allow certain values to be represented exactly,
together with a function that will compute the value at the end.
%if false
\begin{code}
module TestLazyNum where
import LazyNum
import ClassComp
\end{code}
%endif

\begin{code}

t1 = (Sfun SquareRoot Si)

t2 = t1 * (conjgt t1)

t3 = t2 * t2
\end{code}
