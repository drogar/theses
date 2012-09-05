\begin{code}
module TestMatrix where
import Data.Matrix
import Data.LazyNum


c1 :: Matrix LazyNum
c1 =  [[Svar "a", Svar "b"], [Svar "c", Svar "d"]]

c3 :: Matrix LazyNum
c3 =  [[Svar "e", Svar "f"], [Svar "g", Svar "h"]]

c4 :: Matrix LazyNum
c4 =  [[Svar "i", Svar "j"], [Svar "k", Svar "l"]]

c2 :: Matrix LazyNum
c2 =  [[Svar "q11", Svar "q12"], [Svar "q21", Svar "q22"]]

cb = [[c1, c3],[c4,c2]]


\end{code}
