%include polycode.fmt
\subsection{Definition of the Stream data type.}\label{subsec:stream}

%if false
\begin{code}
 module Data.Stream(Stream(..),
                    StreamT(..),
                    module Data.InfList) where
 import Control.Monad
 import Control.Monad.Fix
 import Control.Monad.Trans
 import Control.Monad.State 
 import Data.InfList

 import System.IO.Unsafe(unsafePerformIO)
\end{code}
%endif

Although Haskell does not directly implement 
co-inductive types (Types defined by
destructors rather than constructors),  an equivalent structure
is definable 
due to Haskell's laziness. As discussed in \vref{subsec:inflist}, 
two  destructors  are required, |hd| and |tl|. To get this effect, 
the type defines two
constructors |strhd| and |strtl|. An instance of a |Stream| can then be
defined as the tail is lazily defined and accessed.

\begin{figure}[htbp]
{\begin{singlespace}
\begin{code}

 data Stream a = Stream {strhd :: a,
                      strtl :: Stream a}
\end{code}
\end{singlespace}
}
\caption{Haskell definitions of a Stream}\label{fig:stream}
\end{figure}

%if false
\begin{code}


 instance (Show a) => Show (Stream a) where
     showsPrec n strm = showList (takeI n strm)
     show strm = showsPrec 3 strm ""
\end{code}
%endif

{\begin{singlespace}
\begin{code}
 instance Functor Stream where
     fmap f (Stream a t) = Stream (f a) (fmap f t)

 instance Monad Stream where
     return a = Stream a (return a)
     m >>= k = Stream (strhd . k $ strhd m)
	           ((strtl m) >>= (strtl . k))
\end{code}
\end{singlespace}
}

%if false
Although the |Stream| type is interesting in and of itself, 
practically, there is a requirement to
 combine this with another type, typically, 
the |IO| type. The |StreamT| 
type becomes a monadic transformer when the type is a Monad.

{\begin{singlespace}
\begin{code}
 data StreamT m a = StreamT { strhdT :: m a,
			   strtlT :: StreamT m a}

 instance (Monad m)=>Functor (StreamT m) where
     fmap f str = StreamT (do 
		       a <- strhdT str
		       return (f a))
		       (fmap f (strtlT str))
 instance (Monad m) => Monad (StreamT m) where
     return a = StreamT (return a) (return a)
     m >>= k = StreamT (strhdT m >>= strhdT . k )
	       (strtlT m >>= strtlT . k) 

 instance (MonadPlus m) => MonadPlus (StreamT m) where
     mzero = StreamT mzero mzero
     m `mplus` n = StreamT (strhdT m `mplus` (strhdT n))
		    (strtlT m `mplus` (strtlT n))

 instance (MonadState s m) => MonadState s (StreamT m) where
	get   = lift get
	put  = lift . put

 instance MonadTrans StreamT where
	lift m = StreamT m (lift m) 

 instance (MonadIO m) => MonadIO (StreamT m) where
	liftIO = lift . liftIO

\end{code}
\end{singlespace}
}

%endif
The  instance of |IL| for |Stream| is defined.

{\begin{singlespace}
\begin{code}
 instance IL Stream where
      hd = strhd
      tl = strtl
      takeI 0 str = []
      takeI n str 
	  = strhd str : takeI (n-1) (strtl str)
      makeInfinite [] 
	  =  error "May only convert unending lists"
      makeInfinite (a:as) 
	  = Stream a (makeInfinite as)
      
      iterI f a 
	  = Stream a (iterI f ((strhd . f) a))
      pushI = Stream
      sumList il 
	  = Stream (strhd il) (fmap (+ (strhd il)) 
			    (sumList (strtl il)))
      unzipI (Stream (a,b) tail) 
	  = (Stream a (fstI tail), Stream b (sndI tail))
	    where fstI = fst . unzipI
		  sndI = snd . unzipI
      zipI (Stream a taila) (Stream b tailb) 
	  = Stream (a,b) (zipI taila tailb)
      zipWithI f (Stream a taila) (Stream b tailb) 
          = Stream (f a b) (zipWithI f taila tailb)
   
\end{code}
\end{singlespace}
}

%if false
The instance of |IL| for  |StreamT IO| is defined.
\begin{code}


 instance IL (StreamT IO) where
      hd = unsafePerformIO . strhdT
      tl = strtlT
      takeI 0 _ = []
      takeI n str 
	  = hd str : takeI (n-1) (strtlT str)
      makeInfinite [] 
	  =  error "May only convert unending lists to infinite lists"
      makeInfinite (a:as) 
	  = StreamT (do return a) (makeInfinite as)
      
      iterI f a = StreamT (do return a)
		       (iterI f ((hd . f) a))
      pushI a str = StreamT (do return a) str
      sumList il 
	  = StreamT (strhdT il) (fmap  (+ (hd il))
			      (sumList (strtlT il)))
      unzipI (StreamT a tail) 
	  = (StreamT (liftM fst a) (fstI tail), 
	     StreamT (liftM snd a) (sndI tail))
	    where fstI = fst . unzipI
		  sndI = snd . unzipI
      zipI (StreamT a taila) (StreamT b tailb) 
	  = StreamT (liftM2 pr a b) (zipI taila tailb)
	    where pr a b = (a,b)
      zipWithI f (StreamT a taila) (StreamT b tailb) 
               = StreamT (liftM2 f a b) (zipWithI f taila tailb)
      
\end{code}
%endif