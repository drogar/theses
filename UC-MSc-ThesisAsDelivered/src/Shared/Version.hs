module Shared.Version (Version(..), Stability(..),
                       unVers, version, simulatorVersion,
                       currentVersion)
  where
import Data.Char

data Version = Version {majorVersion :: Int,
                        minorVersion :: Int,
                        dotRelease   :: Int,
                        stability    :: Stability}
             deriving Eq
instance Show Version where
   show (Version mj mi dr st) = 
       "Major Version: "++(show mj) ++ 
                      "\nMinor Version: " ++ (show mi) ++
                      "\nDot Release  : " ++ (show dr) ++
                      "\nStability    : " ++ (show st)


data Stability = Experimental | Alpha | Beta | ReleaseCandidate | Production | Unknown
     deriving (Eq, Ord, Enum, Show, Read)

unVers :: String -> Version
unVers [] = Version 0 0 0 Unknown
unVers ('V':'e':'r':'s':'i':'o':'n':'=':rest)
    = let (mj,rest1) = getvnumber rest
          (mi,rest2) = getvnumber rest1
          (dr,rest3) = getvnumber rest2
          st = getStability rest3
      in Version mj mi dr st
unVers (c:cs) = unVers cs

getvnumber :: String -> (Int, String)
getvnumber [] = (0,[])
getvnumber s@(fs:ss) 
   | isDigit fs = let (n,rest) = span isDigit s
                  in (read n,rest)    
   | otherwise = getvnumber ss

getStability :: String -> Stability
getStability [] = Unknown
getStability ('E':_) = Experimental
getStability ('A':_) = Alpha
getStability ('B':_) = Beta
getStability ('R':_) = ReleaseCandidate
getStability ('P':_) = Production
getStability ('U':_) = Unknown
getStability (c:cs) = getStability cs


version :: String
version = programmingLang ++ (vinfo "")

simulatorVersion :: String
simulatorVersion =  showVersion currentVersion ""

vinfo =  showString ", Version=" .  (showVersion currentVersion)

showVersion v = showsPrec 0 (majorVersion v) .
   showDot . 
   showsPrec 0 (minorVersion v) . 
   showDot . 
   showsPrec 0 (dotRelease v) .
   showString " (" . 
   showsPrec 0 (stability v) .
   showString ")"

showDot = showChar '.'

programmingLang = "Linear QPL Compiler"

simname = "Simulator for Linear QPL"

currentVersion = Version 0  6 2 Experimental

-- Version 0.6.1 - Change rotation matrix to a 2x2 1&0 \\ 0 & e^{2i\pi/2^n}
-- Version 0.6.2 - Change "type" to "qdata" in sytax of data type constructions.