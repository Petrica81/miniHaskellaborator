
module Printing (showExp) where

import Exp
import Data.List (intercalate)
import Data.Void (vacuous)
import Parsing (natExp)

showVar :: Var -> String
showVar v = show v

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
showExp (CLam v cexp) = "\\" ++ showVar v ++ "->" ++ showExp cexp 
showExp (CApp c1 c2) = "(" ++ showExp c1 ++ " " ++ showExp c2 ++ ")"
showExp (Let v c1 c2) = "(let " ++ showVar v ++ ":=" ++ showExp c1 ++ "in" ++ showExp c2 ++ ")"
showExp (LetRec v c1 c2) = "(letRec " ++ showVar v ++ ":=" ++ showExp c1 ++ "in" ++ showExp c2 ++ ")"
showExp (List l) = "[]" ++  intercalate "," (map showExp l) ++ "]"
