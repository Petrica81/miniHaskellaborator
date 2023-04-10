module Main where

import System.IO
import System.Console.Isocline
import Text.ParserCombinators.Parsec(parse)

import Exp
import Eval
import Sugar
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
    input <- readline "DumiScript"
    case parse replCommand "<input>" input of
        
        Left err -> print err >> main
        Right cmd -> case cmd of
            
            Quit -> return ()
            Load _ -> putStrLn("WIP") >> main
            Eval s -> case parse exprParser "<input>" s of
                Left err -> print err >> main
                Right c -> (putStrLn . showExp . sugarExp . normalize . desugarExp $ c) >> main
                --Right c -> (putStrLn . showExp $ c) >> main