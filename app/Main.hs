module Main where

import AbsLBNF
import ParLBNF
import ErrM
import PrintLBNF

main :: IO ()
main = do
  input <- readFile "LBNF.cf"
  case pGrammar (myLexer input) of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (printTree tree)
    Bad err -> putStrLn $ "Parse error:\n" ++ err
