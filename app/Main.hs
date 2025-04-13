module Main where

-- import AbsLBNF
-- import ParLBNF
-- import ErrM
-- import PrintLBNF

-- import AbsTreeSitter
-- import ParTreeSitter
-- import ErrM
-- import PrintTreeSitter

import AbsRustRegex
import ParRustRegex
import ErrM
import PrintRustRegex

main = do
  input <- readFile "samplesRustRegex/quote"
  case pRustRegexGrammar (myLexer input) of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (printTree tree)
    Bad err -> putStrLn $ "Parse error:\n" ++ err
