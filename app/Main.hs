module Main where

-- import AbsLBNF
-- import ParLBNF
-- import ErrM
-- import PrintLBNF

import AbsTreeSitter
import ParTreeSitter
import ErrM
import PrintTreeSitter

main :: IO ()
main = do
  input <- readFile "samplesTreeSitter/quote.js"
  case pTreeSitterGrammar (myLexer input) of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (printTree tree)
    Bad err -> putStrLn $ "Parse error:\n" ++ err
