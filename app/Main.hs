module Main where

import qualified AbsLBNF as LBNF
import qualified ParLBNF as LBNF
import qualified ErrM as LBNF
import qualified PrintLBNF as LBNF

import qualified AbsTreeSitter as TreeSitter
import qualified ParTreeSitter as TreeSitter
import qualified ErrM as TreeSitter
import qualified PrintTreeSitter as TreeSitter

import qualified AbsRustRegex as RustRegex
import qualified ParRustRegex as RustRegex
import qualified ErrM as RustRegex
import qualified PrintRustRegex as RustRegex

main = do
  input <- readFile "samplesRustRegex/quote"
  case RustRegex.pRustRegexGrammar (RustRegex.myLexer input) of
    RustRegex.Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (RustRegex.printTree tree)
    RustRegex.Bad err -> putStrLn $ "Parse error:\n" ++ err
