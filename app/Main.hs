module Main where

import ErrM

import qualified AbsLBNF as LBNF
import qualified ParLBNF as LBNF
import qualified PrintLBNF as LBNF

import qualified AbsTreeSitter as TreeSitter
import qualified ParTreeSitter as TreeSitter
import qualified PrintTreeSitter as TreeSitter

import qualified AbsRustRegex as RustRegex
import qualified ParRustRegex as RustRegex
import qualified PrintRustRegex as RustRegex

import Translate

main = do
  input <- readFile "samplesRustRegex/quote"
  case RustRegex.pGrammar (RustRegex.myLexer input) of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (RustRegex.printTree tree)
    Bad err -> putStrLn $ "Parse error:\n" ++ err
