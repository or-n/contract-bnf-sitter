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
  input <- readFile "samplesLBNF/digit.cf"
  case LBNF.pGrammar (LBNF.myLexer input) of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (LBNF.printTree tree)
      let t = translate tree
      putStrLn (TreeSitter.printTree t)
    Bad err -> putStrLn $ "Parse error:\n" ++ err
