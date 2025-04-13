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

lbnf = LBNF.pGrammar . LBNF.myLexer
treeSitter = TreeSitter.pGrammar . TreeSitter.myLexer

main = do
  -- input <- readFile "samplesLBNF/digit.cf"
  -- input <- readFile "samplesTreeSitter/quote.js"
  input <- readFile "samplesTreeSitter/digit.js"
  case treeSitter input of
    Ok tree -> do
      putStrLn "Parsed successfully!"
      putStrLn (TreeSitter.printTree tree)
      -- let t = translate tree
      -- let str = TreeSitter.printTree t
      -- putStrLn str
      -- writeFile "samplesTreeSitter/digit.js" str
    Bad err -> putStrLn $ "Parse error:\n" ++ err
