module Main where

import ErrM
import Data.List (lookup)

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

samplesLBNF =
  [ ("digit", "digit.cf")
  , ("const", "const.cf")
  ]

samplesTreeSitter =
  [ ("minimal", "minimal.js")
  , ("quote", "quote.js")
  , ("digit", "digit.js")
  , ("const", "const.js")
  ]

data Input = LBNF | TreeSitter

main = go LBNF "const"

go inputType sampleId = do
  let dirLBNF = "samplesLBNF/"
  let dirTreeSitter = "samplesTreeSitter/"
  case inputType of
    LBNF -> do
      let Just sample = lookup sampleId samplesLBNF
      input <- readFile $ dirLBNF <> sample
      case lbnf input of
        Ok tree -> do
          putStrLn "Parsed successfully!"
          putStrLn (LBNF.printTree tree)
          let t = translate tree
          let str = TreeSitter.printTree t
          putStrLn str
          writeFile (dirTreeSitter <> sampleId <> ".js") str
        Bad err -> putStrLn $ "Parse error:\n" ++ err
    TreeSitter -> do
      let Just sample = lookup sampleId samplesTreeSitter
      input <- readFile $ dirTreeSitter <> sample
      case treeSitter input of
        Ok tree -> do
          putStrLn "Parsed successfully!"
          putStrLn (TreeSitter.printTree tree)
        Bad err -> putStrLn $ "Parse error:\n" ++ err
