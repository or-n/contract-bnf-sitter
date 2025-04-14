module Main where

import Data.Maybe (fromMaybe)

import qualified AbsLBNF as LBNF
import qualified ParLBNF as LBNF
import qualified PrintLBNF as LBNF

import qualified AbsTreeSitter as TreeSitter
import qualified ParTreeSitter as TreeSitter
import qualified PrintTreeSitter as TreeSitter

-- import qualified AbsRustRegex as RustRegex
-- import qualified ParRustRegex as RustRegex
-- import qualified PrintRustRegex as RustRegex

import Translate

lbnf = LBNF.pGrammar . LBNF.myLexer
treeSitter = TreeSitter.pGrammar . TreeSitter.myLexer

dirLBNF = "samplesLBNF/"
dirTreeSitter = "samplesTreeSitter/"

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

main = go "const" LBNF

dir = \case
  LBNF -> "samplesLBNF/"
  TreeSitter -> "samplesTreeSitter/"

class Grammar a where
  printTree :: a -> String

instance Grammar LBNF.Grammar where
  printTree = LBNF.printTree

instance Grammar TreeSitter.Grammar where
  printTree = TreeSitter.printTree

go sampleId = \case
  LBNF -> do
    let sample = fromMaybe "" $ lookup sampleId samplesLBNF
    input <- readFile $ dirLBNF <> sample
    case lbnf input of
      Right tree -> do
        putStrLn "Parsed successfully!"
        putStrLn (printTree tree)
        let t = translate tree
        let str = printTree t
        putStrLn str
        writeFile (dirTreeSitter <> sampleId <> ".js") str
      Left err -> putStrLn $ "Parse error:\n" ++ err
  TreeSitter -> do
    let sample = fromMaybe "" $ lookup sampleId samplesTreeSitter
    input <- readFile $ dirTreeSitter <> sample
    case treeSitter input of
      Right tree -> do
        putStrLn "Parsed successfully!"
        putStrLn (printTree tree)
      Left err -> putStrLn $ "Parse error:\n" ++ err
