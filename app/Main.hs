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

main = go "predefined" LBNF

lbnf = LBNF.pGrammar . LBNF.myLexer
treeSitter = TreeSitter.pGrammar . TreeSitter.myLexer

samples = "samples/"
dirLBNF = samples <> show LBNF <> "/"
dirTreeSitter = samples <> show TreeSitter <> "/" 

samplesLBNF =
  [ ("digit", "digit.cf")
  , ("const", "const.cf")
  , ("predefined", "predefined.cf")
  , ("bar", "bar.cf")
  , ("sep", "sep.cf")
  ]

samplesTreeSitter =
  [ ("minimal", "minimal.js")
  , ("quote", "quote.js")
  , ("digit", "digit.js")
  , ("const", "const.js")
  , ("predefined", "predefined.js")
  , ("bar", "bar.js")
  , ("sep", "sep.js")
  ]

data Input = LBNF | TreeSitter
  deriving Show

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
        putStrLn "Parsed successfully!\n"
        putStrLn (printTree tree)
        putStrLn ""
        let r = translate tree
        case r of
          Right t -> do
            putStrLn "Translated successfully!\n"
            let str = printTree t
            putStrLn str
            writeFile (dirTreeSitter <> sampleId <> ".js") str
          Left err -> putStrLn $ "Translation error:\n" <> show err
      Left err -> putStrLn $ "Parse error:\n" <> err
  TreeSitter -> do
    let sample = fromMaybe "" $ lookup sampleId samplesTreeSitter
    input <- readFile $ dirTreeSitter <> sample
    case treeSitter input of
      Right tree -> do
        putStrLn "Parsed successfully!\n"
        putStrLn (printTree tree)
      Left err -> putStrLn $ "Parse error:\n" <> err
