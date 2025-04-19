import Test.Hspec

import System.Process (callProcess, readProcess)
import System.Directory

runGenerateTreeSitter = callProcess "tree-sitter" ["generate"]

runParseTreeSitter path = readProcess "tree-sitter" ["parse", path] ""

runGenerateLBNF path = callProcess "bnfc" ["-haskell", "--makefile", path]

runParseLBNF name path = readProcess ("./" <> name) [path] ""

tmp = "tmp"

genTreeSitter path = do
  createDirectory tmp
  copyFile path $ tmp <> "/grammar.js"
  setCurrentDirectory tmp
  runGenerateTreeSitter
  
genLBNF path = do
  createDirectory tmp
  setCurrentDirectory tmp
  runGenerateLBNF $ "../" <> path
  callProcess "make" []

rm = do
  setCurrentDirectory "../"
  removePathForcibly tmp

expectLBNF = unlines
  [ "../samples/predefined/char_a"
  , ""
  , "Parse Successful!"
  , ""
  , "[Abstract Syntax]"
  , ""
  , "Char 'a'"
  , ""
  , "[Linearized tree]"
  , ""
  , "Char 'a'"  
  ]

main = hspec $ do
  before (genLBNF "samples/LBNF/predefined.cf")
    $ after_ rm
    $ describe "LBNF parse" $ do
      it "predefined/char_a" $ do
        output <- runParseLBNF "TestPredefined" "../samples/predefined/char_a"
        output `shouldBe` expectLBNF
  before (genTreeSitter "samples/TreeSitter/predefined.js")
    $ after_ rm
    $ describe "TreeSitter parse" $ do
      it "predefined/char_a" $ do
        output <- runParseTreeSitter "../samples/predefined/char_a"
        let expect = "(top [0, 0] - [1, 0]\n  (char [0, 0] - [0, 8]))\n"
        output `shouldBe` expect
