import Test.Hspec
import Test.QuickCheck

import System.Process (callProcess, readProcess)
import System.Directory

import qualified GenLBNF

runGenerateTreeSitter = callProcess "tree-sitter" ["generate"]

runParseTreeSitter path = readProcess "tree-sitter" ["parse", path] ""

runGenerateLBNF path = callProcess "bnfc" ["--haskell", "--makefile", path]

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

main = hspec $ do
  before (genLBNF "samples/LBNF/predefined.cf")
    $ after_ rm
    $ describe "LBNF parse" $ do
      it "predefined/char_a" $ do
        output <- runParseLBNF "TestPredefined" "../samples/predefined/char_a"
        output `shouldBe` unlines
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
  before (genTreeSitter "samples/TreeSitter/predefined.js")
    $ after_ rm
    $ describe "TreeSitter parse" $ do
      it "predefined/char_a" $ do
        output <- runParseTreeSitter "../samples/predefined/char_a"
        output `shouldBe` unlines
          [ "(top [0, 0] - [1, 0]"
          , "  (char [0, 0] - [0, 8]))"
          ]
  describe "arbitrary" $ do
    it "generates Char" $ property $ \(GenLBNF.PredefinedChar x) ->
      GenLBNF.check (GenLBNF.PredefinedChar x)
