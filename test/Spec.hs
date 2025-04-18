import Test.Hspec

import System.Process (callProcess, readProcess)
import System.Directory

runGenerate = callProcess "tree-sitter" ["generate"]

runParse path = readProcess "tree-sitter" ["parse", path] ""

tmp = "tmp"

gen path = do
  createDirectory tmp
  copyFile path $ tmp <> "/grammar.js"
  setCurrentDirectory tmp
  runGenerate

rm = do
  setCurrentDirectory "../"
  removePathForcibly tmp

main = hspec $ do
  before (gen "samples/TreeSitter/predefined.js")
    $ after_ rm
    $ describe "TreeSitter parse" $ do
      it "predefined/char_a" $ do
        output <- runParse "../samples/predefined/char_a"
        let expect = "(grammar [0, 0] - [1, 0]\n  (char [0, 0] - [0, 8]))\n"
        output `shouldBe` expect
