import Test.Hspec
import Test.QuickCheck

import System.Process (callProcess, readProcess)
import System.Directory

import GenLBNF
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

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

outputLBNF path tree linear = unlines
  [ path
  , ""
  , "Parse Successful!"
  , ""
  , "[Abstract Syntax]"
  , ""
  , tree
  , ""
  , "[Linearized tree]"
  , ""
  , linear
  ]

unquote = tail . init

fix "\\\\" = "\\\\"
fix "\\\'" = "\\\'"
fix "\"" = "\""
fix x = unquote (show x)

outputTreeSitter top_a top_b n what = unlines
  [ "(top [0, 0] - [" <> show top_a <> ", " <> show top_b <> "]"
  , "  (" <> what <> " [0, 0] - [0, " <> show n <> "]))"
  ]

main = hspec $ do
  describe "arbitrary" $ do
    it "valid Char" $ property $ \(PredefinedChar x) ->
      check apostrophe x
    it "valid StringChar" $ property $ \(PredefinedStringChar x) ->
      check quote x
    it "valid Ident" $ property $ \(PredefinedIdent x) ->
      checkIdent x
    it "valid Integer" $ property $ \(PredefinedInteger x) ->
      checkInteger x
    it "valid Double" $ property $ \x ->
      checkDouble (show (x :: PredefinedDouble))
  beforeAll (genLBNF "samples/LBNF/predefined.cf")
    $ afterAll_ rm
    $ describe "LBNF parse" $ do
      it "predefined/char_a" $ do
        output <- runParseLBNF "TestPredefined" "../samples/predefined/char_a"
        output `shouldBe` outputLBNF "../samples/predefined/char_a" "Char 'a'" "Char 'a'"
      it "predefined: arbitrary char" $ property $ \(PredefinedChar x) -> do
        let input = "Char " <> wrap [apostrophe] x
        writeFile "input" input
        output <- runParseLBNF "TestPredefined" "input"
        let abstract = "Char " <> wrap [apostrophe] (fix x)
        let linear = "Char " <> wrap [apostrophe] x
        output `shouldBe` outputLBNF "input" abstract linear
  beforeAll (genTreeSitter "samples/TreeSitter/predefined.js")
    $ afterAll_ rm
    $ describe "TreeSitter parse" $ do
      it "predefined/char_a" $ do
        output <- runParseTreeSitter "../samples/predefined/char_a"
        output `shouldBe` outputTreeSitter 1 0 8 "char"
      it "predefined: arbitrary char" $ property $ \(PredefinedChar x) -> do
        let input = "Char " <> wrap [apostrophe] x
        let n = 7 + BS.length (TE.encodeUtf8 (T.pack x))
        writeFile "input" input
        output <- runParseTreeSitter "input"
        output `shouldBe` outputTreeSitter 0 n n "char"  
      it "predefined: arbitrary string" $ property $ \(PredefinedString x) -> do
        let input = "String " <> wrap [quote] x
        let n = 9 + BS.length (TE.encodeUtf8 (T.pack x))
        writeFile "input" input
        output <- runParseTreeSitter "input"
        output `shouldBe` outputTreeSitter 0 n n "string"
