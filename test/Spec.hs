import Test.Hspec
import Test.QuickCheck

import System.Process (callProcess, readProcess)
import System.Directory
import Control.DeepSeq(deepseq)
import Data.List (isPrefixOf)

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

alex = "/nix/store/9mxr9863x278zpigz5x3y1kl542sibk7-alex-3.4.0.1/bin/alex"
happy = "/nix/store/ndprcmy790p3mq2z3zhzbj90c8pgl0x5-happy-1.20.1.1/bin/happy"

patchMakefile = do
  content <- readFile "Makefile"
  let patched = unlines $
        map (\line ->
              if "ALEX " `isPrefixOf` line then "ALEX=" <> alex
              else if "HAPPY " `isPrefixOf` line then "HAPPY=" <> happy
              else line
            ) (lines content)
  patched `deepseq` writeFile "Makefile" patched
  
genLBNF path = do
  createDirectory tmp
  setCurrentDirectory tmp
  runGenerateLBNF $ "../" <> path
  patchMakefile
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
  [ "(source_file [0, 0] - [" <> show top_a <> ", " <> show top_b <> "]"
  , "  (top [0, 0] - [0, " <> show n <> "]"
  , "    (" <> what <> " [0, 0] - [0, " <> show n <> "])))"
  ]

predefined_char_a = "../samples/predefined/char_a"
sep_abc = "../samples/sep/abc"

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
  describe "predefined" $ do
    beforeAll (genLBNF "samples/LBNF/predefined.cf")
      $ afterAll_ rm
      $ describe "LBNF" $ do
        it "char_a" $ do
          output <- runParseLBNF "TestPredefined" predefined_char_a
          output `shouldBe` outputLBNF predefined_char_a "Char 'a'" "Char 'a'"
        it "arbitrary char" $ property $ \(PredefinedChar x) -> do
          let input = "Char " <> wrap [apostrophe] x
          writeFile "input" input
          output <- runParseLBNF "TestPredefined" "input"
          let abstract = "Char " <> wrap [apostrophe] (fix x)
          let linear = "Char " <> wrap [apostrophe] x
          output `shouldBe` outputLBNF "input" abstract linear
    beforeAll (genTreeSitter "samples/TreeSitter/predefined.js")
      $ afterAll_ rm
      $ describe "TreeSitter" $ do
        it "char_a" $ do
          output <- runParseTreeSitter predefined_char_a
          output `shouldBe` outputTreeSitter 1 0 8 "char"
        it "arbitrary char" $ property $ \(PredefinedChar x) -> do
          let input = "Char " <> wrap [apostrophe] x
          let n = 7 + BS.length (TE.encodeUtf8 (T.pack x))
          writeFile "input" input
          output <- runParseTreeSitter "input"
          output `shouldBe` outputTreeSitter 0 n n "char"  
        it "arbitrary string" $ property $ \(PredefinedString x) -> do
          let input = "String " <> wrap [quote] x
          let n = 9 + BS.length (TE.encodeUtf8 (T.pack x))
          writeFile "input" input
          output <- runParseTreeSitter "input"
          output `shouldBe` outputTreeSitter 0 n n "string"
  describe "sep" $ do
    beforeAll (genLBNF "samples/LBNF/sep.cf")
      $ afterAll_ rm
      $ describe "LBNF" $ do
        it "sep_abc" $ do
          output <- runParseLBNF "TestSep" sep_abc
          putStr output
          let abstract = "A [MkA (Ident \"a\"),MkA (Ident \"b\"),MkA (Ident \"c\")]"
          let linear = "A a, b, c"
          output `shouldBe` outputLBNF sep_abc abstract linear
    beforeAll (genTreeSitter "samples/TreeSitter/sep.js")
      $ afterAll_ rm
      $ describe "TreeSitter" $ do
        it "sep_abc" $ do
          output <- runParseTreeSitter sep_abc
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 9]"
            , "    (a [0, 0] - [0, 9]"
            , "      (mkA [0, 2] - [0, 3])"
            , "      (mkA [0, 5] - [0, 6])"
            , "      (mkA [0, 8] - [0, 9]))))"
            ]
        
