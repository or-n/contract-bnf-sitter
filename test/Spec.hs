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

alex = "/nix/store/bbm48ma0dh22vbawwhj1g9nqkxbbrbc1-alex-3.4.0.1/bin/alex"
happy = "/nix/store/n4899xhj40gddjam6g5bfmy0dl4jbm9a-happy-1.20.1.1/bin/happy"

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

outputTreeSitter top_a top_b n what = unlines
  [ "(source_file [0, 0] - [" <> show top_a <> ", " <> show top_b <> "]"
  , "  (top [0, 0] - [0, " <> show n <> "]"
  , "    (" <> what <> " [0, 0] - [0, " <> show n <> "])))"
  ]

fileLBNF binary path name abstract linear = it name $ do
    output <- runParseLBNF binary (path name)
    output `shouldBe` outputLBNF (path name) abstract linear

fileTreeSitter path name end result = it name $ do
    output <- runParseTreeSitter (path name)
    output `shouldBe` outputTreeSitter 1 0 end result

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
    let
      path x = "../samples/predefined/" <> x
      char_a = "char_a"
      char_newline = "char_newline"
      double = "double"
      double_e = "double_e"
      double_trailing_0s = "double_trailing_0s"
      ident = "ident"
      ident_ = "ident_"
      ident_apo = "ident_apo"
      integer_0 = "integer_0"
      integer_leading_0 = "integer_leading_0"
      string = "string"
      string_empty = "string_empty"
    beforeAll (genLBNF "samples/LBNF/predefined.cf")
      $ afterAll_ rm
      $ describe "LBNF" $ do
        let file = fileLBNF "TestPredefined" path
        file char_a
          "Char 'a'"
          "Char 'a'"
        file char_newline
          "Char '\\n'"
          "Char '\\n'"
        file double
          "Double 9.99"
          "Double 9.99"
        file double_e
          "Double 3.14e-5"
          "Double 3.14e-5"
        file double_trailing_0s
          "Double 3.0"
          "Double 3.0"
        file ident
          "IdentT (Ident \"aB\")"
          "Ident aB"
        file ident_
          "IdentT (Ident \"x_\")"
          "Ident x_"
        file ident_apo
          "IdentT (Ident \"y'\")"
          "Ident y'"
        file integer_0
          "Integer 0"
          "Integer 0"
        file integer_leading_0
          "Integer 928735"
          "Integer 928735"
        file string
          "String \"a b\\nx\""
          "String \"a b\\nx\""
        file string_empty
          "String \"\""
          "String \"\""
        it "arbitrary char" $ property $ \(PredefinedChar x) -> do
          let input = "Char " <> wrap [apostrophe] x
          writeFile "input" input
          output <- runParseLBNF "TestPredefined" "input"
          let unquote = tail . init
          let fixChar = \case
                "\\\\" -> "\\\\"
                "\\\'" -> "\\\'"
                "\"" -> "\""
                x -> unquote (show x)
          let abstract = "Char " <> wrap [apostrophe] (fixChar x)
          let linear = "Char " <> wrap [apostrophe] x
          output `shouldBe` outputLBNF "input" abstract linear
    beforeAll (genTreeSitter "samples/TreeSitter/predefined.js")
      $ afterAll_ rm
      $ describe "TreeSitter" $ do
        let file = fileTreeSitter path
        file char_a 8 "char"
        file char_newline 9 "char"
        file double 11 "double"
        file double_e 14 "double"
        file double_trailing_0s 11 "double"
        file ident 8 "identT"
        file ident_ 8 "identT"
        file ident_apo 8 "identT"
        file integer_0 9 "integer"
        file integer_leading_0 15 "integer"
        file string 15 "string"
        file string_empty 9 "string"
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
    let
      path x = "../samples/sep/" <> x
      abc = "abc"
      empty = "empty"
      terminator_abc = "terminator_abc"
      c_zero = "c_zero"
      c_one = "c_one"
      c_two = "c_two"
    beforeAll (genLBNF "samples/LBNF/sep.cf")
      $ afterAll_ rm
      $ describe "LBNF" $ do
        let file = fileLBNF "TestSep" path
        file abc
          "A [MkA (Ident \"a\"),MkA (Ident \"b\"),MkA (Ident \"c\")]"
          "A a, b, c"
        file empty
          "A []"
          "A"
        file terminator_abc 
          "B [MkB (Ident \"a\"),MkB (Ident \"b\"),MkB (Ident \"c\")]"
          "B a, b, c,"
        -- file c_zero
        --   "C []"
        --   "C"
        file c_one
          "C [MkC (Ident \"a\")]"
          "C a;"
        file c_two
          "C [MkC (Ident \"a\"),MkC (Ident \"b\")]"
          "C a;\nb;"
    beforeAll (genTreeSitter "samples/TreeSitter/sep.js")
      $ afterAll_ rm
      $ describe "TreeSitter" $ do
        it abc $ do
          output <- runParseTreeSitter (path abc)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 9]"
            , "    (a [0, 0] - [0, 9]"
            , "      (mkA [0, 2] - [0, 3])"
            , "      (mkA [0, 5] - [0, 6])"
            , "      (mkA [0, 8] - [0, 9]))))"
            ]
        it empty $ do
          output <- runParseTreeSitter (path empty)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 1]"
            , "    (a [0, 0] - [0, 1])))"
            ]
        it terminator_abc $ do
          output <- runParseTreeSitter (path terminator_abc)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 10]"
            , "    (b [0, 0] - [0, 10]"
            , "      (mkB [0, 2] - [0, 3])"
            , "      (mkB [0, 5] - [0, 6])"
            , "      (mkB [0, 8] - [0, 9]))))"
            ]
        it c_one $ do
          output <- runParseTreeSitter (path c_one)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 4]"
            , "    (c [0, 0] - [0, 4]"
            , "      (mkC [0, 2] - [0, 3]))))"
            ]
        it c_two $ do
          output <- runParseTreeSitter (path c_two)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (top [0, 0] - [0, 7]"
            , "    (c [0, 0] - [0, 7]"
            , "      (mkC [0, 2] - [0, 3])"
            , "      (mkC [0, 5] - [0, 6]))))"
            ]
    describe "precedence" $ do
      let path x = "../samples/precedence/" <> x
      let a = "a"
      beforeAll (genLBNF "samples/LBNF/precedence.cf")
        $ afterAll_ rm
        $ describe "LBNF" $ do
          let file = fileLBNF "TestPrecedence" path
          file a
            "Exp (Shift (Number 1) (Scale (Number 2) (Number 3)))"
            "1 + 2 * 3"
      beforeAll (genTreeSitter "samples/TreeSitter/precedence.js")
        $ afterAll_ rm
        $ describe "TreeSitter" $ do
          it a $ do
            output <- runParseTreeSitter (path a)
            output `shouldBe` unlines
              [ "(source_file [0, 0] - [1, 0]"
              , "  (top [0, 0] - [0, 9]"
              , "    (exp [0, 0] - [0, 9]"
              , "      (shift [0, 0] - [0, 9]"
              , "        (number [0, 0] - [0, 1])"
              , "        (scale [0, 4] - [0, 9]"
              , "          (number [0, 4] - [0, 5])"
              , "          (number [0, 8] - [0, 9]))))))"
              ]
  describe "entrypoints" $ do
    let
      path x = "../samples/entrypoints/" <> x
      a = "a" -- pA is used by default in TestEntrypoints.hs
    beforeAll (genLBNF "samples/LBNF/entrypoints.cf")
      $ afterAll_ rm
      $ describe "LBNF" $ do
          let file = fileLBNF "TestEntrypoints" path
          file a "A" "A"
    beforeAll (genTreeSitter "samples/TreeSitter/entrypoints.js")
      $ afterAll_ rm
      $ describe "TreeSitter" $ do
        it a $ do
          output <- runParseTreeSitter (path a)
          output `shouldBe` unlines
            [ "(source_file [0, 0] - [1, 0]"
            , "  (a [0, 0] - [0, 1]))"
            ]
