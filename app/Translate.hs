module Translate where

import Control.Monad (join, when)
import Data.List (groupBy, group, find)
import Data.Function (on)
import Data.Maybe (isJust)
import Util (lowerFirst)

import qualified AbsLBNF as LBNF
import qualified AbsTreeSitter as TreeSitter

data TranslationError
  = Keyword String
  deriving Show

mkGrammar name constDecls rules = TreeSitter.Grammar (TreeSitter.Preamble constDecls)
  $ TreeSitter.GrammarBody (TreeSitter.Name name) (TreeSitter.Rules rules)

translate grammar = do
  let defs = definitions grammar
      rules = filter isRule defs
  separators <- mapM translateSeparator $ filter isSeparator defs
  terminators <- mapM translateTerminator $ filter isTerminator defs
  catRules <- mapM translateRule rules
  let ruleGroups = groupBy ((==) `on` fst) catRules
  catRules' <- mapM pushChoiceRule ruleGroups 
  pure $ uncurry (mkGrammar "grammar")
    . consts
    . map (mapRuleExpression substPredefined)
    . join
    $ [separators, terminators] <> catRules'

translateSeparator = \case
  LBNF.Separator minSize cat text -> do
    catId <- catToId cat
    id' <- catToId $ LBNF.ListCat cat
    let
      basicExpr = TreeSitter.Seq
        [ TreeSitter.Symbol catId
        , TreeSitter.Repeat $ TreeSitter.Seq
            [ TreeSitter.Literal text
            , TreeSitter.Symbol catId
            ] 
        ]
      expr = case minSize of
        LBNF.MNonempty -> basicExpr
        LBNF.MEmpty -> TreeSitter.Optional basicExpr
    pure $ TreeSitter.Rule id' expr
  _ -> undefined

translateTerminator = \case
  LBNF.Terminator minSize cat text -> do
    catId <- catToId cat
    id' <- catToId $ LBNF.ListCat cat
    let
      basicExpr = TreeSitter.Seq
        [ TreeSitter.Symbol catId
        , TreeSitter.Literal text
        ]
      expr = case minSize of
        LBNF.MNonempty -> TreeSitter.Repeat basicExpr
        LBNF.MEmpty -> TreeSitter.Repeat1 basicExpr
    pure $ TreeSitter.Rule id' expr
  _ -> undefined

mapRuleExpression f (TreeSitter.Rule id' expr) = TreeSitter.Rule id' (f expr)

isRule = \case LBNF.Rule _ _ _ -> True; _ -> False
isSeparator = \case LBNF.Separator _ _ _ -> True; _ -> False
isTerminator = \case LBNF.Terminator _ _ _ -> True; _ -> False

substPredefined = substSymbol (TreeSitter.Id "_integer") (regex "[0-9]+")
  . substSymbol (TreeSitter.Id "_double") (regex "[0-9]+\\.[0-9]+(e-?[0-9]+)?")
  . substSymbol (TreeSitter.Id "_char") (regex "'([^'\\\\]|\\\\['\\\\tnrf])'")
  . substSymbol (TreeSitter.Id "_string") (regex "\"([^\"\\\\]|\\\\['\\\\tnrf])*\"")
  . substSymbol (TreeSitter.Id "_ident") (regex "[a-zA-z][a-zA-z0-9_']*")

regex x = TreeSitter.Regex . TreeSitter.RegEx $ "/" <> x <> "/"

consts rules =
  let
    constId i = TreeSitter.Id ("c" <> show i)
    shouldMkConst (literal, count) = length literal > 3 && count >= 2
    literalGroups = map (\(i, (literal, _count)) -> (constId i, literal)) 
      $ zip [(0 :: Int)..]
      $ filter shouldMkConst
      $ map (\xs -> (head xs, length xs))
      $ group
      $ concatMap (collectLiterals . ruleExpression)
      $ rules
    substLiteralId (id', literal) = substLiteral literal (TreeSitter.Const id')
    subst expression = foldr substLiteralId expression literalGroups
    constDecl = uncurry TreeSitter.ConstDecl
  in
    (map constDecl literalGroups, map (mapRuleExpression subst) rules)

translateRule = \case
  LBNF.Rule label cat items -> do
    items' <- mapM itemToExpression items
    id' <- labelToText label
    let rule = TreeSitter.Rule (TreeSitter.Id id') (toSeq items')
    pure (cat, rule)
  _ -> undefined

definitions (LBNF.MkGrammar defs) = defs

toSeq = \case
  [x] -> x
  xs -> TreeSitter.Seq xs

itemToExpression = \case
  LBNF.Terminal text -> pure $ TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol <$> catToId cat

ruleId (TreeSitter.Rule id' _) = id'

ruleExpression (TreeSitter.Rule _ expression) = expression

pushChoiceRule xs = do
  let cat = fst (head xs)
      rules = map snd xs
      ids = map (TreeSitter.Symbol . ruleId) rules
  id' <- catToId cat
  let rule = TreeSitter.Rule id' $ toChoice ids
  pure (rule : rules)

toChoice = \case
  [x] -> x
  xs -> TreeSitter.Choice xs

keywordsLBNF =
  [ "Ident"
  ]

keywordsTreeSitter =
  [ "module"
  , "exports"
  , "grammar"
  , "const"
  , "name"
  , "rules"
  , "choice"
  , "seq"
  , "repeat"
  , "repeat1"
  , "optional"
  ]

guardNotKeyword x = do
  when (isJust $ find (== x) keywordsTreeSitter) $ do
    Left $ Keyword x
  when (isJust $ find (== x) keywordsLBNF) $ do
    Left $ Keyword x
  pure x

catToId = fmap TreeSitter.Id . catToText

catToText = \case
  LBNF.ListCat cat -> ("_list" <>) <$> catToText cat
  LBNF.IdCat id' -> do
    text <- guardNotKeyword . lowerFirst $ ident id'
    if text == "top"
      then pure text
      else pure $ "_" <> text

labelToText = \case
  LBNF.LabNoP labelId -> fmap lowerFirst . guardNotKeyword $ labelIdToText labelId
  _ -> undefined

labelIdToText = \case
  LBNF.Id id' -> ident id'
  _ -> undefined

ident (LBNF.Ident text) = text

collectLiterals = \case
  TreeSitter.Literal text -> [text]
  x -> foldExpression (concatMap collectLiterals) collectLiterals [] x
  
substLiteral from to = \case
  TreeSitter.Literal text | text == from -> to
  x -> mapExpression (substLiteral from to) x

substSymbol from to = \case
  TreeSitter.Symbol id' | id' == from -> to
  x -> mapExpression (substSymbol from to) x

mapExpression f = \case
  TreeSitter.Choice xs -> TreeSitter.Choice (map f xs)
  TreeSitter.Seq xs -> TreeSitter.Seq (map f xs)
  TreeSitter.Repeat x -> TreeSitter.Repeat (f x)
  TreeSitter.Repeat1 x -> TreeSitter.Repeat1 (f x)
  TreeSitter.Optional x -> TreeSitter.Optional (f x)
  x -> x

foldExpression many one zero = \case
  TreeSitter.Choice xs -> many xs
  TreeSitter.Seq xs -> many xs
  TreeSitter.Repeat x -> one x
  TreeSitter.Repeat1 x -> one x
  TreeSitter.Optional x -> one x
  _ -> zero
