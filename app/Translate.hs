module Translate where

import Control.Monad (join, when)
import Data.List (find, partition, isPrefixOf)
import Data.Maybe (isJust, catMaybes)
import Data.Either (rights)
import qualified Data.Map.Strict as Map
import Util (lowerFirst)

import qualified AbsLBNF as LBNF
import qualified AbsTreeSitter as TreeSitter

data TranslationError
  = Keyword String
  | NoRules
  | EntrypointsDefinedMoreThanOnce
  deriving Show

mkGrammar constDecls = TreeSitter.Grammar (TreeSitter.Preamble constDecls)

mkBody name rules inlines = TreeSitter.GrammarBody
  (TreeSitter.Name name)
  (TreeSitter.Rules rules)
  (TreeSitter.Inlines inlines)

translate (LBNF.MkGrammar defs) = do
  (separators, substOptionalSeparators) <- separatorRules defs
  terminators <- mapM translateTerminator $ filter isTerminator defs
  catRules <- mapM translateRule $ filter isRule defs
  let catRules' = rights
        $ map (\(cat, rule) -> do x <- rule; Right (cat, x)) catRules
  ruleGroups <- mapM pushChoiceRule $ Map.toList $ Map.fromListWith (<>)
    $ map (\(a, b) -> (a, [b])) catRules'
  sourceFile <- entrypointRule defs catRules'
  let (constDecls, rules) = consts
        . map (mapRuleExpression (substPredefined . substOptionalSeparators))
        . filter (not . isIdSpecial . ruleId)
        . join
        $ [[sourceFile], separators, terminators] <> ruleGroups
  pure $ mkGrammar constDecls $ mkBody "grammar" rules []

separatorRules defs = do
  separators <- mapM translateSeparator $ filter isSeparator defs
  let substOptional =
        flip (foldr substOptionalSeparator) (map fst $ filter snd separators)
  pure (map fst separators, substOptional)

entrypointRule defs catRules = do
    entrypoints <- case filter isEntryp defs of
      [LBNF.Entryp idents] -> mapM (catToId . LBNF.IdCat) idents
      [] -> case catRules of
        (first, _) : _ -> mapM catToId [first]
        _ -> Left NoRules
      _ -> Left EntrypointsDefinedMoreThanOnce
    pure $ TreeSitter.Rule (TreeSitter.Id "source_file")
      $ toChoice $ map TreeSitter.Symbol entrypoints

substOptionalSeparator sepRule =
  let id' = ruleId sepRule
  in substSymbol id' (TreeSitter.Optional (TreeSitter.Symbol id'))

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
      mustBeOptional = minSize == LBNF.MEmpty
    pure (TreeSitter.Rule id' basicExpr, mustBeOptional)
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
isEntryp = \case LBNF.Entryp _ -> True; _ -> False

substPredefined = substSymbol (TreeSitter.Id "_integer") (regex "[0-9]+")
  . substSymbol (TreeSitter.Id "_double") (regex "[0-9]+\\.[0-9]+(e-?[0-9]+)?")
  . substSymbol (TreeSitter.Id "_char") (regex "'([^'\\\\]|\\\\['\\\\tnrf])'")
  . substSymbol (TreeSitter.Id "_string") (regex "\"([^\"\\\\]|\\\\[\"\\\\tnrf])*\"")
  . substSymbol (TreeSitter.Id "_ident") (regex "[a-zA-z][a-zA-z0-9_']*")

regex x = TreeSitter.Regex . TreeSitter.RegEx $ "/" <> x <> "/"

consts rules =
  let
    constId i = TreeSitter.Id ("c" <> show i)
    shouldMkConst :: (String, Int) -> Bool
    shouldMkConst (literal, count) = length literal > 3 && count >= 2
    literalGroups = map (\(i, (literal, _count)) -> (constId i, literal)) 
      $ zip [(0 :: Int)..]
      $ filter shouldMkConst
      $ Map.toList
      $ Map.fromListWith (+)
      $ map (\x -> (x, 1))
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
    id' <- TreeSitter.Id <$> case label of
      LBNF.LabNoP LBNF.Wild -> pure "_"
      LBNF.LabNoP LBNF.ListE -> do
        id' <- catToText cat
        pure $ "_ListE" <> id'
      LBNF.LabNoP LBNF.ListCons -> do
        id' <- catToText cat
        pure $ "_ListCons" <> id'
      LBNF.LabNoP LBNF.ListOne -> do
        id' <- catToText cat
        pure $ "_ListOne" <> id'
      _ -> labelToText label
    case toSeq items' of
      Just seq -> do 
        let rule = TreeSitter.Rule id' seq
        pure (cat, Right rule)
      _ ->
        pure (cat, Left (TreeSitter.Symbol id'))
  _ -> undefined

toSeq = \case
  [] -> Nothing
  [x] -> Just x
  xs -> Just $ TreeSitter.Seq xs

itemToExpression = \case
  LBNF.Terminal text -> pure $ TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol <$> catToId cat

ruleId (TreeSitter.Rule id' _) = id'

ruleExpression (TreeSitter.Rule _ expression) = expression

isIdSpecial (TreeSitter.Id id') =
  "_" == id' ||
  "_ListE" `isPrefixOf` id' ||
  "_ListCons" `isPrefixOf` id' ||
  "_ListOne" `isPrefixOf` id'

pushChoiceRule (cat, rules) = do
  let (specialRules, notSpecialRules) = partition (isIdSpecial . ruleId) rules
      symbols = map (TreeSitter.Symbol . ruleId) notSpecialRules
      special = map ruleExpression specialRules
  id' <- catToId cat
  let rule = TreeSitter.Rule id' $ toChoice $ symbols <> special
  pure (rule : rules)

toChoice = \case
  [x] -> x
  xs -> TreeSitter.Choice xs

isOptional = \case
  TreeSitter.Null -> True
  TreeSitter.Seq [] -> True
  TreeSitter.Optional _ -> True
  TreeSitter.Choice xs -> any isOptional xs

keywordsLBNF =
  [ "ident"
  , "char"
  , "coertions"
  , "comment"
  , "digit"
  , "entrypoints"
  , "eps"
  , "internal"
  , "layout"
  , "letter"
  , "lower"
  , "nonempty"
  , "position"
  , "rules"
  , "separator"
  , "stop"
  , "terminator"
  , "token"
  , "toplevel"
  , "upper"
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
    text <- fmap lowerFirst . guardNotKeyword $ ident id'
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
