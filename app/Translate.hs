module Translate where

import Control.Monad (join)
import Data.List (groupBy, group)
import Data.Function (on)
import Util (lowerFirst)

import qualified AbsLBNF as LBNF
import qualified AbsTreeSitter as TreeSitter

mkGrammar name constDecls rules = TreeSitter.Grammar (TreeSitter.Preamble constDecls)
  $ TreeSitter.GrammarBody (TreeSitter.Name name) (TreeSitter.Rules rules)

translate grammar =
  let
    defs = definitions grammar
    rules = filter isRule defs
    separators = filter isSeparator defs
  in
    uncurry (mkGrammar "grammar")
    . consts
    . map (mapRuleExpression substPredefined)
    . join
    . (map translateSeparator separators :)
    . map pushChoiceRule
    . groupBy ((==) `on` fst)
    . map translateRule
    $ rules

translateSeparator = \case
  LBNF.Separator minSize cat text ->
    let
      catId = TreeSitter.Id $ catToText cat
      id' = TreeSitter.Id . catToText $ LBNF.ListCat cat
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
    in
      TreeSitter.Rule id' expr
  _ -> undefined

mapRuleExpression f (TreeSitter.Rule id' expression) =
  TreeSitter.Rule id' (f expression)

isRule = \case LBNF.Rule _ _ _ -> True; _ -> False

isSeparator = \case LBNF.Separator _ _ _ -> True; _ -> False

substPredefined = substSymbol (TreeSitter.Id "_integer") (regex "[0-9]+")
  . substSymbol (TreeSitter.Id "_double") (regex "[0-9]+\\.[0-9]+(e-?[0-9]+)?")
  . substSymbol (TreeSitter.Id "_char") (regex "'([^'\\\\]|\\\\[tnrf])'")
  . substSymbol (TreeSitter.Id "_string") (regex "\"([^\"\\\\]|\\\\[tnrf])*\"")
  . substSymbol (TreeSitter.Id "_ident") (regex "[a-zA-z][a-zA-z0-9_']*")

regex x = TreeSitter.Regex . TreeSitter.RegEx $ "/" <> x <> "/"

consts rules =
  let
    shouldMkConst (_literal, count) = count >= 2
    literalGroups = map (\(literal, _count) -> (TreeSitter.Id literal, literal)) 
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
  LBNF.Rule label cat items ->
    let
      rule = TreeSitter.Rule
        (TreeSitter.Id (labelToText label))
        (toSeq (map itemToExpression items))
    in
      (cat, rule)
  _ -> undefined

definitions (LBNF.MkGrammar defs) = defs

toSeq = \case
  [x] -> x
  xs -> TreeSitter.Seq xs

itemToExpression = \case
  LBNF.Terminal text -> TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol (TreeSitter.Id (catToText cat))

ruleId (TreeSitter.Rule id' _) = id'

ruleExpression (TreeSitter.Rule _ expression) = expression

pushChoiceRule xs =
  let
    cat = fst (head xs)
    rules = map snd xs
    ids = map ruleId rules
    id' = TreeSitter.Id (catToText cat)
    rule = TreeSitter.Rule id'
      $ toChoice
      $ map TreeSitter.Symbol
      $ ids
  in
    rule : rules

toChoice = \case
  [x] -> x
  xs -> TreeSitter.Choice xs

catToText = \case
  LBNF.ListCat cat -> "_list" <> catToText cat
  LBNF.IdCat id' -> case ident id' of
    text -> "_" <> lowerFirst text

labelToText = \case
  LBNF.LabNoP labelId -> lowerFirst $ labelIdToText labelId
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
