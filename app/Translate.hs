module Translate where

import Control.Monad (join)
import Data.List (groupBy, group, partition)
import Data.Function (on)
import Data.Char (toLower)

import qualified AbsLBNF as LBNF
import qualified AbsTreeSitter as TreeSitter

mkGrammar name constDecls rules = TreeSitter.Grammar (TreeSitter.Preamble constDecls)
  $ TreeSitter.GrammarBody (TreeSitter.Name name) (TreeSitter.Rules rules)

translate grammar =
  let
    defs = definitions grammar
    (rules, _notRules) = partition isRule defs
  in
    uncurry (mkGrammar "grammar")
    . consts
    . map (mapExpression substPredefined)
    . join
    . map pushChoiceRule
    . groupBy ((==) `on` fst)
    . map translateRule
    $ rules

mapExpression f (TreeSitter.Rule id' expression) =
  TreeSitter.Rule id' (f expression)

isRule = \case LBNF.Rule _ _ _ -> True; _ -> False

substPredefined = substSymbol (TreeSitter.Id "_integer") (regex "[0-9]+")
  . substSymbol (TreeSitter.Id "_double") (regex "[0-9]+\\.[0-9]+(e-?[0-9]+)?")
  . substSymbol (TreeSitter.Id "_char") (regex "'([^'\\\\]|\\\\[tnrf])'")
  . substSymbol (TreeSitter.Id "_string") (regex "\"([^\"\\\\]|\\\\[tnrf])*\"")
  . substSymbol (TreeSitter.Id "_ident") (regex "[a-zA-z][a-zA-z0-9_']*")

regex = TreeSitter.Regex . TreeSitter.RegEx . \x -> "/" <> x <> "/"

constDecl (literal, id') = TreeSitter.ConstDecl id' literal 

consts rules =
  let
    shouldMkConst (_literal, count) = count >= 2
    literalGroups = map (\(literal, _count) -> (literal, constId literal)) 
      $ filter shouldMkConst
      $ map (\case xs@(x: _) -> (x, length xs); [] -> undefined)
      $ group
      $ concatMap (collectLiterals . ruleExpression)
      $ rules
    substLiteralId (literal, id') = substLiteral literal (TreeSitter.Const id')
    subst expression = foldr substLiteralId expression literalGroups
  in
    (map constDecl literalGroups, map (mapExpression subst) rules)

constId literal = TreeSitter.Id literal

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
      $ TreeSitter.Choice
      $ map TreeSitter.Symbol
      $ ids
  in
    rule : rules

catToText = \case
  LBNF.ListCat cat -> "List" <> catToText cat
  LBNF.IdCat (LBNF.Ident text) -> case text of
    "Grammar" -> "grammar"
    _ -> "_" <> lowerFirst text

labelToText = \case
  LBNF.LabNoP labelId -> lowerFirst $ labelIdToText labelId
  _ -> undefined

labelIdToText = \case
  LBNF.Id id' -> ident id'
  _ -> undefined

ident (LBNF.Ident text) = text

lowerFirst = \case (first : rest) -> toLower first : rest; text -> text
 
collectLiterals = \case
  TreeSitter.Choice rules -> concatMap collectLiterals rules
  TreeSitter.Seq rules -> concatMap collectLiterals rules
  TreeSitter.Repeat rule -> collectLiterals rule
  TreeSitter.Repeat1 rule -> collectLiterals rule
  TreeSitter.Optional rule -> collectLiterals rule
  TreeSitter.Symbol _id' -> []
  TreeSitter.Const _id' -> []
  TreeSitter.Literal text -> [text]
  TreeSitter.Regex _text -> []
  
substLiteral from to = go where
  go = \case
    TreeSitter.Choice rules -> TreeSitter.Choice (map go rules)
    TreeSitter.Seq rules -> TreeSitter.Seq (map go rules)
    TreeSitter.Repeat rule -> TreeSitter.Repeat (go rule)
    TreeSitter.Repeat1 rule -> TreeSitter.Repeat1 (go rule)
    TreeSitter.Optional rule -> TreeSitter.Optional (go rule)
    TreeSitter.Symbol id' -> TreeSitter.Symbol id'
    TreeSitter.Const id' -> TreeSitter.Const id'
    TreeSitter.Literal text -> if text == from
      then to
      else TreeSitter.Literal text
    TreeSitter.Regex text -> TreeSitter.Regex text

substSymbol from to = go where
  go = \case
    TreeSitter.Choice rules -> TreeSitter.Choice (map go rules)
    TreeSitter.Seq rules -> TreeSitter.Seq (map go rules)
    TreeSitter.Repeat rule -> TreeSitter.Repeat (go rule)
    TreeSitter.Repeat1 rule -> TreeSitter.Repeat1 (go rule)
    TreeSitter.Optional rule -> TreeSitter.Optional (go rule)
    TreeSitter.Symbol id' -> if id' == from
      then to
      else TreeSitter.Symbol id'
    TreeSitter.Const id' -> TreeSitter.Const id'
    TreeSitter.Literal text -> TreeSitter.Literal text
    TreeSitter.Regex text -> TreeSitter.Regex text
