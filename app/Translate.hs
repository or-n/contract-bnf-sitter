module Translate where

import Control.Monad (join)
import Data.List (groupBy, group, partition)
import Data.Function (on)
import Util (lowerFirst)

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
    . map (mapRuleExpression substPredefined)
    . join
    . map pushChoiceRule
    . groupBy ((==) `on` fst)
    . map translateRule
    $ rules

mapRuleExpression f (TreeSitter.Rule id' expression) =
  TreeSitter.Rule id' (f expression)

isRule = \case LBNF.Rule _ _ _ -> True; _ -> False

substPredefined = substSymbol (TreeSitter.Id "_integer") (regex "[0-9]+")
  . substSymbol (TreeSitter.Id "_double") (regex "[0-9]+\\.[0-9]+(e-?[0-9]+)?")
  . substSymbol (TreeSitter.Id "_char") (regex "'([^'\\\\]|\\\\[tnrf])'")
  . substSymbol (TreeSitter.Id "_string") (regex "\"([^\"\\\\]|\\\\[tnrf])*\"")
  . substSymbol (TreeSitter.Id "_ident") (regex "[a-zA-z][a-zA-z0-9_']*")

regex x = TreeSitter.Regex . TreeSitter.RegEx $ "/" <> x <> "/"

constDecl (literal, id') = TreeSitter.ConstDecl id' literal 

consts rules =
  let
    shouldMkConst (_literal, count) = count >= 2
    literalGroups = map (\(literal, _count) -> (literal, constId literal)) 
      $ filter shouldMkConst
      $ map (\xs -> (head xs, length xs))
      $ group
      $ concatMap (collectLiterals . ruleExpression)
      $ rules
    substLiteralId (literal, id') = substLiteral literal (TreeSitter.Const id')
    subst expression = foldr substLiteralId expression literalGroups
  in
    (map constDecl literalGroups, map (mapRuleExpression subst) rules)

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

collectLiterals = \case
  TreeSitter.Literal text -> [text]
  x -> foldExpression (concatMap collectLiterals) collectLiterals [] x
  
substLiteral from to = \case
  TreeSitter.Literal text -> if text == from
    then to
    else TreeSitter.Literal text
  x -> mapExpression (substLiteral from to) x

substSymbol from to = \case
  TreeSitter.Symbol id' -> if id' == from
    then to
    else TreeSitter.Symbol id'
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
