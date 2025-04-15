module Translate where

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
    (rules, _not_rules) = partition isRule defs
  in
    uncurry (mkGrammar "grammar")
    . consts
    . map to_choice
    . groupBy ((==) `on` ruleId)
    . map substPredefined
    . map translateDef
    $ rules

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
      $ concatMap collectLiterals rules
    substLiteralId (literal, id') = substLiteral literal (TreeSitter.Const id')
    substInRule rule = foldr substLiteralId rule literalGroups
  in
    (map constDecl literalGroups, map substInRule rules)

constId literal = TreeSitter.Id literal

translateDef = \case
  LBNF.Rule _label cat items -> TreeSitter.Rule
    (TreeSitter.Id (to_text cat))
    (to_seq (map to_rule items))
  _ -> undefined

definitions (LBNF.MkGrammar defs) = defs

to_seq = \case
  [x] -> x
  xs -> TreeSitter.Seq xs

to_rule = \case
  LBNF.Terminal text -> TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol (TreeSitter.Id (to_text cat))

ruleId (TreeSitter.Rule id' _) = id'
ruleId _ = undefined

ruleExpression (TreeSitter.Rule _ expression) = expression
ruleExpression _ = undefined

to_choice = \case
  [] -> undefined
  [x] -> x
  rules@(rule : _) -> TreeSitter.Rule (ruleId rule)
    $ TreeSitter.Choice
    $ map ruleExpression rules

to_text = \case
  LBNF.ListCat cat -> "List" <> to_text cat
  LBNF.IdCat (LBNF.Ident text) -> case text of
    "Grammar" -> "grammar"
    _ -> "_" <> lowerFirst text

lowerFirst = \case (first : rest) -> toLower first : rest; text -> text
 
collectLiterals = \case
  TreeSitter.Rule _id' rule -> collectLiterals rule
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
    TreeSitter.Rule id' rule -> TreeSitter.Rule id' (go rule)
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
    TreeSitter.Rule id' rule -> TreeSitter.Rule id' (go rule)
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
