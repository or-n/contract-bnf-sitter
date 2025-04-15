module Translate where

import Data.List (groupBy, group)
import Data.Function (on)
import ErrM

import qualified AbsLBNF as LBNF
import qualified ParLBNF as LBNF
import qualified PrintLBNF as LBNF

import qualified AbsTreeSitter as TreeSitter
import qualified ParTreeSitter as TreeSitter
import qualified PrintTreeSitter as TreeSitter

import qualified AbsRustRegex as RustRegex
import qualified ParRustRegex as RustRegex
import qualified PrintRustRegex as RustRegex

mkGrammar name constDecls rules = TreeSitter.Grammar (TreeSitter.Preamble constDecls)
  $ TreeSitter.GrammarBody (TreeSitter.Name name) (TreeSitter.Rules rules)

translate = uncurry (mkGrammar "grammar")
  . consts
  . map substPredefined
  . map to_choice
  . groupBy ((==) `on` ruleId)
  . map translateDef
  . definitions

substPredefined =
  substSymbol (TreeSitter.Id "Integer") (regex "[0-9]+")

regex = TreeSitter.Regex . TreeSitter.RegEx . \x -> "/" <> x <> "/"

constDecl (literal, id) = TreeSitter.ConstDecl id literal 

consts rules =
  let
    shouldMkConst (literal, count) = count >= 2
    literalGroups = map (\(literal, count) -> (literal, constId literal)) 
      $ filter shouldMkConst
      $ map (\xs@(x: _) -> (x, length xs))
      $ group
      $ concatMap collectLiterals rules
    substLiteralId (literal, id) = substLiteral literal (TreeSitter.Const id)
    substInRule rule = foldr substLiteralId rule literalGroups
  in
    (map constDecl literalGroups, map substInRule rules)

constId literal = TreeSitter.Id literal

translateDef = \case
  LBNF.Rule _label cat items -> TreeSitter.Rule
    (TreeSitter.Id (to_text cat))
    (to_seq (map to_rule items))
  LBNF.Comment open -> undefined
  LBNF.Comments open close -> undefined
  LBNF.Internal label cat items -> undefined
  LBNF.Token ident reg -> undefined
  LBNF.PosToken ident reg -> undefined
  LBNF.Entryp idents -> undefined
  LBNF.Separator minSize cat text -> undefined
  LBNF.Terminator minSize cat text -> undefined
  LBNF.Coercions ident n -> undefined
  LBNF.Rules ident rhss -> undefined
  LBNF.Layout ids -> undefined
  LBNF.LayoutStop ids -> undefined
  LBNF.LayoutTop -> undefined

definitions (LBNF.MkGrammar definitions) = definitions

to_seq = \case
  [x] -> x
  xs -> TreeSitter.Seq xs

to_rule = \case
  LBNF.Terminal text -> TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol (TreeSitter.Id (to_text cat))

ruleId (TreeSitter.Rule id _) = id

ruleExpression (TreeSitter.Rule _ expression) = expression

to_choice = \case
  [x] -> x
  rules@(rule : _) -> TreeSitter.Rule (ruleId rule)
    $ TreeSitter.Choice
    $ map ruleExpression rules

to_text = \case
  LBNF.ListCat cat -> "List" <> to_text cat
  LBNF.IdCat (LBNF.Ident text) -> text
  
collectLiterals = \case
  TreeSitter.Rule id rule -> collectLiterals rule
  TreeSitter.Choice rules -> concatMap collectLiterals rules
  TreeSitter.Seq rules -> concatMap collectLiterals rules
  TreeSitter.Repeat rule -> collectLiterals rule
  TreeSitter.Repeat1 rule -> collectLiterals rule
  TreeSitter.Optional rule -> collectLiterals rule
  TreeSitter.Symbol id -> []
  TreeSitter.Const id -> []
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
