module Translate where

import Data.List (groupBy)
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

translate (LBNF.MkGrammar defs) = TreeSitter.Grammar (TreeSitter.Preamble [])
  $ TreeSitter.GrammarBody (TreeSitter.Name "grammar")
  $ TreeSitter.Rules
  $ map to_choice
  $ groupBy ((==) `on` ruleId)
  $ map translateDef
  defs

translateDef = \case
  LBNF.Rule _label cat items -> TreeSitter.Rule
    (TreeSitter.Id (to_text cat))
    (to_seq (map to_expression items))
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

to_seq = \case
  [x] -> x
  xs -> TreeSitter.Seq xs

to_expression = \case
  LBNF.Terminal text -> TreeSitter.Literal text
  LBNF.NTerminal cat -> TreeSitter.Symbol (TreeSitter.Id (to_text cat))

ruleId (TreeSitter.Rule id _) = id

ruleExpression (TreeSitter.Rule _ expression) = expression

to_choice = \case
  [x] -> x
  rules@(rule : _) ->
    TreeSitter.Rule (ruleId rule)
    $ TreeSitter.Choice
    $ map ruleExpression rules

to_text = \case
  LBNF.ListCat cat -> "List" <> to_text cat
  LBNF.IdCat (LBNF.Ident text) -> text
  
