module GenLBNF where

import Control.Monad (when)
import Test.QuickCheck (Arbitrary(..), Gen)
import Data.Char (isPrint)

data PredefinedChar = PredefinedChar String

instance Show PredefinedChar where
  show (PredefinedChar x) = x

special = ['\'', '\\', '\t', '\n', '\r', '\f']

toRegular = \case
  '\t' -> 't'
  '\n' -> 'n'
  '\r' -> 'r'
  '\f' -> 'f'
  c -> c

instance Arbitrary PredefinedChar where
  arbitrary = do
    c <- arbitrary
    let
      x = if c `elem` special
        then ['\\', toRegular c]
        else [c]
    if isPrint c
      then pure (PredefinedChar x)
      else arbitrary

wrapChar x = "'" <> x <> "'"

check x = case x of
  [c] -> c `notElem` ['\'', '\\']
  ['\\', c] -> c `elem` map toRegular special
  _ -> False
