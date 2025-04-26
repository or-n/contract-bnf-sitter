module GenLBNF where

import Control.Monad (when)
import Test.QuickCheck (Arbitrary(..), Gen, vectorOf, chooseInt)
import Data.Char (isPrint, ord)

data PredefinedChar = PredefinedChar String
data PredefinedStringChar = PredefinedStringChar String

instance Show PredefinedChar where
  show (PredefinedChar x) = wrap [apostrophe] x
instance Show PredefinedStringChar where
  show (PredefinedStringChar x) = wrap [quote] x

apostrophe = '\''
quote = '\"'

special = ['\\', '\t', '\n', '\r', '\f']

toRegular = \case
  '\t' -> 't'
  '\n' -> 'n'
  '\r' -> 'r'
  '\f' -> 'f'
  c -> c

isBMPP c = ord c <= 0xFFFF

arbitraryChar wrapChar = do
    c <- arbitrary
    let
      x = if c `elem` (wrapChar : special)
        then ['\\', toRegular c]
        else [c]
    if isPrint c
      then pure x
      else arbitraryChar wrapChar

instance Arbitrary (PredefinedChar) where
  arbitrary = PredefinedChar <$> arbitraryChar apostrophe
instance Arbitrary (PredefinedStringChar) where
  arbitrary = PredefinedStringChar <$> arbitraryChar quote

wrap c x = c <> x <> c

check wrapChar x = case x of
  [c] -> isPrint c && c `notElem` [wrapChar, '\\']
  ['\\', c] -> c `elem` map toRegular (wrapChar : special)
  _ -> False

data PredefinedString = PredefinedString String

instance Show PredefinedString where
  show (PredefinedString x) = wrap [quote] x

instance Arbitrary PredefinedString where
  arbitrary = do
    n <- chooseInt (0, 10)
    chars <- vectorOf n arbitrary
    pure . PredefinedString $ concat [x | PredefinedStringChar x <- chars]
  
