module GenLBNF where

import Control.Monad (when)
import Test.QuickCheck (Arbitrary(..), Gen, vectorOf, chooseInt, suchThat)
import Data.Char (isPrint, ord, isLetter, isDigit)

newtype PredefinedChar = PredefinedChar String
newtype PredefinedStringChar = PredefinedStringChar String
newtype PredefinedIdentChar = PredefinedIdentChar Char deriving Show

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
    c <- arbitrary `suchThat` (\c -> isPrint c && isBMPP c)
    pure $ if c `elem` (wrapChar : special)
        then ['\\', toRegular c]
        else [c]

instance Arbitrary PredefinedChar where
  arbitrary = PredefinedChar <$> arbitraryChar apostrophe
instance Arbitrary PredefinedStringChar where
  arbitrary = PredefinedStringChar <$> arbitraryChar quote
instance Arbitrary PredefinedIdentChar where
  arbitrary = PredefinedIdentChar <$> arbitrary `suchThat` checkIdentChar

wrap c x = c <> x <> c

check wrapChar x = case x of
  [c] -> isPrint c && isBMPP c && c `notElem` [wrapChar, '\\']
  ['\\', c] -> c `elem` map toRegular (wrapChar : special)
  _ -> False

checkIdentChar c = isLetter c || isDigit c || c `elem` ['_', apostrophe]

checkIdent = \case
  x : xs -> isLetter x && all checkIdentChar xs
  _ -> False 

newtype PredefinedString = PredefinedString String

instance Show PredefinedString where
  show (PredefinedString x) = wrap [quote] x

instance Arbitrary PredefinedString where
  arbitrary = do
    n <- chooseInt (0, 10)
    chars <- vectorOf n arbitrary
    pure . PredefinedString $ concat [x | PredefinedStringChar x <- chars]
  
newtype PredefinedIdent = PredefinedIdent String
  deriving Show

instance Arbitrary PredefinedIdent where
  arbitrary = do
    x <- arbitrary `suchThat` isLetter
    n <- chooseInt (0, 10)
    chars <- vectorOf n arbitrary
    pure $ PredefinedIdent $ x : [x | PredefinedIdentChar x <- chars]
