module GenLBNF where

import Control.Monad (when)
import Test.QuickCheck (Arbitrary(..), Gen, vectorOf, chooseInt, suchThat)
import Data.Char (isPrint, ord, isLetter, isDigit)

newtype PredefinedInteger = PredefinedInteger String deriving Show
data PredefinedDouble = PredefinedDouble String String (Maybe (Bool, String))
newtype PredefinedChar = PredefinedChar String
newtype PredefinedStringChar = PredefinedStringChar String
newtype PredefinedIdentChar = PredefinedIdentChar Char deriving Show

instance Show PredefinedDouble where
  show (PredefinedDouble n1 n2 m) = case m of
    Just (isNegative, n3) ->
      let sign = if isNegative then "-" else ""
      in n1 <> "." <> n2 <> "e" <> sign <> n3
    _ -> n1 <> "." <> n2

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

instance Arbitrary PredefinedInteger where
  arbitrary = do
    n <- chooseInt (1, 10)
    PredefinedInteger <$> vectorOf n (arbitrary `suchThat` isDigit)
instance Arbitrary PredefinedDouble where
  arbitrary = do
    PredefinedInteger n1 <- arbitrary
    PredefinedInteger n2 <- arbitrary
    isScientific <- arbitrary
    if isScientific
      then do
        isNegative <- arbitrary
        PredefinedInteger n3 <- arbitrary
        pure $ PredefinedDouble n1 n2 (Just (isNegative, n3))
      else pure $ PredefinedDouble n1 n2 Nothing
instance Arbitrary PredefinedChar where
  arbitrary = PredefinedChar <$> arbitraryChar apostrophe
instance Arbitrary PredefinedStringChar where
  arbitrary = PredefinedStringChar <$> arbitraryChar quote
instance Arbitrary PredefinedIdentChar where
  arbitrary = PredefinedIdentChar <$> arbitrary `suchThat` checkIdentChar

wrap c x = c <> x <> c

checkInteger xs = all isDigit xs

checkDouble xs0 =
  let (n1, xs1) = span isDigit xs0 in
  case xs1 of
    '.' : xs2 ->
      let (n2, xs3) = span isDigit xs2 in
      case xs3 of
        'e' : '-' : xs4 -> all isDigit xs4
        'e' : xs4 -> all isDigit xs4
        _ -> True
    _ -> False

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
