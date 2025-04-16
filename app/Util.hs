module Util where

import Data.Char (toLower)

lowerFirst = \case (first : rest) -> toLower first : rest; text -> text
