module Minimalisp.Control.Parser.Text.StringEq where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.CharEq
import Data.List (singleton)

stringEq :: String -> Parser String
stringEq = foldMap (fmap singleton . charEq)
