module Minimalisp.Control.Parser.Text.CharEq where

import Data.Maybe
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.Char

charEq :: Char -> Parser Char
charEq c =
  let mapErr ci =
        let actual = show . maybe "EOF" show $ listToMaybe ci
         in "Expected " <> show c <> ", but got " <> actual <> "."
   in conditional (== c) char \@/$ mapErr
