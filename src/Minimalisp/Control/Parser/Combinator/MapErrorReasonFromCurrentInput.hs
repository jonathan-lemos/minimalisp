module Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput where

import Data.Bifunctor
import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

mapErrorReasonFromCurrentInput :: (String -> String) -> Parser a -> Parser a
mapErrorReasonFromCurrentInput mapper = mapParse $ fmap (first $ newReasonFromCurrentInput mapper)

(\@/$) :: Parser a -> (String -> String) -> Parser a
(\@/$) = flip mapErrorReasonFromCurrentInput

infixl 2 \@/$
