module Minimalisp.Control.Parser.Combinator.MaybeParse where

import Minimalisp.Control.Parser.Combinator.EitherParse
import Minimalisp.Control.Parser

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
  Left _ -> Nothing
  Right v -> Just v

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse = fmap rightToMaybe . eitherParse
