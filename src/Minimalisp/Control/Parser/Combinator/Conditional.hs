module Minimalisp.Control.Parser.Combinator.Conditional where

import Minimalisp.Control.Parser

conditional :: (a -> Bool) -> Parser a -> Parser a
conditional predicate parser = do
  v <- parser
  if predicate v
    then pure v
    else fail "Condition not met. This string should not appear."
