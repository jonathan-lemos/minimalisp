module Minimalisp.Control.Parser.Combinator.Conditional where

import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

conditional :: (a -> Bool) -> Parser a -> Parser a
conditional predicate parser = Parser $ \s -> do
  (s2, v) <- parse parser s
  if predicate v
    then Right (s2, v)
    else Left $ ParseError {currentInput = s, reason = "Condition failed."}
