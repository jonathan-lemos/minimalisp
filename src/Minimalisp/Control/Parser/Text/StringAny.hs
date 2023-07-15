module Minimalisp.Control.Parser.Text.StringAny (stringAny) where

import Control.Applicative
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Text.Utils

errMsg :: [String] -> String -> String -> String
errMsg options input ci =
  let delta = length input - length ci
      prefixEq a b = take delta a == take delta b
      validOptions = map (drop delta) $ filter (prefixEq input) options
   in case validOptions of
        [] -> "Unexpected " <> diagnosticToken ci
        [x] -> "Expected " <> quote x <> ", but got " <> diagnosticToken ci
        x -> "Expected any of " <> show x <> ", but got " <> diagnosticToken ci

stringAny :: [String] -> Parser String
stringAny ss = Parser $ \s -> do
  parse (asum (stringEq <$> ss) \@/$ \ci -> errMsg ss s ci) s
