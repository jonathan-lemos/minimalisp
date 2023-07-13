module Minimalisp.Control.Parser where

import Control.Applicative
import Minimalisp.Data.ParseError

type ParseFunction a = String -> Either ParseError (String, a)

newtype Parser a = Parser
  { parse :: ParseFunction a
  }

mapParse :: (ParseFunction a -> ParseFunction b) -> Parser a -> Parser b
mapParse f (Parser a) = Parser (f a)

defaultErrMsg :: String
defaultErrMsg = "Syntax Error. This string should not appear."

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f = mapParse ((fmap . fmap) f .)

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ Right . (,v)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  af <*> bf =
    Parser $ \s -> do
      (s2, f) <- parse af s
      (s3, v) <- parse bf s2
      return (s3, f v)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  a >>= b =
    Parser $ \s -> do
      (s2, v) <- parse a s
      parse (b v) s2

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser $ Left . ParseError msg

instance (Semigroup t) => Semigroup (Parser t) where
  (<>) :: Parser t -> Parser t -> Parser t
  a <> b = liftA2 (<>) a b

instance (Monoid t) => Monoid (Parser t) where
  mempty :: Parser t
  mempty = pure mempty

instance Alternative Parser where
  empty = Parser $ Left . ParseError defaultErrMsg

  (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = Parser $ \s ->
    case (parse a s, parse b s) of
      (Right (s2, v), _) -> Right (s2, v)
      (_, Right (s2, v)) -> Right (s2, v)
      (Left pe1, Left pe2) ->
        case (length . currentInput) pe1 - (length . currentInput) pe2 of
          x | x < 0 -> Left pe1
          _ -> Left pe2
          
