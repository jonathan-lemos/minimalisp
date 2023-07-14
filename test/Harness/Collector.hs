module Harness.Collector where

data Collector a b = Collector [a] b

instance Functor (Collector a) where
  fmap f (Collector as b) = Collector as (f b)

instance Applicative (Collector a) where
  pure = Collector []

  (Collector a f) <*> (Collector b g) = Collector (a <> b) (f g)

instance Monad (Collector a) where
  (Collector as b) >>= f = do
    let Collector cs d = f b
     in Collector (as <> cs) d

instance Semigroup (Collector a b) where
  a <> b = id <$ a <*> b

instance Monoid b => Monoid (Collector a b) where
  mempty = pure mempty

liftCollector :: a -> Collector a ()
liftCollector a = Collector [a] ()

collectedValues :: Collector a b -> [a]
collectedValues (Collector xs _) = xs
