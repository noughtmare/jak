module Data.Monoid.Nth (Nth, getNth, nth) where

newtype Nth a = Nth { getNth :: Int -> Maybe a }

nth :: a -> Nth a
nth = Nth . const . Just

instance Monoid (Nth a) where
  mempty = Nth (\_ -> Nothing)
  mappend (Nth f) (Nth g) = Nth (\i -> case compare i 0 of
    LT -> Nothing
    EQ -> f undefined
    GT -> g (i-1))