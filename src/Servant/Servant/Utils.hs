module Servant.Servant.Utils where

infixl 4 <$$>
(<$$>) :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
(<$$>) = fmap . fmap
