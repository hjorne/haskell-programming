module Test where

import Control.Monad (join)

myReturn :: Monad m => a -> m a
myReturn = pure

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join . fmap f x