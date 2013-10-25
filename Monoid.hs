-- | Epic concise version, using the Haskell type classes to full effect.

module Monoid where

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

import Util

main = run fizzify

fizzify = fromMaybe <$> show <*> mconcat
                                    [ check 3 "Fizz"
                                    , check 5 "Buzz" ]

check n s = (s <$) . guard . ((== 0) . (`mod` n))
