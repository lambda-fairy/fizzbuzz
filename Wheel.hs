-- | Roll a wheel along the list of numbers. Different functions on the
-- edge of the wheel give different answers as the wheel turns around.
--
-- Taken from <http://www.reddit.com/r/programming/comments/10d7w/fizzbuzz_spoilers/c10g19>

module Wheel where

main = mapM_ putStrLn $ zipWith ($) wheel [1..100]

wheel = cycle [i, i, f, i, b, f, i, i, f, b, i, f, i, i, (\x -> f x ++ b x)]
  where
    i = show
    f = const "Fizz"
    b = const "Buzz"
