-- | Various utility functions shared by all FizzBuzz implementations.

module Util (run, runList) where

run :: (Integer -> String) -> IO ()
run = runList . map

runList :: ([Integer] -> [String]) -> IO ()
runList f = mapM_ putStrLn $ f [1..100]
