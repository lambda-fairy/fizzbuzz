-- | Race conditions? What race conditions?

module Sleep where

import Control.Concurrent
import Control.Monad

sleep :: Rational -> IO ()
sleep = threadDelay . round . (* 1000000)

nom :: Int -> String -> MVar String -> IO ()
nom n s ref = forever $ do
    sleep (realToFrac n)
    modifyMVar_ ref (return . (++ s))

output :: MVar String -> IO ()
output ref =
    forM_ [1..100] $ \x -> do
        sleep 1
        s <- modifyMVar ref (\s -> return ("", s))
        case s of
            "" -> print x
            _  -> putStrLn s

main :: IO ()
main = do
    ref <- newMVar ""
    forkIO $ fizz ref
    forkIO $ sleep 0.1 >> buzz ref
    sleep 0.2 >> output ref
  where
    fizz = nom 3 "Fizz"
    buzz = nom 5 "Buzz"
