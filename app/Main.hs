module Main where

import Control.Concurrent
import System.Random


main :: IO ()
main = do
    coin <- coinFlip
    putStrLn $ "Random coin is: " ++ (show coin)
    box <- newMVar coin
    b <- newEmptyMVar
    var <- newMVar []
    mapM_  forkIO [process "C1" b box, process "C2" b box, process "C3" b box]
    putStrLn "Press Return to show the results."
    _ <- getLine
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    c <- takeMVar b 
    putStrLn $ "The winner is: " ++ c
    

   

data Coin = Head | Tail deriving (Show, Eq)
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

process :: String -> MVar String -> MVar Coin -> IO ()    
process a b box = do 
    c1 <- coinFlip
    c2 <- takeMVar box
    putStrLn $ a ++ " -- got " ++ (show c1)
    if c1 == c2 then
        putMVar b ("Process " ++ a ++ " wins!")
    else do
        putStrLn $ " -- putting coin back in the box "
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        process a b box

