module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import System.Random
import Prelude hiding (lookup)

-- <<types
data Customer = Customer {
  name :: Name,
  account :: Account,
  balance :: Balance
} deriving (Show, Eq)

type Name    = String
data Account = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Eq)
type Balance = Int

-- 11.01.21 Monday's coinflip with:
-- Random thread delays + getline to process results
main :: IO ()
main = do
    coin <- coinFlip
    putStrLn $ "Random coin is: " ++ (show coin)
    box <- newMVar coin
    
    let c1 = Customer {name = "C1", balance = 100, account = One}
    let c2 = Customer {name = "C2", balance = 100, account = Two} 
    let c3 = Customer {name = "C3", balance = 100, account = Three}

    b <- newEmptyMVar
    var <- newMVar []
    mapM_  forkIO [customerthreads c1 b box, customerthreads c2 b box, customerthreads c3 b box]
    putStrLn "Press Return to show the results."
    _ <- getLine
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    c <- takeMVar b 
    putStrLn $ "The winner is: " ++ (show c)
    

data Coin = Head | Tail deriving (Show, Eq)
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

customerthreads :: Customer -> MVar (String, Customer) -> MVar Coin -> IO ()    
customerthreads cust b box = do 
    c1 <- coinFlip
    c2 <- takeMVar box
    putStrLn $ (show cust) ++ " -- got " ++ (show c1)
    if c1 == c2 then
        putMVar b ((" We've got a winner: "), cust)
    else do
        putStrLn $ " -- putting coin back in the box "
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        customerthreads cust b box


        