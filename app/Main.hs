{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
    let c4 = Customer {name = "C4", balance = 20, account = Four}
    let c5 = Customer {name = "C5", balance = 20, account = Five}
    let c6 = Customer {name = "C6", balance = 20, account = Six}
    let c7 = Customer {name = "C3", balance = 20, account = Seven}
    let c8 = Customer {name = "C8", balance = 20, account= Eight}
    let c9 = Customer {name = "C9", balance = 20, account = Nine}
    let c10 = Customer {name = "C10", balance = 20, account = Ten}
    putStrLn $ "10 customers created."

    b <- newEmptyMVar
    putStrLn $ "10 customer threads being created."
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    mapM_ forkIO [customerthreads c1 b box, customerthreads c2 b box, customerthreads c3 b box, customerthreads c4 b box, customerthreads c5 b box, customerthreads c6 b box, customerthreads c7 b box, customerthreads c8 b box, customerthreads c9 b box, customerthreads c10 b box]
    putStrLn "Press Return to show the results."
    _ <- getLine
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    c <- takeMVar b 
    putStrLn $ "The payee is: " ++ (show c)
    -- run again to find second winner
    putStrLn $ " -- putting coin back in the box"
    putMVar b c
    putStrLn "Press Return to find the recipient."
    _ <- getLine
    box2 <- newMVar coin
    b2 <- newEmptyMVar
    mapM_  forkIO [customerthreads c1 b2 box2, customerthreads c2 b2 box2, customerthreads c3 b2 box2]
    randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
    putStrLn "Press Return to show the results."
    _ <- getLine
    d <- takeMVar b2
    amount <- randomN
    putStrLn $ "The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c) ++ " and the random amount is: " ++ (show amount)
    (c, d) <- transfer c d amount
    putStrLn $ "****UPDATED*****The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c)

    {-if (d balance) == (c balance) then
        putStrLn $ "wow"
        else 
            putStrLn "nope"-}

data Coin = Head | Tail deriving (Show, Eq)
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail


customerthreads :: Customer -> MVar Customer -> MVar Coin -> IO ()    
customerthreads cust b box = do 
    c1 <- coinFlip
    c2 <- takeMVar box
    putStrLn $ (show cust) ++ " -- got " ++ (show c1)
    if c1 == c2 then do
        putMVar b (cust)
    else do
        putStrLn $ " -- putting coin back in the box "
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        customerthreads cust b box

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  -- | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to)
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))        

randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r



-- This function creates threads with the MVar holding two things 
{-customerthreads :: Customer -> MVar (String, Customer) -> MVar Coin -> IO ()    
customerthreads cust b box = do 
    c1 <- coinFlip
    c2 <- takeMVar box
    putStrLn $ (show cust) ++ " -- got " ++ (show c1)
    if c1 == c2 then do
        putMVar b ((" Details: "), cust)
    else do
        putStrLn $ " -- putting coin back in the box "
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        customerthreads cust b box-}


-- Next steps:
-- now we have two customers - maybe put them together in an MVar in main?
-- or make the MVar with Customer + String just Customer?
-- then create a function to transfer money between them 
-- then test running the function        

--Still can't
-- access Customer data points