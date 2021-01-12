{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-} 

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import System.Random
import Control.Monad ( forever, replicateM )
import Prelude hiding (lookup)
-- **
import System.Exit
import Data.Char (toLower)
import System.IO

-- <<types
data Customer = Customer { name :: Name, account :: Account, balance :: Balance } deriving (Show, Eq)

type Name    = String
data Account = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Eq)
type Balance = Int


-- 11.01.21 Monday's coinflip with:
-- Random thread delays + getline to process results
main :: IO ()
main = {-forever $-} do 
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
       --_ <- getLine
       randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
       c <- takeMVar b 
       putStrLn $ "The payee is: " ++ (show c)
    -- run again to find recipient
       putStrLn $ " -- putting coin back in the box"
       putMVar b c
       putStrLn "Press Return to find the recipient."
       --_ <- getLine
       box2 <- newMVar coin
       b2 <- newEmptyMVar
       mapM_ forkIO [customerthreads c1 b2 box2, customerthreads c2 b2 box2, customerthreads c3 b2 box2, customerthreads c4 b2 box2, customerthreads c5 b2 box2, customerthreads c6 b2 box2, customerthreads c7 b2 box2, customerthreads c8 b2 box2, customerthreads c9 b2 box2, customerthreads c10 b2 box2]
       randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
       putStrLn "Press Return to show the results."
       --_ <- getLine
       d <- takeMVar b2
       amount <- randomN
       putStrLn $ "The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c) ++ " and the random amount is: " ++ (show amount)
       if d /= c then do
            let print_balance = print . balance 
            let pd = print_balance d
            let pc = print_balance c 
            pd -- recipient
            pc -- payee
            --if pc == pd then 
            (c, d) <- transfer c d amount
            putStrLn $ "****UPDATED*****The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c)
            else 
                putStrLn $ "no transfer this time as they are the same account" 
           
        {-if d (balance d) == c (balance c) then 
                putStrLn $ "same customer, same balance!"
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
-- access Customer data points using record syntax

--Still need to
-- ensure process fails if two of the same customers are chosen
    -- run 100 times (?)
        -- print out all the final accounts







{-
-- ****** TRY to add the second check to the customer thread
-- Each customer thread should behave as follows: at random time intervals, the thread should select one of the other customers at random, 
-- and transfer a random amount of money (between £10 and £50)  into their account. 
--The amount transferred should not be more than what that customer has available in their account balance (account balances should not be negative).
-- try to move both the checks to the one thread
main :: IO ()
main = {-forever $-} do
       coin <- coinFlip
       putStrLn $ "Random coin is: " ++ (show coin)
       box <- newMVar coin
    
       let c1 = Customer {name = "C1", balance = 100, account = One}
       let c2 = Customer {name = "C2", balance = 100, account = Two} 
       let c3 = Customer {name = "C3", balance = 100, account = Three}
       let c4 = Customer {name = "C4", balance = 20, account = Four}
       let c5 = Customer {name = "C5", balance = 20, account = Five}
       let c6 = Customer {name = "C6", balance = 20, account = Six}
       let c7 = Customer {name = "C7", balance = 20, account = Seven}
       let c8 = Customer {name = "C8", balance = 20, account= Eight}
       let c9 = Customer {name = "C9", balance = 20, account = Nine}
       let c10 = Customer {name = "C10", balance = 20, account = Ten}
       putStrLn $ "10 customers created."

       mcusta <- newEmptyMVar -- (for the payee)
       mcustb <- newEmptyMVar -- (for the recipient)
       xcoin <-  newEmptyMVar -- (for the coin)
       putStrLn $ "10 customer threads being created."
       randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
       mapM_ forkIO [customerthreads c1 mcusta xcoin box mcustb, customerthreads c2 mcusta xcoin box mcustb, customerthreads c3 mcusta xcoin box mcustb, customerthreads c4 mcusta xcoin box mcustb, customerthreads c5 mcusta xcoin box mcustb, customerthreads c6 mcusta xcoin box mcustb, customerthreads c7 mcusta xcoin box mcustb, customerthreads c8 mcusta xcoin box mcustb, customerthreads c9 mcusta xcoin box mcustb, customerthreads c10 mcusta xcoin box mcustb]
       --putStrLn "Press Return to show the results_b."
       --_ <- getLine
       --U randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
       --T c <- takeMVar b 
       --h <- newEmptyMVar
       --T putStrLn $ "The payee is: " ++ (show c)
       ---RU h <- newMVar coin
       --g <- takeMVar h
       -- putStrLn $ "The rec is: " ++ (show g)
    -- run again to find second winner
       --U putStrLn $ " -- putting coin back in the box_b"
       --T putMVar b c
       --putStrLn "Press Return to find the recipient."
       --U _ <- getLine
       --U box2 <- newMVar coin
       --U b2 <- newEmptyMVar
       --U mapM_  forkIO [customerthreads c1 b2 box2, customerthreads c2 b2 box2, customerthreads c3 b2 box2]
       --U randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
       --putStrLn "Press Return to show the results."
       --U _ <- getLine

       --U d <- takeMVar b2
       --U putStrLn $ "The recipient is: " ++ (show d)
       --  amount <- randomN
       --U putStrLn $ "The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c) ++ " and the random amount is: " ++ (show amount)
       --(c, d) <- transfer c d amount
       --putStrLn $ "****UPDATED*****The recipient is: " ++ (show d) ++ "The payee is: " ++ (show c)
       putStrLn $ "test"
    
    {-if (d balance) == (c balance) then
        putStrLn $ "wow"
        else 
            putStrLn "nope"-}

data Coin = Head | Tail deriving (Show, Eq)
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail


customerthreads :: Customer -> MVar Customer -> MVar Coin -> MVar Coin -> MVar Customer -> IO ()    
customerthreads cust mcusta xcoin box mcustb = do 
    c1 <- coinFlip -- each customer flips their own coin and saves it to c1
    c2 <- takeMVar box -- then we take an MVar from the main box c2
    putStrLn $ (show cust) ++ "is playing" -- then we show the customer that is playing
    putMVar xcoin c1
    k <- takeMVar xcoin
    putStrLn $ (show cust) ++ " -- got " ++ (show c1)
    if c1 == c2 then do
        putStrLn "Match! Press return"
        _ <- getLine
        putMVar mcusta (cust)
        putStrLn $ " -- test for next customer -- putting coin back in the box"
        putStrLn $ " -- checking for the next customer that matched random coin"
        
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        c1 <- coinFlip
        c2 <- takeMVar box
        putStrLn $ (show cust) ++ " -- got_b " ++ (show c1)
        
        
        --putMVar box c2
        --randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        --customerthreads cust b x box
        
        --if c1 == c2 
            --then putStrLn $ "we got two values" 
            --else putStrLn $ "nope"
    else do
        putStrLn $ " -- putting coin back in the box "
        putMVar box c2
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        customerthreads cust mcusta xcoin box mcustb

transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  -- | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to)
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))        

randomN :: IO Int 
randomN = do
    r <- randomRIO (10, 50)
    return r


-- tomorrow - try different ways to get the customer threads to select each other
-- first two threads that run?   

-}