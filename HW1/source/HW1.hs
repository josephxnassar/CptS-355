-- CptS 355 - Spring 2021 -- Homework1 - Haskell
-- Name:
-- Collaborators: 

module HW1
     where

import Data.Char
import Data.List (sort)

-- Q1(a) getUniqueRight

getUniqueRight [] = []
getUniqueRight (x:xs) | x `elem` xs = getUniqueRight xs -- If x in xs, recurse
                      | otherwise = x:(getUniqueRight xs) -- x is unique so cons it to list

-- Q1(b) getUniqueLeft

getUniqueLeft iL = lefthelper (reverse iL) [] 
                    where
                         lefthelper [] buf = buf -- buffer needed because result needs to be flipped
                         lefthelper (x:xs) buf | (x `elem` xs) = lefthelper xs buf -- If x in xs, recurse
                                               | otherwise = (lefthelper xs (x:buf)) -- x is unique so cons it to buffer

-- Q2(a) cansInLog

cansInLog [] = 0
cansInLog ((x,y):xs) = y+(cansInLog xs) -- Counts total cans in list

-- Q2(b) numCans

-- myCatsLog = [((7,2020),[("Oceanfish",7),("Tuna",1),("Whitefish",3),("Chicken",4),("Beef",2)]),
--              ((8,2020),[("Oceanfish",6),("Tuna",2),("Whitefish",1),("Salmon",3),("Chicken",6)]),
--              ((9,2020),[("Tuna",3),("Whitefish",3),("Salmon",2),("Chicken",5),("Beef",2),("Turkey",1),("Sardines",1)]),
--              ((10,2020),[("Whitefish",5),("Sardines",3),("Chicken",7),("Beef",3)]),
--              ((11,2020),[("Oceanfish",3),("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",1)]),
--              ((12,2020),[("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",4),("Sardines",1)]),              
--              ((1,2021),[("Chicken",7),("Beef",3),("Turkey",4),("Whitefish",1),("Sardines",2)])
--  ]

numCans [] year = 0
numCans (((month,year),log):xs) y | y==year = (cansInLog log)+(numCans xs y) -- If year satisfied, adds total cans to whatever comes next
                                  | otherwise = numCans xs y -- Otherwise checks whats next

-- Q2(c) getMonths

getMonths [] n f = []
getMonths ((date,log):xs) n f | n < numCansFlavor log f = date:(getMonths xs n f) -- If n is less than total num of cans for certain flavor, keep that date
                              | otherwise = getMonths xs n f -- Otherwise check whats next
                                             where 
                                                  numCansFlavor [] f = 0
                                                  numCansFlavor ((x,y):xs) f | (x==f) = y+(numCansFlavor xs f) -- Counts cans with flavor f
                                                                             | otherwise = numCansFlavor xs f -- Otherwise checks whats next

-- getMonths [] n f = []
-- getMonths ((date,log):xs) n f = if (n < getSeconds log f) then date:(getMonths xs n f) else getMonths xs n f
--                                    where 
--                                         getSeconds [] f = 0
--                                         getSeconds ((x,y):xs) f | (x==f) = y+(getSeconds xs f)
--                                                                 | otherwise = getSeconds xs f
-- Q3 deepCount
     
deepCount v [] = 0
deepCount v (x:xs) | v `elem` x = (timesinList v x)+(deepCount v xs) -- Checks if v exists in x, returns times in list + whatever comes next
                   | otherwise = deepCount v xs -- Otherwise check next
                                   where 
                                        timesinList v [] = 0
                                        timesinList v (x:xs) | (v==x) = 1+(timesinList v xs) -- If v==x, add 1 to return sum
                                                             | otherwise = timesinList v xs -- Continue until end of nested list
         
-- Q4 clusterConsecutive

clusterConsecutive iL = clusterhelper iL 0 []
                         where
                              clusterhelper [] p buf | buf /= [] = (reverse buf):[] -- Only list empty
                                                     | otherwise = [] -- List and buffer both empty
                              clusterhelper (x:xs) p buf | (p==0) || (x==p+1) = clusterhelper xs x (x:buf) -- If previous 0 or x 1 greater than p, cons x to buffer
                                                         | otherwise = (reverse buf):(clusterhelper (x:xs) 0 []) -- Otherwise cons reverse buf, pass entire list