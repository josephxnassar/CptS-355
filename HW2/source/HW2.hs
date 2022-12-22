     module HW2
     where

{- 1. longest & longestTail &  mylongest - 20%-}

{- (a) longest - 5% -}

longest [] = []
longest (x:xs) | length x > length (longest xs) = x
               | otherwise = longest xs

{- (b) longestTail - 10% -}

longestTail xs = tailhelper xs []
                    where 
                         tailhelper [] buf = buf
                         tailhelper (x:xs) buf | length x > length buf = tailhelper xs x
                                               | otherwise = tailhelper xs buf

{- (c) mylongest - 5% -}

longerlist xs ys | length xs > length ys = xs
                 | otherwise = ys

mylongest xs = foldl longerlist [] xs

--mylongest xs = foldl (\x y -> if length x > length y then x else y) [] xs
-----------------------------------------------------------

{- 2. getMonths & monthlyCans 20% -}

{- (a) getMonths - 10%-}
-- please don't include the myCatsLog list in your solution file. 

-- myCatsLog = [((7,2020),[("Oceanfish",7),("Tuna",1),("Whitefish",3),("Chicken",4),("Beef",2)]),
--              ((8,2020),[("Oceanfish",6),("Tuna",2),("Whitefish",1),("Salmon",3),("Chicken",6)]),
--              ((9,2020),[("Tuna",3),("Whitefish",3),("Salmon",2),("Chicken",5),("Beef",2),("Turkey",1),("Sardines",1)]),
--              ((10,2020),[("Whitefish",5),("Sardines",3),("Chicken",7),("Beef",3)]),
--              ((11,2020),[("Oceanfish",3),("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",1)]),
--              ((12,2020),[("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",4),("Sardines",1)]),              
--              ((1,2021),[("Chicken",7),("Beef",3),("Turkey",4),("Whitefish",1),("Sardines",2)])
--  ]

-- numCansFlavor xs f = addL (map (\(x,y) -> if x==f then y else 0) xs)
--                where addL xs = foldl (+) 0 xs
          
-- getMonths xs n f = appendL (map (\(date,log) -> if n < (numCansFlavor log f) then [date] else []) xs)
--                     where appendL xs = foldl (++) [] xs

-- NOTE The above solution is something I had tried to come up with, after talking to a TA they helped me with the one below.

getMonths xs n f = map fst (filter (\(_,log) -> any (\(name,count) -> name == f && count > n ) log) xs)
          
-- getMonths xs n f = 

{- (b) monthlyCans - 20%-}

numCans xs = addL (map (\(x,y) -> y) xs)
          where addL xs = foldl (+) 0 xs

monthlyCans xs = map (\(date,log) -> (date,numCans log)) xs

-----------------------------------------------------------

{- 3. convert, sumGrades, and organize  - 23% -}

--define the Grade datatype
data Grade = LETTER Char |  SCORE Int | PASS | FAIL 
              deriving (Show, Eq, Ord)

{- (a) convert - 3% -}

convert (LETTER l) | l == 'A' = SCORE 4
                   | l == 'B' = SCORE 3
                   | l == 'C' = SCORE 2
                   | l == 'D' = SCORE 1
                   | l == 'F' = SCORE 0

convert (SCORE s) = (SCORE s)
convert PASS = PASS
convert FAIL = FAIL

{- (b) sumGrades - 10% -}

converttoInt (SCORE s) = s
converttoInt PASS = 0
converttoInt FAIL = 0

sumGrades xs = SCORE (foldl (+) 0 (map converttoInt (map convert xs)))

{- (c) organize - 10% -}

isLetter (LETTER l) = True
isLetter (SCORE s) = False
isLetter PASS = False
isLetter FAIL = False

isScore (LETTER l) = False
isScore (SCORE s) = True
isScore PASS = False
isScore FAIL = False

isPassFail (LETTER l) = False
isPassFail (SCORE s) = False
isPassFail PASS = True
isPassFail FAIL = True

-- isScore s = 

organize xs = ([filter (\(name,grade) -> isScore(grade)) xs])++([filter (\(name,grade) -> isLetter(grade)) xs])++([filter (\(name,grade) -> isPassFail(grade)) xs])

-----------------------------------------------------------

{-4 - createLevelTree & listify & isBalanced  - 27%-}

data Tree a = LEAF a | NODE a (Tree a) (Tree a)
               deriving (Show, Read, Eq)

data LevelTree a = LLEAF a | LNODE (a,Int,Int) (LevelTree a) (LevelTree a)
                    deriving (Show, Read, Eq)

{- (a) createLevelTree - 12% -}

-- tree1 =  NODE "grand-grand-mom" 
--          (NODE "grand-mom" 
--                (LEAF "aunt") 
--                (NODE "mom" 
--                       (LEAF "me") 
--                       (LEAF "brother"))) 
--           (LEAF "grand-uncle")

createLevelTree (LEAF l) = (LLEAF l)
createLevelTree (NODE a t1 t2) = LNODE (a,height t1,height t2) (createLevelTree t1) (createLevelTree t2)

height (LEAF l) = 1
height (NODE a t1 t2) = max (height t1) (height t2) + 1

{- (b) listify - 6% -}

listify (LLEAF l) = [(l,0,0)]
listify (LNODE (a,height1,height2) t1 t2) = [(a,height1,height2)]

{- (c) isBalanced - 10% -}

isBalanced (LLEAF l) = True
isBalance (LNODE (a,height1,height2) t1 t2) = height1 == height2+1 || height2 == height1+1

{-Testing your tree functions - 4%-}
-- include your tree examples in the test file. 

