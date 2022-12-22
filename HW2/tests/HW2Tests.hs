{-Haskell HW2 HUnit test cases
 Please add at least 2 additional tests for problems 3(b,c) and 4(a,b,c)-}

module HW2SampleTests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2


-- Sample Tree Integer examples given in the assignment prompt; make sure to provide your own tree examples
-- Your trees should have minimum 4 levels. 
tree1 =  NODE "grand-grand-mom" 
         (NODE "grand-mom" 
               (LEAF "aunt") 
               (NODE "mom" 
                      (LEAF "me") 
                      (LEAF "brother"))) 
          (LEAF "grand-uncle")

tree2 = NODE 1 (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) 
               (NODE 7 (LEAF 8) (LEAF 9))

leveltree1 = LNODE ("grand-grand-mom",3,1) (LNODE ("grand-mom",1,2) (LLEAF "aunt") (LNODE ("mom",1,1) (LLEAF "me") (LLEAF "brother"))) (LLEAF "grand-uncle")
leveltree2 = LNODE (1,3,2) (LNODE (2,2,1) (LNODE (3,1,1) (LLEAF 4) (LLEAF 5)) (LLEAF 6)) (LNODE (7,1,1) (LLEAF 8) (LLEAF 9))

p1a_test1 = TestCase (assertEqual "longest-test1" [7,8,9,10]  (longest [[1],[5,7],[1,2,3],[7,8,9,10]]) )  
p1a_test2 = TestCase (assertEqual "longest-test2" "longest"  (longest ["which","of","these","words","is","the","longest","?"]) ) 
p1a_test3 = TestCase (assertEqual "longest-test3" [] ((longest [])::[Int]) ) 

p1b_test1 = TestCase (assertEqual "longestTail-test1" [7,8,9,10]  (longestTail [[1],[5,7],[1,2,3],[7,8,9,10]]) )  
p1b_test2 = TestCase (assertEqual "longestTail-test2" "longest"  (longestTail ["which","of","these","words","is","the","longest","?"]) ) 
p1b_test3 = TestCase (assertEqual "longestTail-test3" []  ((longestTail [])::[Int]) ) 

p1c_test1 = TestCase (assertEqual "mylongest-test1" [7,8,9,10]  (mylongest [[1],[5,7],[1,2,3],[7,8,9,10]]) )  
p1c_test2 = TestCase (assertEqual "mylongest-test2" "longest"  (mylongest ["which","of","these","words","is","the","longest","?"]) ) 
p1c_test3 = TestCase (assertEqual "mylongest-test3" []  ((mylongest [])::[Int]) ) 

myCatsLog = [((7,2020),[("Oceanfish",7),("Tuna",1),("Whitefish",3),("Chicken",4),("Beef",2)]),
             ((8,2020),[("Oceanfish",6),("Tuna",2),("Whitefish",1),("Salmon",3),("Chicken",6)]),
             ((9,2020),[("Tuna",3),("Whitefish",3),("Salmon",2),("Chicken",5),("Beef",2),("Turkey",1),("Sardines",1)]),
             ((10,2020),[("Whitefish",5),("Sardines",3),("Chicken",7),("Beef",3)]),
             ((11,2020),[("Oceanfish",3),("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",1)]),
             ((12,2020),[("Tuna",2),("Whitefish",2),("Salmon",2),("Chicken",4),("Beef",2),("Turkey",4),("Sardines",1)]),              
             ((1,2021),[("Chicken",7),("Beef",3),("Turkey",4),("Whitefish",1),("Sardines",2)])
 ]

p2a_test1 = TestCase (assertEqual "getMonths-test1" [(7,2020),(8,2020)] (getMonths myCatsLog 4 "Oceanfish") ) 
p2a_test2 = TestCase (assertEqual "getMonths-test2" [(8,2020),(10,2020),(1,2021)] (getMonths myCatsLog 5 "Chicken" )) 

p2b_test1 = TestCase (assertEqual "monthlyCans-test1" [((7,2020),17),((8,2020),18),((9,2020),17),((10,2020),18),((11,2020),16),((12,2020),17),((1,2021),17)] (monthlyCans myCatsLog)) 

p3a_test1 = TestCase (assertEqual "convert-test1" (SCORE 4) (convert (LETTER 'A')) ) 
p3a_test2 = TestCase (assertEqual "convert-test2" (SCORE 2) (convert (LETTER 'C')) ) 
p3a_test3 = TestCase (assertEqual "convert-test3" (SCORE 0) (convert (LETTER 'F')) ) 
p3a_test4 = TestCase (assertEqual "convert-test4" (PASS) (convert PASS) ) 
p3a_test5 = TestCase (assertEqual "convert-test5" (SCORE 10) (convert (SCORE 10) ) ) 

p3b_test1 = TestCase (assertEqual "sumGrades-test1" (SCORE 14) (sumGrades [LETTER 'A', PASS , FAIL, SCORE 1, SCORE 2, SCORE 4, FAIL, PASS, LETTER 'D',LETTER 'C'] ) ) 
p3b_test2 = TestCase (assertEqual "sumGrades-test2" (SCORE 0) (sumGrades [PASS, FAIL]) ) 
p3b_test3 = TestCase (assertEqual "sumGrades-test3" (SCORE 0) (sumGrades []) ) 
p3b_test4 = TestCase (assertEqual "sumGrades-test4" (SCORE 4) (sumGrades [LETTER 'A', PASS , FAIL] ) ) 
p3b_test5 = TestCase (assertEqual "sumGrades-test5" (SCORE 7) (sumGrades [SCORE 1, SCORE 2, SCORE 4, PASS] ) ) 


organize_output1 = [[("MATH216",SCORE 1),("CptS322",SCORE 4)], [("CptS355",LETTER 'A'),("CptS321",LETTER 'D'),("Math171",LETTER 'C')],[("CptS499",PASS),("EE499",FAIL)]]
organize_output2 = [[("MATH216",SCORE 1),("CptS322",SCORE 4)],[],[("CptS499",PASS),("EE499",FAIL)]]
organize_output3 = [[],[("CptS355",LETTER 'A'),("CptS321",LETTER 'D'),("Math171",LETTER 'C')],[]]
organize_output4 = [[],[("CptS355",LETTER 'A'),("CptS321",LETTER 'D'),("Math171",LETTER 'C')],[]]
organize_output5 = [[("MATH216",SCORE 1), ("CptS322",SCORE 4)],[],[]]

p3c_test1 = TestCase (assertEqual "organize-test1" organize_output1 (organize [("CptS355", LETTER 'A'), ("CptS499",PASS),("EE499",FAIL),("MATH216",SCORE 1), ("CptS322",SCORE 4), ("CptS321",LETTER 'D'),("Math171",LETTER 'C')] )) 
p3c_test2 = TestCase (assertEqual "organize-test2" organize_output2 (organize [("CptS499",PASS) , ("EE499",FAIL), ("MATH216",SCORE 1), ("CptS322",SCORE 4)]) ) 
p3c_test3 = TestCase (assertEqual "organize-test3" organize_output3 (organize [("CptS355", LETTER 'A'), ("CptS321",LETTER 'D'),("Math171",LETTER 'C')]) ) 
p3c_test4 = TestCase (assertEqual "organize-test4" organize_output4 (organize [("CptS355", LETTER 'A'), ("CptS321",LETTER 'D'),("Math171",LETTER 'C')] )) 
p3c_test5 = TestCase (assertEqual "organize-test5" organize_output5 (organize [("MATH216",SCORE 1), ("CptS322",SCORE 4)] )) 

p4a_test1 = TestCase (assertEqual "createLevelTree-test1" leveltree1  (createLevelTree tree1) ) 
p4a_test2 = TestCase (assertEqual "createLevelTree-test2" leveltree2  (createLevelTree tree2) ) 

listify_output1 =  [("grand-grand-mom",3,1),("grand-mom",1,2), ("aunt",0,0),("mom",1,1),("me",0,0),("brother",0,0),("grand-uncle",0,0)]
listify_output2 =  [(1,3,2),(2,2,1),(3,1,1),(4,0,0),(5,0,0),(6,0,0),(7,1,1),(8,0,0),(9,0,0)]
p4b_test1 = TestCase (assertEqual "listify-test1" listify_output1 (listify leveltree1) ) 
p4b_test2 = TestCase (assertEqual "listify-test2" listify_output2  (listify leveltree2) ) 

p4c_test1 = TestCase (assertEqual "isBalanced-test1" False (isBalanced leveltree1) ) 
p4c_test2 = TestCase (assertEqual "isBalanced-test2" True  (isBalanced leveltree2) ) 


tests = TestList [ TestLabel "Problem 1a - test1 " p1a_test1,
                   TestLabel "Problem 1a - test2 " p1a_test2,
                   TestLabel "Problem 1a - test3 " p1a_test3,                   
                   TestLabel "Problem 1b - test1 " p1b_test1,
                   TestLabel "Problem 1b - test2 " p1b_test2,                   
                   TestLabel "Problem 1b - test3 " p1b_test3,                                      
                   TestLabel "Problem 1c - test1 " p1c_test1,
                   TestLabel "Problem 1c - test2 " p1c_test2,
                   TestLabel "Problem 1c - test3 " p1c_test3,                                      
                   TestLabel "Problem 2a - test1 " p2a_test1,
                   TestLabel "Problem 2a - test2 " p2a_test2,  
                   TestLabel "Problem 2b - test1 " p2b_test1,  
                   TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,  
                   TestLabel "Problem 3a - test3 " p3a_test3, 
                   TestLabel "Problem 3a - test4 " p3a_test4, 
                   TestLabel "Problem 3a - test5 " p3a_test5, 
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 3b - test3 " p3b_test3,
                   TestLabel "Problem 3b - test4 " p3b_test4,
                   TestLabel "Problem 3b - test5 " p3b_test5,
                   TestLabel "Problem 3c - test1 " p3c_test1,
                   TestLabel "Problem 3c - test2 " p3c_test2,
                   TestLabel "Problem 3c - test3 " p3c_test3,
                   TestLabel "Problem 3c - test4 " p3c_test4,
                   TestLabel "Problem 3c - test5 " p3c_test5,                   
                   TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4c - test1 " p4c_test1,
                   TestLabel "Problem 4c - test2 " p4c_test2
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests