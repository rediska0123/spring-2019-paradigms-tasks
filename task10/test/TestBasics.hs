import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on finite list" $
        head' [1,2,3] @?= 1
     
    , testCase "head' works on infinite list" $
        head' [2..] @?= 2

    , testCase "tail' works on non-empty list" $
        tail' [1,2,3] @?= [2,3]
    
    , testCase "tail' works on empty list" $
        tail' ([]::[Int]) @?= []
    
    , testCase "tail' works on infinite list" $
        take' 3 (tail' [1..]) @?= [2, 3, 4]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]
    
    , testCase "take' works on empty list" $
        take' 0 ([]::[Int]) @?= []
    
    , testCase "take' takes 4 elements from infinite list" $
        take' 4 [1..] @?= [1, 2, 3, 4]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]
    
    , testCase "drop' drops 5 elements from 3-element list" $
        drop' 5 [1,2,3] @?= []
    
    , testCase "drop' drops 1 element from empty list" $
        drop' 1 ([]::[Int]) @?= []
    
    , testCase "drop' drops 5 elements from infinite list" $
        take' 10 (drop' 5 [1..]) @?= [6..15]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]
    
    , testCase "filter' works on empty list" $
        filter' even ([]::[Int]) @?= []
    
    , testCase "filter' selects only odd numbers from infinite list" $
        take' 3 (filter' odd [1..]) @?= [1, 3, 5]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6
    
    , testCase "foldl'' works on empty list" $
        foldl'' (+) 3 ([]::[Int]) @?= 3

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]
    
    , testCase "concat' works on empty lists as expected" $
        concat' ([]::[Int]) ([]::[Int]) @?= []
    
    , testCase "concat' works on infinite lists as expected" $
        take' 10 (concat' [1..] [2..]) @?= [1..10]
    
    , testCase "concat' works on finite and infinite lists as expected" $
        take' 4 (concat' [1..2] [2..]) @?= [1, 2, 2, 3]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    
    , testCase "quickSort actualy sorts empty list" $
        quickSort' ([]::[Int]) @?= []
    
    , testCase "quickSort actualy sorts large list" $
        quickSort' [2, 4, 3, 3, 5, 2, 3, 4] @?= [2, 2, 3, 3, 3, 4, 4, 5]
    ]
