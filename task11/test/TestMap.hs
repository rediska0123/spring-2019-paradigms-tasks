{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.
  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.
  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Test toAscList, fromList" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (4, "x")] :: m Int String in
                toAscList tr @?= [(1, "b"), (2, "a"), (3, "c"), (4, "x")],
            testCase "toAscList . fromList sorts list: two identical keys" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],
        testGroup "Test insert" [
            testCase "insert to an empty tree" $
                let map = insert 5 "x" (empty :: m Int String) in
                (size map, Map.lookup 5 map) @?= (1, Just "x"),
            testCase "insert unique key" $
                let map = insert 7 "x" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (3, Just "x"),
            testCase "insert existing key" $
                let map = insert 5 "x" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "x")
        ],
        testGroup "Test insertWith" [
            testCase "insert to an empty tree" $
                let map = insertWith (++) 5 "x" (empty :: m Int String) in
                (size map, Map.lookup 5 map) @?= (1, Just "x"),
            testCase "insert unique key" $
                let map = insertWith (++) 7 "xxx" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (3, Just "xxx"),
            testCase "insert existing key" $
                let map = insertWith (++) 5 "xxx" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "xxxa")
        ],
        testGroup "Test insertWithKey" [
            testCase "insert to an empty tree" $
                let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value in
                let map = insertWithKey f 5 "xxx" (empty :: m Int String) in
                (size map, Map.lookup 5 map) @?= (1, Just "xxx"),
            testCase "insert unique key" $
                let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value in
                let map = insertWithKey f 7 "xxx" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (3, Just "xxx"),
            testCase "insert existing key" $
                let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value in
                let map = insertWithKey f 5 "xxx" (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "5:xxx|a")
        ],
        testGroup "Test delete" [
            testCase "delete from an empty tree" $
                let map = delete 5 empty :: m Int String in
                Map.null map @?= True,
            testCase "delete unique key" $
                let map = delete 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "delete existing key" $
                let map = delete 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (1, Nothing)
        ],
        testGroup "Test adjust" [
            testCase "adjust from an empty tree" $
                let map = adjust ("new " ++) 7 empty :: m Int String in
                Map.null map @?= True,
            testCase "adjust unique key" $
                let map = adjust ("new " ++) 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "adjust existing key" $
                let map = adjust ("new " ++) 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "new a")
        ],
        testGroup "Test adjustWithKey" [
            testCase "adjust from an empty tree" $
                let f key x = show key ++ ":new " ++ x in
                let map = adjustWithKey f 7 empty :: m Int String in
                Map.null map @?= True,
            testCase "adjust unique key" $
                let f key x = show key ++ ":new " ++ x in
                let map = adjustWithKey f 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "adjust existing key" $
                let f key x = show key ++ ":new " ++ x in
                let map = adjustWithKey f 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "5:new a")
        ],
        testGroup "Test update" [
            testCase "update an empty tree" $
                let f x = if x == "a" then Just "new a" else Nothing in
                let map = update f 3 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (1, Just "a"),
            testCase "update unique key" $
                let f x = if x == "a" then Just "new a" else Nothing in
                let map = update f 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "update existing key" $
                let f x = if x == "a" then Just "new a" else Nothing in
                let map = update f 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "new a")
        ],
        testGroup "Test updateWithKey" [
            testCase "update an empty tree" $
                let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in
                let map = updateWithKey f 3 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (1, Just "a"),
            testCase "update unique key" $
                let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in
                let map = updateWithKey f 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "update existing key" $
                let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in
                let map = updateWithKey f 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "5:new a")
        ],
        testGroup "Test alter" [
            testCase "alter no changes" $
                let f _ = Nothing in
                let map = alter f 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (2, Nothing),
            testCase "alter: delete" $
                let f _ = Nothing in
                let map = alter f 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 3 map) @?= (1, Just "b"),
            testCase "alter: insert" $
                let f _ = Just "c" in
                let map = alter f 7 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 7 map) @?= (3, Just "c"),
            testCase "alter: update" $
                let f _ = Just "c" in
                let map = alter f 5 (fromList [(5, "a"), (3, "b")]) :: m Int String in
                (size map, Map.lookup 5 map) @?= (2, Just "c")
        ],
        testGroup "Test lookup" [
            testCase "lookup unique key" $
                let map = fromList [(5, "a"), (3, "b")] :: m Int String in
                Map.lookup 7 map @?= Nothing,
            testCase "lookup existing key" $
                let map = fromList [(5, "a"), (3, "b")] :: m Int String in
                Map.lookup 5 map @?= Just "a"
        ],
        testGroup "Test member and notMember" [
            testCase "not a member" $
                let map = fromList [(5, "a"), (3, "b")] :: m Int String in
                (member 7 map, notMember 7 map) @?= (False, True),
            testCase "a member" $
                let map = fromList [(5, "a"), (3, "b")] :: m Int String in
                (member 5 map, notMember 5 map) @?= (True, False)                
        ],
        testGroup "Test null" [
            testCase "empty" $
                Map.null (empty :: m Int String) @?= True,
            testCase "not empty" $
                Map.null (fromList [(5, "a"), (3, "b")] :: m Int String) @?= False
        ],
        testGroup "Test size" [
            testCase "size 2" $
                size (fromList [(5, "a"), (3, "b")] :: m Int String) @?= 2,
            testCase "size 0" $
                size (empty :: m Int String) @?= 0
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 2 "b" (Node 1 "a" Nil Nil) Nil
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
