{-|
  Определение класса типов 'Map'.
-}

{-# LANGUAGE LambdaCase #-}

module Map where

{-|
  Поведение всех определённых здесь функций должно быть аналогично
  поведению функций из модуля "Data.Map.Strict".
  Каждую функцию, у которой здесь предложена реализация по умолчанию
  в виде 'undefined', вам требуется выразить через другую функцию из
  класса 'Map', указанную в комментарии. Например, 'fromList'
  требуется выразить через 'insert' (и, возможно, какие-то другие
  стандартные функции).
  Оставшиеся шесть функций считаются минимальной реализацией.
  Обратите внимание, что имена функций @lookup@ и @null@ совпадают
  с определёнными в стандартной библиотеке, поэтому для обращения к ним
  требуется писать @Map.lookup@ и @Map.null@, иначе компилятор не поймёт,
  какую из двух функций вы хотите.
  Строго говоря, 'alter' и 'Map.lookup' можно обобщить до функции
  вроде 'Data.Map.Strict.alterF', которая позволяет при изменении
  'Map' ещё и вытащить наружу старое значение, но мы этим заниматься
  не будем.
-}
class Map t where
    empty :: Ord k => t k a

    singleton :: k -> a -> t k a

    fromList :: Ord k => [(k, a)] -> t k a
    fromList = foldl (\t (key, value) -> insert key value t) empty

    toAscList :: t k a -> [(k, a)]

    insert :: Ord k => k -> a -> t k a -> t k a
    insert = insertWith const

    insertWith :: Ord k => (a -> a -> a) -> k -> a -> t k a -> t k a
    insertWith f key value = alter (\case 
            Nothing -> Just value
            Just x  -> Just (f value x)) key

    insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> t k a -> t k a
    insertWithKey f key = insertWith (f key) key

    delete :: Ord k => k -> t k a -> t k a
    delete = alter (const Nothing)

    adjust :: Ord k => (a -> a) -> k -> t k a -> t k a
    adjust f = alter (\case
            Nothing -> Nothing
            Just x  -> Just (f x))

    adjustWithKey :: Ord k => (k -> a -> a) -> k -> t k a -> t k a
    adjustWithKey f key = adjust (f key) key

    update :: Ord k => (a -> Maybe a) -> k -> t k a -> t k a
    update f = alter (\case
            Nothing -> Nothing
            Just x  -> f x)

    updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> t k a -> t k a
    updateWithKey f key = update (f key) key

    alter :: Ord k => (Maybe a -> Maybe a) -> k -> t k a -> t k a

    lookup :: Ord k => k -> t k a -> Maybe a

    member :: Ord k => k -> t k a -> Bool
    member k t = case Map.lookup k t of
            Nothing -> False
            Just _  -> True

    notMember :: Ord k => k -> t k a -> Bool
    notMember k t = not (member k t)

    null :: t k a -> Bool

    size :: t k a -> Int
