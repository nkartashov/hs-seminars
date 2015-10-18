{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (lookup)
import Test.HUnit
import qualified Data.List as L

-- 1. Пусть определен класс типов контейнеров MapLike.
--    Напишите реализацию по умолчанию для fromList. (0.5 балла)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList = undefined

-- 2. Определить instance MapLike для TreeMap (двоичное дерево поиска без балансировки),
--    ListMap и ArrMap.
--    Можно использовать любые стандартные функции. (2 балла)

data TreeMap k v = Leaf | Branch k v (TreeMap k v) (TreeMap k v) deriving Show
newtype ListMap k v = ListMap { getListMap :: [(k,v)] } deriving Show
newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike TreeMap where
    empty = undefined
    lookup = undefined
    insert = undefined
    delete = undefined

instance MapLike ListMap where
    empty = undefined
    lookup = undefined
    insert = undefined
    delete = undefined

instance MapLike ArrMap where
    empty = undefined
    lookup = undefined
    insert = undefined
    delete = undefined

-- 3. Определите метод toList для Map (0.5 балла)
toList :: TreeMap k v -> [(k, v)]
toList = undefined

-- 4. Определите instance Eq для TreeMap и ListMap. (0.5 баллa)

instance Eq (TreeMap k v) where
    (==) = undefined

instance Eq (ListMap k v) where
    (==) = undefined

-- 3. Написать instance Functor для ListMap k и ArrMap k. (0.5 баллa)

instance Functor (ListMap k) where
    fmap = undefined

instance Functor (ArrMap k) where
    fmap = undefined

------------------------------------------------------------------------------

-- tests

fromListTests = [ (empty :: TreeMap () ()) ~?= fromList []
                , (empty :: ListMap () ()) ~?= fromList []
                ]

sort :: Ord a => [a] -> [a]
sort = map fst . toList . (fromList :: Ord a => [(a, ())] -> TreeMap a ()) . map (\x -> (x, ()))

listMapTests = [ empty ~?= ListMap ([] :: [(Int, Bool)])
    , lookup 3 (ListMap [(1, 'x'), (2, 'y'), (4, 'z')]) ~?= Nothing
    , lookup 'x' (ListMap [('a', 1), ('b', 2), ('c', 3), ('x', 10), ('y', 5)]) ~?= Just 10
    , L.sort (getListMap $ insert 5 "foo" (ListMap list)) ~?= L.sort ((5, "foo") : list)
    , L.sort (getListMap $ insert 6 "" (ListMap list)) ~?= L.sort ((6, "") : list')
    , L.sort (getListMap (delete 4 (ListMap list))) ~?= L.sort list
    , L.sort (getListMap (delete 6 (ListMap list))) ~?= L.sort list'
    , L.sort (getListMap (fromList list)) ~?= L.sort list
    ]
        where
            list' = [(1, "a"), (2, "3"), (7, "bar")]
            list = [(1, "a"),(2, "3"), (6, "qwerty"), (7, "bar")]

arrMapTests = [ lookup 3 (ListMap [(1, 'x'), (2, 'y'), (4, 'z')]) ~?= Nothing
    , lookup 'x' list1 ~?= Just 10
    , lookup 6 (insert 5 "foo" (fromList list :: ArrMap Int String)) ~?= Just "qwerty"
    , lookup 5 (insert 5 "foo" (fromList list :: ArrMap Int String)) ~?= Just "foo"
    , lookup 6 (insert 6 "" (fromList list :: ArrMap Int String)) ~?= Just ""
    , lookup 4 (delete 4 (fromList list :: ArrMap Int String)) ~?= Nothing
    , lookup 6 (delete 6 (fromList list :: ArrMap Int String)) ~?= Nothing
    ]
        where
            list1 = fromList [('a', 1), ('b', 2), ('c', 3), ('x', 10), ('y', 5)] :: ArrMap Char Int
            list' = [(1, "a"), (2, "3"), (7, "bar")]
            list = [(1, "a"),(2, "3"), (6, "qwerty"), (7, "bar")]

functorTests = [ test length (fromList list1 :: ArrMap Int String) queries1
               , testListMap (++ "bar") list1
               ]
        where
            list1 = [(1, "a"), (2, "3"), (6, "qwerty"), (7, "bar")]
            queries1 = [1, 6, 47]
            qsequence m = map (\q -> lookup q m)
            resultMap f = map (fmap f)
            test f m qs = (resultMap f $ qsequence m qs) ~?= (resultMap f $ qsequence (ListMap list1) qs)
            testListMap f l = (map snd $ getListMap $ fmap f m) ~?= (fmap f $ map snd $ getListMap m)
                where m = ListMap l

a ~?/= b = TestCase $ assert $ a /= b

eqTests = [ (fromList [(1, 1)] :: TreeMap Int Int) ~?/= (fromList [] :: TreeMap Int Int)
          , (fromList [(1, 1)] :: ListMap Int Int) ~?/= (fromList [] :: ListMap Int Int)
          ]

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
      $  label "FromList" fromListTests
      ++ label "TreeMap" -- можете сами написать тесты на каждую функцию :)
            [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
      ++ label "ListMap" listMapTests
      ++ label "ArrMap" arrMapTests
      ++ label "Functor" functorTests
      ++ label "Eq" eqTests
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
