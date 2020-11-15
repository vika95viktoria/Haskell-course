import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    -- fromList [] = empty
    -- fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

-- instance MapLike ListMap where
--     empty = ListMap {getListMap = []}    
--     lookup key mapLike = if length searchResult == 0 then Nothing else Just (snd $ head searchResult)
--                             where searchResult = filter (\(k, v) -> k == key) (getListMap mapLike)
--     insert key val mapLike = ListMap {getListMap = helper existing}
--                              where existing = lookup key mapLike   
--                                    ls = getListMap mapLike
--                                    helper Nothing = (key,val):ls
--                                    helper (Just value) = map (\(k,v) -> if k == key then (k, val) else (k, v)) ls 
--     delete key mapLike = fromList $ filter (\(k,v) -> k /= key) (getListMap mapLike )                                  



newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    lookup k arrMap = (getArrowMap arrMap) k
    empty = ArrowMap{getArrowMap = (\x -> Nothing)}
    insert key val arrMap = ArrowMap{getArrowMap = (\x -> if x == key then Just val else oldFun x)}
                             where oldFun = getArrowMap arrMap
    delete key arrMap = ArrowMap{getArrowMap = (\x -> if x == key then Nothing else oldFun x)}
                                      where oldFun = getArrowMap arrMap
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)                                  

-- helper k = Maybe b    