module DPSynth.Experimental.TreeCache
  ( TreeCache (..),
    TreeCacheNode (..),
    emptyTreeCache,
    queryTreeCache,
    insertTreeCache,
  )
where

import qualified Data.Map as M

newtype TreeCache k v = TreeCache (M.Map k (TreeCacheNode k v))
  deriving (Show, Eq)

data TreeCacheNode k v = TreeCacheNode v (TreeCache k v)
  deriving (Show, Eq)

emptyTreeCache :: TreeCache k v
emptyTreeCache = TreeCache M.empty

queryTreeCache :: (Ord k) => TreeCache k v -> [k] -> ([v], [k])
queryTreeCache _ [] = ([], [])
queryTreeCache (TreeCache m) (l : ls) = case M.lookup l m of
  Nothing -> ([], l : ls)
  Just (TreeCacheNode res cache) ->
    let (results, remaining) = queryTreeCache cache ls
     in (res : results, remaining)

insertTreeCache :: (Ord k) => TreeCache k v -> [(k, v)] -> TreeCache k v
insertTreeCache cache [] = cache
insertTreeCache (TreeCache m) ((i, l) : ls) = case M.lookup i m of
  Nothing ->
    TreeCache $
      M.insert i (TreeCacheNode l $ insertTreeCache (TreeCache M.empty) ls) m
  Just (TreeCacheNode res cache) ->
    TreeCache $ M.insert i (TreeCacheNode res $ insertTreeCache cache ls) m