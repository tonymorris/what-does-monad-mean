-- Monad interface
class Monad' m where
  bind :: m a -> (a -> m b) -> m b
  pure :: a -> m a

newtype Value a = Value {
  val :: a
}

-- Monad instances
instance Monad' Value where
  bind (Value a) f = f a
  pure = Value

instance Monad' [] where -- aka List
  bind = (>>=)
  pure = return

instance Monad' Maybe where -- aka Option
  bind = (>>=)
  pure = return

instance Monad' ((->) t) where -- aka Function1
  bind a f = \x -> f (a x) x
  pure = const

-- Monad functions
sequence' :: (Monad' m) => [m a] -> m [a]
sequence' = foldr (\h t -> bind h (bind t . (pure .) . (:))) (pure [])

join' :: (Monad' m) => m (m a) -> m a
join' a = bind a id

replicate' :: (Monad m) => Int -> m a -> m [a]
replicate' n a = sequence (replicate n a)

-- etc. etc. Monad functions

main = do print (sequence' [[1, 2, 3], [4, 5, 6]])
          print (sequence' [Just 7, Just 8])
          print (join' [[1, 2, 3], [4, 5, 6]])
          print (join' (Just (Just 7)))
          print (replicate' 3 "abc")
          print (replicate' 4 (Just 8))

{-
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
Just [7,8]
[1,2,3,4,5,6]
Just 7
["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa",
  "bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac",
  "cba","cbb","cbc","cca","ccb","ccc"]
Just [8,8,8,8]
-}