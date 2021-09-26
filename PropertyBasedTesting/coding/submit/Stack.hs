module Stack (stackFoldl, stackFoldr, stackZip, stackMap) where

data Stack a  = Empty
              | Stack (Stack a) a
              deriving (Show, Eq)

stackFoldl :: (b -> a -> b) -> b -> Stack a -> b
stackFoldl f b Empty       = b
stackFoldl f b (Stack s a) = stackFoldl f (b `f` a) s

stackFoldr :: (a -> b -> b) -> b -> Stack a -> b
stackFoldr f b Empty       = b
stackFoldr f b (Stack s a) = a `f` stackFoldr f b s

stackZip :: Stack a -> Stack b -> Stack (a,b)
stackZip Empty        _            = Empty
stackZip _            Empty        = Empty
stackZip (Stack sa a) (Stack sb b) = Stack (stackZip sa sb) (a,b)

stackMap :: (a -> b) -> Stack a -> Stack b
stackMap f Empty       = Empty
stackMap f (Stack s a) = Stack (stackMap f s) $ f a