
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving show

b = Node (Leaf) "Hello" (Leaf)
