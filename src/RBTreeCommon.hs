module RBTreeCommon ( Color(Red, Black)
                    , RBTree(Empty, Node)
                    , memberImpl
                    , insertImpl
                    , insertDividedImpl
                    , removeImpl
                    , fromOrdListImpl
                    , toOrdListImpl ) where
import Prelude hiding (rem)

data Color = Red | Black
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a)
type BalanceFun a = Color -> RBTree a -> a -> RBTree a -> RBTree a

instance Show Color where
    show Red = "R"
    show Black = "B"

instance Show a => Show (RBTree a) where
   show Empty = "_"
   show (Node color left x right) = "(" ++ show left ++ " " ++ show x ++ show color ++ " " ++ show right ++ ")"

memberImpl :: Ord a => a -> RBTree a -> Bool
memberImpl _ Empty = False
memberImpl x (Node _ left y right)
    | x < y = memberImpl x left
    | x > y = memberImpl x right
    | otherwise = True

insertImpl :: Ord a => BalanceFun a -> a -> RBTree a -> RBTree a
insertImpl balance x tree = toBlackNode $ ins tree
    where
        ins Empty = Node Red Empty x Empty
        ins tree@(Node color left y right)
            | x < y = balance color (ins left) y right
            | x > y = balance color left y (ins right)
            | otherwise = tree

insertDividedImpl :: Ord a => BalanceFun a -> BalanceFun a -> a -> RBTree a -> RBTree a
insertDividedImpl balanceL balanceR x tree = toBlackNode $ ins tree
    where
        ins Empty = Node Red Empty x Empty
        ins tree@(Node color left y right)
            | x < y = balanceL color (ins left) y right
            | x > y = balanceR color left y (ins right)
            | otherwise = tree

removeImpl :: Ord a => BalanceFun a -> a -> RBTree a -> RBTree a
removeImpl balance x tree = toBlackNode $ rem tree
    where
        rem Empty = Empty
        rem tree@(Node _ left y right)
            | x < y = remL tree
            | x > y = remR tree
            | otherwise = fuse left right

        remL (Node Red left y right) = Node Red (rem left) y right
        remL (Node Black left y right) = balL $ Node Black (rem left) y right

        remR (Node Red left y right) = Node Red left y (rem right)
        remR (Node Black left y right) = balR $ Node Black left y (rem right)

        balL (Node Black (Node Red a x b) y c) = Node Red (Node Black a x b) y c
        balL (Node Black a x (Node Black b y c)) = balance Black a x (Node Red b y c)
        balL (Node Black a w (Node Red (Node Black b x c) y (Node Black d z e))) = Node Red (Node Black a w b) x (balance Black c y (Node Red d z e))
        balL tree = tree

        balR (Node Black a x (Node Red b y c)) = Node Red a x (Node Black b y c)
        balR (Node Black (Node Black a x b) y c) = balance Black (Node Red a x b) y c
        balR (Node Black (Node Red (Node Black a w b) x (Node Black c y d)) z e) = Node Red (balance Black (Node Red a w b) x c) y (Node Black d z e)
        balR tree = tree

        fuse Empty tree = tree
        fuse tree Empty = tree
        fuse a@(Node Black _ _ _) (Node Red b x c) = Node Red (fuse a b) x c
        fuse (Node Red a x b) c@(Node Black _ _ _) = Node Red a x (fuse b c)
        fuse (Node Red l1 x r1) (Node Red l2 y r2)  =
            let m = fuse r1 l2
            in case m of
                (Node Red l3 z r3) -> Node Red (Node Red l1 x l3) z (Node Red r3 y r2)
                (Node Black _ _ _) -> Node Red l1 x (Node Red m y r2)
                Empty -> Node Red l1 x (Node Red m y r2)
        fuse (Node Black l1 x r1) (Node Black l2 y r2)  =
            let m = fuse r1 l2
            in case m of
                (Node Red l3 z r3) -> Node Red (Node Black l1 x l3) z (Node Black r3 y r2)
                (Node Black _ _ _) -> balL (Node Black l1 x (Node Black m y r2))
                Empty -> balL (Node Black l1 x (Node Black m y r2))

toBlackNode :: RBTree a -> RBTree a
toBlackNode (Node _ left y right) = Node Black left y right
toBlackNode Empty = Empty


fromOrdListImpl :: [a] -> RBTree a
fromOrdListImpl list = fst $ constructTree (length list) list
    where
        constructTree :: Int -> [a] -> (RBTree a, [a])
        constructTree 0 xs = (Empty, xs)
        constructTree 1 (x : xs) = (Node Black Empty x Empty, xs)
        constructTree 2 (x : y : xs) = (Node Black (Node Red Empty x Empty) y Empty, xs)
        constructTree len xs = do
            let len1 = (len - 1) `div` 2
            let len2 = len - len1 - 1
            
            let (node1, xs1) = constructTree len1 xs
            let x = head xs1
            let (node2, xs2) = constructTree len2 (tail xs1)

            let node = Node Black node1 x node2
            (node, xs2)

toOrdListImpl :: RBTree a -> [a]
toOrdListImpl tree = constructList tree []
    where 
        constructList Empty list = list
        constructList (Node _ l x r) list = do
            let list' = constructList r list
            constructList l (x : list')
