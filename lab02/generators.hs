import Control.Monad
import Data.List
import Test.QuickCheck

newtype OrderedInts = OrderedInts [Int] deriving Show

ordered :: [Int] -> Bool
ordered (x:y:ys) = x <= y && ordered (y:ys)
ordered _ = True

prop_insert :: Int -> OrderedInts -> Bool
prop_insert x (OrderedInts xs) = ordered (insert x xs)

instance Arbitrary OrderedInts where
    arbitrary = fmap (OrderedInts . sort) (listOf arbitrary)

data BST a = Nil | Node a (BST a) (BST a) deriving (Eq, Show)

isBST :: Ord a => BST a -> Bool
isBST Nil = True
isBST (Node x l r) = isBST l && isBST r && ok where
    ok = case (l, r) of
        (Nil, Nil) -> True
        (Node y _ _, Nil) -> x >= y
        (Nil, Node z _ _) -> x <= z
        (Node y _ _, Node z _ _) -> x >= y && x <= z

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Nil = Node x Nil Nil
insertBST x (Node y l r)
    | x < y = (Node y (insertBST x l) r)
    | otherwise = (Node y l (insertBST x r))

prop_insert_BST :: Int -> BST Int -> Property
prop_insert_BST x tree = isBST tree ==> isBST $ insertBST x tree

instance (Arbitrary a, Ord a) => Arbitrary (BST a) where
    arbitrary = sized arbTree where
        arbTree 0 = return Nil
        arbTree n = frequency [
            (1, return Nil),
            (4, insertBST <$> arbitrary <*> arbTree (n `div` 2))
            ]

prop_gen_BST :: Property
prop_gen_BST = forAll (arbitrary :: Gen (BST Int)) isBST

main = do
    quickCheck prop_insert
    quickCheck prop_insert_BST
    quickCheck prop_gen_BST
    sample (arbitrary :: Gen OrderedInts)
    sample (arbitrary :: Gen (BST Int))
