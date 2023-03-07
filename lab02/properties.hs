{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

prop_add :: [Int] -> [Int] -> [Int] -> Bool
prop_add xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

prop_add2 :: [Int] -> [Int] -> Bool
prop_add2 xs ys = xs ++ ys == ys ++ xs

prop_concat :: [[Int]] -> [[Int]] -> Bool
prop_concat xs ys = concat (xs ++ ys) == (concat xs) ++ (concat ys)

prop_take :: Int -> [Int] -> Int -> [Int] -> Bool
prop_take n xs m ys = (take n xs) ++ (take m ys) == take (n + m) (xs ++ ys)

prop_drop :: Int -> [Int] -> Int -> [Int] -> Bool
prop_drop n xs m ys = (drop n xs) ++ (drop m ys) == drop (n + m) (xs ++ ys)

return []
runTests = $quickCheckAll

main = runTests
