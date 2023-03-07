import Test.HUnit

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = ys ++ [x] ++ zs where
    ys = qsort [y | y <- xs, y <= x]
    zs = qsort [z | z <- xs, z > x]

test1 = TestCase $ assertEqual "qsort [2,3,1] == [1,2,3]" (qsort [2,3,1]) [1,2,3]
test2 = TestCase $ assertEqual "qsort [] == []" (qsort []) []
test3 = TestCase $ assertEqual "qsort [5,4,3,2,1] == [1,2,3,4,5]" (qsort [5,4,3,2,1]) [1,2,3,4,5]
test4 = TestCase $ assertEqual "qsort [2,2,1,3,1,3] == [1,1,2,2,3,3]" (qsort [2,2,1,3,1,3]) [1,1,2,2,3,3]
test5 = TestCase $ assertEqual "qsort [1,1,1] == [1,1,1]" (qsort [1,1,1]) [1,1,1]
test6 = TestCase $ assertEqual "qsort [1,2,3] == [1,2,3]" (qsort [1,2,3]) [1,2,3]

tests = TestList [test1, test2, test3, test4, test5, test6]

main = runTestTT tests
