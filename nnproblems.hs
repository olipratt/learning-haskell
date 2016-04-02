-- Solutions to: https://wiki.haskell.org/99_questions/1_to_10

-- Some initial code to set up a basic unit test framework.
assert :: Bool -> [Char]
assert False = "Assert FAILED"
assert True = "Assert passed"

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : (intersperse sep xs)

concatLists :: [[a]] -> [a]
concatLists = foldl (++) []

joinArrWithSep :: [a] -> [[a]] -> [a]
joinArrWithSep _ [] = []
joinArrWithSep _ [x] = x
joinArrWithSep sep (x:xs) = x ++ sep ++ (joinArrWithSep sep xs)

formatResults :: [[[Char]]] -> [Char]
formatResults = joinArrWithSep "" . joinArrWithSep ["\n"]

runTest :: Int -> [[Char]] -> [[Char]]
runTest n results = ["Start of test " ++ (show n) ++ ": "] ++
                      [(joinArrWithSep ", " results)]

runTestList :: [[[Char]]] -> [Char]
runTestList = formatResults . map(uncurry(runTest)) . zip [1,2..]

runPrintTestList :: [[[Char]]] -> IO ()
runPrintTestList = putStrLn . runTestList

-- 1.  Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

test1 :: [[Char]]
test1 = [assert ((myLast "Hello, World!") == '!')]

-- 2. Find the last but one element of a list.
myLastButOne :: [a] -> a
myLastButOne [] = error "No last but one for empty lists"
myLastButOne [z] = error "No last but one for single element lists"
myLastButOne [x, _] = x
myLastButOne (_:xs) = myLastButOne xs

test2 = [assert ((myLastButOne "Hello, World!") == 'd')]

-- 3. Find the K'th element of a list. The first element is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Invalid for empty lists"
elementAt _ ii | ii <= 0 = error "Index must be greater than zero"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

test3 = [assert ((elementAt "Hello, World!" 2) == 'e')]

-- 4. Find the number of elements in a list.
numElements :: [a] -> Int
numElements [] = 0
numElements (x:xs) = 1 + (numElements xs)

test4 = [assert ((numElements "") == 0),
         assert ((numElements "Hello, World!") == 13)]

-- 5. Reverse a list.
listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = (listReverse xs) ++ [x]

test5 = [assert ((listReverse "") == ""),
         assert ((listReverse "Hello, World!") == "!dlroW ,olleH")]

-- 6. Find out if a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (listReverse x)

test6 = [assert ((isPalindrome "") == True),
         assert ((isPalindrome "Hello, World!") == False),
         assert ((isPalindrome "Hello, ,olleH") == True)]

-- 7. Flatten a nested list.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

test7 = [assert ((flatten (Elem 5)) == [5]),
         assert ((flatten (List [Elem 1, List [Elem 2,
                              List [Elem 3, Elem 4], Elem 5]])) == [1,2,3,4,5]),
         assert ((flatten (List [Elem 5, List []])) == [5])]

-- 8. Eliminate consecutive duplicates in list.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (x:xs)
                  | x /= y = x:compress(y:xs)

test8 = [assert ((compress "") == ""),
         assert ((compress "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") == "a"),
         assert ((compress "Hello, World!") == "Helo, World!")]

-- 9. Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (== x) (x:xs)) : pack (dropWhile (== x) (x:xs))

test9 = [assert ((pack "") == []),
         assert ((pack "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") ==
                    ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]),
         assert ((pack "Hello, World!") == ["H", "e", "ll", "o", ",", " ",
                                            "W", "o", "r", "l", "d", "!"])]

-- 10. Run length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x , head x)) . pack

test10 = [assert ((encode "") == []),
          assert ((encode "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") == [(30, 'a')]),
          assert ((encode "Hello, World!") == [(1, 'H'), (1, 'e'), (2, 'l'),
                                               (1, 'o'), (1, ','), (1, ' '),
                                               (1, 'W'), (1, 'o'), (1, 'r'),
                                               (1, 'l'), (1, 'd'), (1, '!')])]

-- 11. Run length encoding of a list, with only duplicates numbered.
data RunLenEle a = Single a | Multiple Int a
  deriving (Show)

instance Eq a => Eq (RunLenEle a) where
  (Single x) == (Single y) = x == y
  (Multiple n x) == (Multiple m y) = (n == m) && (x == y)
  _ == _ = False

dupOnlyEncode :: Eq a => [a] -> [RunLenEle a]
dupOnlyEncode = map tupleRLECons . encode
  where
    tupleRLECons (1, x) = Single x
    tupleRLECons (n, x) = Multiple n x

test11 = [assert ((dupOnlyEncode "") == []),
          assert ((dupOnlyEncode "aaaaaaaaaaaaaaaaaaaa") == [ Multiple 20 'a']),
          assert ((dupOnlyEncode "Hello, World!") ==
                  [Single 'H', Single 'e', Multiple 2 'l', Single 'o',
                   Single ',', Single ' ', Single 'W', Single 'o', Single 'r',
                   Single 'l', Single 'd', Single '!'])]

-- 12. Decode a list encoded as above.
expandRLE :: RunLenEle a -> [a]
expandRLE (Single x) = [x]
expandRLE (Multiple n x) = replicate n x

decodeRLEList :: Eq a => [RunLenEle a] -> [a]
decodeRLEList = concatMap expandRLE

test12 = [assert (length (decodeRLEList [] :: [RunLenEle Char]) == 0),
          assert ((decodeRLEList [ Multiple 20 'a']) == "aaaaaaaaaaaaaaaaaaaa"),
          assert ((decodeRLEList [Single 'H', Single 'e', Multiple 2 'l',
                                  Single 'o', Single ',', Single ' ',
                                  Single 'W', Single 'o', Single 'r',
                                  Single 'l', Single 'd', Single '!']) ==
                  "Hello, World!")]

-- 13. Run-length encode a list without creating sublists - SKIPPED.

-- 14. Duplicate elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

test14 = [assert ((dupli "") == ""),
          assert ((dupli "aaaaaaa") == "aaaaaaaaaaaaaa"),
          assert ((dupli "Hello, World!") == "HHeelllloo,,  WWoorrlldd!!")]

-- 15. Replicate elements of a list a given number of times.
repli :: Int -> [a] -> [a]
repli n x = concatMap (replicate n) x

test15 = [assert ((repli 2 "") == ""),
          assert ((repli 0 "aaaaaaa") == ""),
          assert ((repli 2 "aaaaaaa") == "aaaaaaaaaaaaaa"),
          assert ((repli 1 "Hello, World!") == "Hello, World!"),
          assert ((repli 2 "Hello, World!") == "HHeelllloo,,  WWoorrlldd!!")]

-- 16. Drop every nth element of a list.
dropEvery :: Int -> [a] -> [a]
dropEvery 0 xs = xs
dropEvery n [] = []
dropEvery n xs = (take (n - 1) xs) ++ (dropEvery n (drop n xs))

test16 = [assert ((dropEvery 2 "") == ""),
          assert ((dropEvery 0 "aaaaaaa") == "aaaaaaa"),
          assert ((dropEvery 1 "aaaaaaa") == ""),
          assert ((dropEvery 2 "aaaaaaa") == "aaaa"),
          assert ((dropEvery 4 "Hello, World!") == "Helo, orl!")]

-- 17. Split a list into two parts, with the length of the first part given.
split :: Int -> [a] -> [[a]]
split n xs = [(take (n) xs), (drop n xs)]

test17 = [assert ((split 2 "") == ["", ""]),
          assert ((split 0 "aaaaaaa") == ["", "aaaaaaa"]),
          assert ((split 1 "aaaaaaa") == ["a", "aaaaaa"]),
          assert ((split 2 "aaaaaaa") == ["aa", "aaaaa"]),
          assert ((split 4 "Hello, World!") == ["Hell", "o, World!"])]




-- Run the program.
main = runPrintTestList [test1,
                         test2,
                         test3,
                         test4,
                         test5,
                         test6,
                         test7,
                         test8,
                         test9,
                         test10,
                         test11,
                         test12,
                         ["Skipped"],
                         test14,
                         test15,
                         test16,
                         test17]