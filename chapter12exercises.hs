main = do putStrLn (show (primes !! 100000))

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /= 0]

-- Ex 12.4
fibs :: [Integer]
fibs = 0:1:[a + b | (a, b) <- zip fibs (tail fibs)]
       
-- Note, this didn't work, not sure why...   
-- Ahh, no, I know why.  It's because it forms the cartesian product
-- of a and b values. a is infinite, so the first value of a (0) gets 
-- added to all of b's values, which just end up equalling 1... 
fibswrong :: [Integer]
fibswrong = 0:1:[a + b | a <- fibswrong
                       , b <- tail fibswrong]

-- This also doesn't work, but produces a different list - it will depend on the
-- way / order the values are generated for a abd b - which is last first (so
-- scoping works, like b is an inner loop, and its extent can depend on a)
fibswrong' :: [Integer]
fibswrong' = 0:1:[a + b | a <- tail fibswrong'
                        , b <- fibswrong']

fib :: Int->Integer
fib n = fibs !! n

firstfibover1000 :: Integer
firstfibover1000 = head (dropWhile (<= 1000) fibs)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeatTree :: a -> Tree a
repeatTree x = t where t = Node t x t
-- Huttons answer: repeatTree x = Node t x t where t = repeatTree x
                    
takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = Leaf
takeTree n Leaf = Leaf
takeTree n (Node l x r) = Node (takeTree (n - 1) l) x (takeTree (n - 1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree 


 
