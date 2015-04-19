myexp :: Int -> Int -> Int
m `myexp` 0 = 1
m `myexp` (n + 1) = m * (m `myexp` n)

myand :: [Bool]->Bool
myand [] = True
myand (True:xs) = myand xs
myand _ = False

myconcat1 :: [[a]] -> [a]
myconcat1 [] = []
myconcat1 (x:xs) = x ++ myconcat1 xs

myconcat2 :: [[a]] -> [a]
myconcat2 xs = myconcat' xs []

myconcat' :: [[a]]->[a]->[a]
myconcat' [] ys = ys
myconcat' (x:xs) ys = myconcat' xs (ys ++ x)

myconcat3 :: [[a]]->[a]
myconcat3 = foldr (++) []

myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate (n + 1) x = x:(myreplicate n x)

elementat :: [a]->Int->a
elementat (x:_) 0 = x
elementat (_:xs) (n + 1) = elementat xs n

myelem :: Eq a => a->[a]->Bool
myelem y [] = False
myelem y (x:xs) | y == x = True 
                | otherwise = myelem y xs
                              
mymerge :: Ord a => [a]->[a]->[a]                              
mymerge xs [] = xs
mymerge [] ys = ys
mymerge (x:xs) (y:ys) | x < y = x:(mymerge xs (y:ys)) 
                      | otherwise = y:(mymerge (x:xs) ys)
                                    
myhalve :: [a]->([a],[a])
myhalve [] = ([],[])
myhalve xs = (take l xs, drop l xs)
             where
               l = length xs `div` 2
               
mymergesort :: Ord a => [a]->[a]
mymergesort [] = []
mymergesort [x] = [x]
mymergesort xs = mymerge (mymergesort ls) (mymergesort rs)
                 where (ls, rs) = myhalve xs

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake (n + 1) [] = []
mytake (n + 1) (x:xs) = x:(mytake n xs)

mylast :: [a]->a
mylast [x] = x
mylast (_:xs) = mylast xs 
