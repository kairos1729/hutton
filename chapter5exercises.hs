import Data.Char

onehundredintsquares = sum [x ^ 2| x <- [1..100]]

myreplicate n x = [x | _ <- [1..n]]

pyths::Int->[(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]

factors::Int->[Int]
factors n = [x | x<-[1..n], n `mod` x == 0]

perfects::Int->[Int]
perfects n = [x|x<-[1..n], x == sum (init (factors x))]

comp1 = [(x,y)|x<-[1,2,3],y<-[4,5,6]]

comp2 = concat [[(x,y)|y<-[4,5,6]]|x<-[1,2,3]]

find::Eq a=>a->[(a,b)]->[b]
find k t = [v | (k',v)<-t, k==k']

positions::Eq a=>a->[a]->[Int]
positions x xs = find x (zip xs [0..n])
                 where n = length xs - 1

scalarproduct::[Int]->[Int]->Int
scalarproduct xs ys = sum [x * y | (x,y)<-zip xs ys]

-- Caesar Cipher
lowers xs = length [x | x<-xs, isLower x]
alphas xs = length [x | x<-xs, isAlpha x]
count x xs = length [x'|x'<-xs,x'==x]
let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)
let2intCap c = ord c - ord 'A'
int2letCap n = chr (ord 'A' + n)

shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2letCap ((let2intCap c + n) `mod` 26)
          | otherwise = c
encode n xs = [shift n x|x<-xs]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
         6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]
percent n m = (fromIntegral n / fromIntegral m) * 100
freqs xs = [percent (count x ls) n | x<-['a'..'z']]
          where 
            ls = map toLower xs
            n = lowers ls
chisqr os es = sum [((o - e)^2)/e|(o,e)<-zip os es]
rotate n xs = drop n xs ++ take n xs
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n<-[0..25]]
             table' = freqs xs
