import Data.Char

lc1 f p xs = [f x | x <- xs, p x]

lc2 f p = map (f) . filter p

myall _ [] = True
myall p (x:xs) = p x && myall p xs

myall2 p = foldr ((&&).p) True

myall3 p = and . map p

myany p = foldr (\x y->(p x) || y) False

myany2 p = foldr ((||).p) False

myany3 p = or . map p

mytakewhile _ [] = []
mytakewhile p (x:xs) 
  | p x = x : mytakewhile p xs
  | otherwise = []

mydropwhile _ [] = []
mydropwhile p (x:xs) 
  | p x = mydropwhile p xs
  | otherwise = x:xs
                
mymap f = foldr ((:).f) []

myfilter p = foldr (\x y->if p x then x:y else y) []

dec2int = foldl (\x y->x * 10 + y) 0

compose = foldr (.) id

test (x,y) = (x,y)
 
mycurry :: ((a,b)->c)->(a->b->c)
mycurry f = \x y->f (x,y)

mycurriedtest = mycurry test

myuncurry :: (a->b->c)->((a,b)->c)
myuncurry f = \(x,y)->f x y

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


chopn _ [] = []
chopn n bits = take n bits : chopn n (drop n bits)

chop8 = chopn 8

chop9 = chopn 9

mychop8 = unfold null (take 8) (drop 8)

myunfoldmap :: (a->b)->[a]->[b]
myunfoldmap f = unfold (null) (f.head) (tail) 

myiterate f = unfold (const False) id f

type Bit = Int

bin2int :: [Bit]->Int
bin2int = foldr (\x y->x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 bits = take 8 (bits ++ repeat 0)

count e xs = length [x | x<-xs, x==e] 

parity bits = count 1 bits `mod` 2 

addparity bits = parity bits : bits

checkparity (b:bs)
  | b == parity bs = bs
  | otherwise = error "parity mismatch"
                              
encode = concat . map (addparity . make8 . int2bin . ord)

decode = map (chr . bin2int . checkparity) . chop9

transmit = decode . channel . encode

channel = id

dodgychannel = tail

transmitdodgy = decode . dodgychannel . encode

                              
