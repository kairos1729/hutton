import Data.List

-- countdown
data Op = Add | Sub | Mul | Div | Exp deriving (Show)

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _  = True
valid Div x y = x `mod` y == 0
valid Exp x y = ((toInteger x ^ toInteger y) < 6500) -- try to prevent overflow

-- Ex 11.5
{-
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _  = True
valid Div x y = (y /= 0) && (x `mod` y == 0)
-}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr deriving (Show)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

depth :: Expr -> Int
depth (Val _) = 1
depth (App _ l r) = 1 + max (depth l) (depth r)

score :: Expr -> Int
score e = opsscore e + 1000 * depth e

opsscore :: Expr -> Int
opsscore (Val _) = 0
opsscore (App o l r) = (opscore o) + opsscore l + opsscore r  

opscore :: Op -> Int
opscore Add = 1
opscore Sub = 2
opscore Mul = 3 
opscore Div = 4
opscore Exp = 5

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y 
                   | x <- eval l,
                     y <- eval r,
                     valid o x y]
                   
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
                    
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) 

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices' :: [a] -> [[a]]
choices' xs = concat (map perms (subs xs))

-- Ex 11.1
choices :: [a] -> [[a]]
choices xs = [perm | s <- subs xs
                   , perm <- perms s]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e 
           | (ls, rs) <- split ns, 
             l <- exprs ls,
             r <- exprs rs,
             e <- combine l r]
           
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div] --, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e 
                 | ns' <- choices ns,
                   e <- exprs ns',
                   eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res 
             | (ls, rs) <- split ns,
               lx <- results ls,
               ry <- results rs,
               res <- combine' lx ry]
             
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops
                                                 , valid o x y]
                       
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e 
                  | ns' <- choices ns
                  , (e, m) <- results ns'
                  , m == n]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns
                      , (e, m) <- results' ns'
                      , m == n]

solutions''' :: [Int] -> Int -> [(Expr, Int, Int)]
solutions''' ns n = [(e, m, delta) | (e, m, delta) <- rs
                                   , (abs delta) <= mindelta]
  where 
    rs = [(e, m, m - n) | ns' <- choices ns
                        , (e, m) <- results' ns']
    mindelta = minimum [abs delta | (_, _, delta) <- rs]
                                             
results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns
                   , lx <- results' ls
                   , ry <- results' rs
                   , res <- combine'' lx ry]
             
combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops
                                                  , valid' o x y]
                        
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0
valid' Exp x y = y /= 1 
                 && x /= 1 
                 && ((toInteger x ^ toInteger y) < 6500) -- try to prevent overflow

main = countdown [1,3,7,10,25,50] 765
-- main = countdown [1,2,3] 765

-- ex 11.4
{-
main = do putStrLn (show (length [e 
                                 | ns <- choices [1,3,7,10,25,50]
                                 , e <- exprs ns]))
          putStrLn (show (length [v 
                               | ns <- choices [1,3,7,10,25,50]
                               , e <- exprs ns
                               , v <- eval e]))
-}          

countdown :: [Int] -> Int -> IO()
countdown ns n = seqn [putStrLn (
                          show i ++ ": " ++ 
                          "value=" ++ show m ++ ", " ++ 
--                          "delta=" ++ show delta ++ ", " ++ 
--                          "depth=" ++ show (depth e) ++ " " ++
--                          "opsscore=" ++ show (opsscore e) ++ " " ++
                          "score=" ++ show (score e) ++ " " ++
                          expr2str e) | 
                       (i, (e, m, delta)) <- 
                         zip 
                         [1..] 
                         (sortBy comparescore (solutions''' ns n))]

comparescore :: (Expr, Int, Int) -> (Expr, Int, Int) -> Ordering
comparescore (e1, _, _) (e2, _, _) = compare (score e1) (score e2)


seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs
                  
expr2str :: Expr -> String
expr2str (Val v) = show v
expr2str (App o l r) = "(" ++ expr2str l ++ op2str o ++ expr2str r ++ ")" 

op2str :: Op -> String
op2str Add = " + "
op2str Sub = " - "
op2str Mul = " * "
op2str Div = " / "
op2str Exp = " ^ "

-- Ex 11.2
-- My Answer:
{-
withoutfirst :: (Eq a) => [a] -> a -> [a]
withoutfirst [] _ = []
withoutfirst (x:xs) y | x == y = xs
                      | otherwise = x : xs `withoutfirst` y

isChoice :: (Eq a) => [a]->[a]->Bool
isChoice [] _ = True
isChoice (x:xs) from | x `elem` from = isChoice xs (from `withoutfirst` x)
                     | otherwise = False
-}

-- Huttons (nicer):
removeone :: (Eq a) => a -> [a] -> [a]
removeone _ [] = []
removeone x (y:ys) 
  | x == y = ys
  | otherwise = y: removeone x ys

isChoice :: (Eq a) => [a]->[a]->Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)

blah :: Integer->Integer
blah x = 2 ^ x

