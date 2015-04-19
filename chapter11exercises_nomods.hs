-- countdown
data Op = Add | Sub | Mul | Div | deriving (Show)


valid :: Op -> Integer -> Integer -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _  = True
valid Div x y = x `mod` y == 0

-- Ex 11.5
{-
valid :: Op -> Integer -> Integer -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _  = True
valid Div x y = (y /= 0) && (x `mod` y == 0)
-}

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Integer | App Op Expr Expr deriving (Show)

values :: Expr -> [Integer]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Integer]
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

solution :: Expr -> [Integer] -> Integer -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Integer] -> [Expr]
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
ops = [Add, Sub, Mul, Div]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = [e 
                 | ns' <- choices ns,
                   e <- exprs ns',
                   eval e == [n]]

type Result = (Expr, Integer)

results :: [Integer] -> [Result]
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
                       
solutions' :: [Integer] -> Integer -> [Expr]
solutions' ns n = [e 
                  | ns' <- choices ns
                  , (e, m) <- results ns'
                  , m == n]

solutions'' :: [Integer] -> Integer -> [Expr]
solutions'' ns n = [e | ns' <- choices ns
                      , (e, m) <- results' ns'
                      , m == n]
                              
results' :: [Integer] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns
                   , lx <- results' ls
                   , ry <- results' rs
                   , res <- combine'' lx ry]
             
combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops
                                                  , valid' o x y]
                        
valid' :: Op -> Integer -> Integer -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

main = countdown [1,3,7,10,25,50] 765
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

countdown :: [Integer] -> Integer -> IO()
countdown ns n = seqn [putStrLn (show i ++ ": " ++ expr2str s) | 
                       (i, s) <- zip [1..] (solutions'' ns n)]

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

