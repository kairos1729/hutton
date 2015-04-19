import Data.Char
import Control.Monad

-- ex 10.1
data Nat = Zero | Succ Nat deriving Show

int2nat :: Int->Nat
int2nat 0 = Zero
int2nat (n + 1) = Succ (int2nat n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = add (mult n m) m

-- ex 10.2
data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) | m == n = True
                      | m < n = occurs m l -- possibly two comparisons
                      | otherwise = occurs m r

myoccurs :: Int -> Tree -> Bool
myoccurs m (Leaf n)     = m == n
myoccurs m (Node l n r) = 
  case compare m n of -- only one comparison
    LT -> myoccurs m l
    EQ -> True
    GT -> myoccurs m r

-- ex 10.3
data Treeb = Leafb Int | Nodeb Treeb Treeb deriving Show

numleaves :: Treeb -> Int
numleaves (Leafb _) = 1
numleaves (Nodeb l r) = (numleaves l) + (numleaves r)

balanced :: Treeb -> Bool
balanced (Leafb _) = True
balanced (Nodeb l r) = 
  abs((numleaves l) - (numleaves r)) <= 1 
  && (balanced l) 
  && (balanced r)
  
-- ex 10.4
halve :: [Int] -> ([Int],[Int])
halve xs = splitAt (length xs `div` 2) xs

balance :: [Int] -> Treeb
balance [] = error "Can't balance empty list"
balance [x] = Leafb x
balance xs = Nodeb (balance ls) (balance rs)
  where (ls, rs) = halve xs
  
-- Tautology checker
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving (Show)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k'] 

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = (eval s p) <= (eval s q)
eval s (Equiv p q) = (eval s p) == (eval s q)

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools (n + 1) = map (False:) bss ++ map (True:) bss
                where bss = bools n
                      
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) 
  where vs = rmdups (vars p)
        
isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p]

p1 :: Prop
p1 = And (Var 'A')(Not (Var 'A'))

p2 :: Prop
p2 = Or (Var 'A')(Not (Var 'A'))

p3 :: Prop
p3 = Equiv (Var 'A') (Var 'A')

p4 :: Prop
p4 = Equiv (Var 'A') (Var 'B')

newtype Parser a = P (String -> [(a,String)])
                    
instance Monad Parser where
  return v =  P (\inp -> [(v,inp)])
  p >>= f =  P (\inp -> case parse p inp of
                   [] -> []
                   [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero =  P (\inp -> [])
  p `mplus` q =  P (\inp -> case parse p inp of 
                       [] -> parse q inp
                       [(v,out)] -> [(v,out)])
                 
failure :: Parser a
failure =  mzero

item :: Parser Char
item =  P (\inp -> case inp of
              [] -> []
              (x:xs) -> [(x,xs)])
 
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

-- or else
(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q
       
sat :: (Char->Bool) -> Parser Char
sat p = do x <- item  
           if p x then return x else failure
           
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum 

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
                   
many :: Parser a -> Parser [a]
many p = many1 p +++ return []
         
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             y <- many p
             return (x:y)
             
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)
           
nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs) 
         
space :: Parser ()
space = do many (sat isSpace)
           return ()
           
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = do token ident
                
natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)
             
parseprop :: String -> (Maybe Prop, String)
parseprop xs = case parse proposition xs of
  [(p,[])] -> (Just p, "ok")
  [(_,out)] -> (Nothing, "unused input " ++ out)
  [] -> (Nothing, "invalid input")
  
proposition :: Parser Prop
proposition = conjunction 
              +++ disjunction 
              +++ implication 
              +++ equivalence 
              +++ atom 

conjunction :: Parser Prop
conjunction = do p1 <- atom
                 symbol "^"
                 p2 <- atom
                 return (And p1 p2)

disjunction :: Parser Prop
disjunction = do p1 <- atom
                 symbol "v"
                 p2 <- atom
                 return (Or p1 p2)

implication :: Parser Prop
implication = do p1 <- atom
                 symbol "=>"
                 p2 <- atom
                 return (Imply p1 p2)

equivalence :: Parser Prop
equivalence = do p1 <- atom
                 symbol "<=>"
                 p2 <- atom
                 return (Equiv p1 p2)

atom :: Parser Prop
atom = negation +++ atombase

negation :: Parser Prop
negation = do symbol "!"
              p <- atom
              return (Not p) 

atombase :: Parser Prop
atombase = factor +++ var +++ constant

factor :: Parser Prop
factor = do symbol "("
            p <- proposition
            symbol ")"
            return p

var :: Parser Prop
var = do v <- token upper 
         return (Var v)
         
constant :: Parser Prop
constant = do symbol "True" 
              return (Const True)
           +++ 
           do symbol "False"
              return (Const False)

-- Ex 10.8
tautology :: IO()
tautology = do x <- getLine
               case parseprop x of
                 (Just p, _) -> putStrLn ("isTaut=" ++ show (isTaut p))
                 (Nothing, error) -> putStrLn ("error: " ++ error)
               tautology


-- Abstract machine
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

valueh :: Expr -> Int
valueh (Val n) = n
valueh (Add x y) = valueh x + valueh y
valueh (Mul x y) = valueh x * valueh y

type Cont = [Op]

data Op = EVALADD Expr | ADD Int | EVALMUL Expr | MUL Int 

evalm :: Expr -> Cont -> Int
evalm (Val n) c = exec c n
evalm (Add x y) c = evalm x (EVALADD y : c)
evalm (Mul x y) c = evalm x (EVALMUL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = evalm y (ADD n:c)
exec (ADD n : c) m = exec c (n + m)
exec (EVALMUL y : c) n = evalm y (MUL n:c)
exec (MUL n : c) m = exec c (n * m)

value :: Expr -> Int
value e = evalm e []

