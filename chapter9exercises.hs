import Data.Char
import Control.Monad
import System.IO 
 
mgetLine :: IO String
mgetLine = do x <- getChar
              if x == '\n' then 
                return [] 
                else
                do xs <- mgetLine 
                   return (x:xs)
                  
mputStr :: String -> IO ()
mputStr [] = return ()
mputStr (x:xs) = do putChar x
                    mputStr xs

mputStrLn :: String -> IO ()
mputStrLn xs = do mputStr xs
                  putChar '\n'
--putStrLn :: String -> IO ()
--putStrLn xs = do putStr xs
--                 putChar '\n'

strlen :: IO()
strlen = do mputStrLn "Enter string: "
            xs <- mgetLine
            mputStrLn ("length=" ++ show(length xs))


beep :: IO()
beep = putStr "\BEL"

cls :: IO()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto::Pos->IO()
goto (x,y) = 
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos->String->IO()
writeat p xs = do goto p 
                  putStr xs

seqn :: [IO a] -> IO()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs

mputStr2 xs = seqn [putChar x | x <- xs]

-- parser for calculator
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

eval :: String -> Int
eval xs = case parse expr xs of
  [(n,[])] -> n
  [(_,out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ do symbol "-"
                   e <- expr
                   return (t - e)
            +++ return t

term :: Parser Int
term = do p <- power
          do symbol "*"
             t <- term
             return (p * t)
            +++ do symbol "/"
                   t <- term
                   return (p `div` t)
            +++ return p
            
power :: Parser Int
power = do f <- factor
           do symbol "^"
              p <- power
              return (f ^ p)
             +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

-- calculator
getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]
       
buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO()
showbox = seqn [writeat (1,y) xs | (y,xs) <- zip [1..13] box]

display :: String->IO()
display xs = do writeat (3,2) "             "
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String->IO()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
               else 
               do beep
                  calc xs

process :: Char->String->IO()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = evalexpr xs
  | elem c "cC" = clear
  | otherwise = press c xs
                      
quit :: IO()
quit = goto (1,14)

delete :: String -> IO()
delete "" = calc ""
delete xs = calc (init xs)

-- Ex 9.2 as well (not a very good solution)
evalexpr :: String->IO()
evalexpr xs = case parse expr xs of
  [(n,"")]->calc(show n)
  [(_, rs)]-> do beep
                 calc (reverse (drop (length rs) (reverse xs))) 
         
clear :: IO()
clear = calc ""

press :: Char->String->IO()
press c xs = calc (xs ++ [c])

run :: IO()
run = do cls
         showbox
         clear
         
-- Game of life
width :: Int
width = 80
height :: Int
height = 25
type Board = [Pos]
glider :: [Pos]
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]
glider2 = glider ++ [(10+x,25-y)| (x,y)<-glider]
showcells b = seqn [writeat p "o" | p <- b]
isAlive b p = elem p b
isEmpty b p = not (isAlive b p)
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)
liveneighbs b = length . filter (isAlive b) . neighbs
survivors b = [p | p<-b, elem (liveneighbs b p) [2,3]]
births b = [p | p<-rmdups (concat (map neighbs b)),
          isEmpty b p,
          liveneighbs b p == 3]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)
nextgen b = survivors b ++ births b
life :: Board -> IO()
life b = do cls
            showcells b
            wait 15000
            life (nextgen b)
wait n = seqn [return () | _<-[1..n]]

lifenoflicker :: Board -> IO()
lifenoflicker b = do cls
                     lifenoflickerloop b []

lifenoflickerloop :: Board -> Board -> IO()
lifenoflickerloop b oldb = 
  do showchanges b oldb
     wait 15000
     lifenoflickerloop (nextgen b) b

showchanges :: Board -> Board -> IO()
showchanges b oldb = do seqn [writeat p " " | p<-oldb, not (elem p b) ]
                        seqn [writeat p "o" | p<-b, not (elem p oldb) ]

-- Ex 9.1 (in the end I copied from the answer, as I can't figure out
-- if the terminal is doing what is expected by the book)
-- Actually the following doesn't work either :( maybe the \DEL character 
-- is wrong?
readLine = get ""

get xs = do x <- getChar
            case x of
              '\n' -> return xs
              '\DEL' -> if null xs then
                          get xs
                        else
                          do putStr "\ESC[1D \ESC[1D"
                             get (init xs)
              _ -> get (xs ++ [x])

-- Ex 9.6 Nim
type NimBoard = [Int]

nimboardstart :: NimBoard
nimboardstart = [5,4,3,2,1]

nim = nimmove nimboardstart 1                        

nimmove :: NimBoard -> Int -> IO()
nimmove board player 
  | gamecomplete board = displaywinner (otherplayer player)
  | otherwise =
    do displaynim board                              
       displayPlayer player
       row <- getNimRow             
       n <- getNimNumberToRemove
       newboard <- return (newb board row n)
       if nimboardislegal newboard then
         nimmove newboard (otherplayer player)
         else
         do putStrLn "Illegal move"
            nimmove board player 

otherplayer 1 = 2
otherplayer 2 = 1

-- gamecomplete [0,0,0,0,0] = True
-- gamecomplete _ = False
gamecomplete b = all (== 0) b

displaywinner player = putStr ("Winner is player " ++ (show player) ++ "!!")

displaynim :: NimBoard -> IO()
displaynim [a,b,c,d,e] = do displaynimrow 1 a
                            displaynimrow 2 b
                            displaynimrow 3 c
                            displaynimrow 4 d
                            displaynimrow 5 e
                    
displaynimrow row n = putStrLn ((show row) ++ ": " ++ stars n)

stars n = replicate n '*'              
                           
displayPlayer player = putStrLn ("Player " ++ (show player) ++ ": ")

getNimRow = do x <- askForRow
               if (x < 1) || (x > 5) then
                 do putStrLn "Illegal row"
                    getNimRow
                 else
                 return x

askForRow :: IO Int
askForRow = do putStrLn "Enter row: "
               x <- getLine
               return (read x)

getNimNumberToRemove :: IO Int
getNimNumberToRemove = do putStrLn "Enter number to remove: "
                          x <- getLine
                          return (read x)

newb :: NimBoard -> Int -> Int -> NimBoard                
newb b row delta = [n - changefor boardrow row delta  | (boardrow, n) <- zip [1..5] b]
             
changefor boardrow row delta | row == boardrow = delta  
                             | otherwise = 0 

nimboardislegal b = all islegalnimrowcount b

islegalnimrowcount count = (count >= 0) && (count <= 5)
                                       
foo = putStrLn "blah"
      putStrLn "bloo"

