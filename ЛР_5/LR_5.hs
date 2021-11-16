import System.IO
-- Лабораторна робота №5
-- студентки групи КН-31 підгрупа 1
-- Івахненко Ірини

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.

-- Варіант 6
 -- cd ЛР_5
-- :load LR_5.hs

-- Завдання 1. Послiдовнiсть тотожних елементiв списку замiнити одним елементом, 
-- напр.: [1,1,1,5,5,3, 1,1,222,222,222,222] ⇒ [1,5,3,1,222].

-- а) без застосування
f1a:: Eq a => [a] -> [a] 
f1a (x:xs)
    | xs == [] = [x]
    | x == head (xs) = f1a xs
    | otherwise = x : f1a xs
-- f1a [1,1,1,5,5,3, 1,1,222,222,222,222]

-- result - [1,5,3,1,222]

-- б) з застосуванням вбудованих функцiй вищого порядку.
headmy :: [a] -> a
headmy (a:_) = a
headmy [] = error "0"

f1b:: Eq a => [a] -> [a] 
f1b (x:xs)
    | xs == [] = [x]
    | x == headmy (xs) = f1b xs
    | otherwise = x : f1b xs
-- f1b [1,1,1,5,5,3, 1,1,222,222,222,222]

-- result - [1,5,3,1,222]
-- Завдання 2. Знайти найменше спiльне кратне двох чисел.

-- а) без застосування
f2a :: (Integral a) => a -> a -> a
f2a x y =  (x `div` (gcd x y)) * y
-- f2a 3 19

-- result - 57

-- б) з застосуванням вбудованих функцiй вищого порядку.
gcdmy :: Integral b => (b, b) -> b
gcdmy (x1, 0)  =  x1
gcdmy (0, x2)  =  x2
gcdmy (x1, x2) =  gcdmy (x2, (x1 `mod` x2))

f2b :: (Integral a) => a -> a -> a
f2b x y =  (x `div` (gcdmy (x, y))) * y
-- f2b 3 19

-- result - 57

-- Висновок: в ході лабораторної роботи набули досвiду визначення та використання функцiй вищого порядку.
keyf1a :: IO ()
keyf1a = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (f1a x)
    putStrLn "Result" 
    print z
-- > keyf1a
-- Enter massive: 
-- [1,1,1,5,5,3, 1,1,222,222,222,222]
-- Result
-- [1,5,3,1,222]
keyf1b :: IO ()
keyf1b = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (f1b x)
    putStrLn "Result" 
    print z
-- > keyf1b
-- Enter massive: 
-- [1,1,1,5,5,3, 1,1,222,222,222,222]
-- Result
-- [1,5,3,1,222]
keyf2a :: IO ()
keyf2a = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2a x y)
    putStrLn "Result" 
    print z
-- > keyf2a
-- Enter first number: 
-- 3
-- Enter second number: 
-- 19
-- Result
-- 57

keyf2b :: IO ()
keyf2b = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2b x y)
    putStrLn "Result" 
    print z
-- > keyf2b
-- Enter first number: 
-- 3
-- Enter second number: 
-- 19
-- Result
-- 57
f :: String -> [Int]
f = read

ff :: String -> Int
ff = read

fff :: Char -> Int
fff = read . pure

filef1 :: IO ()
filef1 = do        
        s <- readFile "f1_input.txt"
        let r = f1a (f s)
        writeFile "f1_output.txt" ("Result " ++ show r)
-- > filef1
filef2 :: IO ()
filef2 = do        
        s <- readFile "f2_input.txt"
        let numb1 = fff (head s)
        let numb2 = ff (tail s)
        let r = f2a numb1 numb2
        writeFile "f2_output.txt" ("Result " ++ show r)
-- > filef2