import System.Environment ( getArgs )
-- Лабораторна робота №5
-- студентки групи КН-31 підгрупа 1
-- Івахненко Ірини

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.

-- Варіант 6

-- :load ЛР_5//LR_5.hs

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
    putStrLn "Rezult" 
    print z

keyf1b :: IO ()
keyf1b = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (f1b x)
    putStrLn "Rezult" 
    print z


keyf2a :: IO ()
keyf2a = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2a x y)
    putStrLn "Rezult" 
    print z


keyf2b :: IO ()
keyf2b = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2b x y)
    putStrLn "Rezult" 
    print z
