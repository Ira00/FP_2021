
-- Лабораторна робота №5
-- студентки групи КН-31 підгрупа 1
-- Івахненко Ірини

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення- виведення. Набути досвiду компiляцiї Haskell-програм.


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

f2a :: (Integral a) => a -> a -> a
f2a x y =  (x `div` (gcd x y)) * y


 -- 5.2.1. Реалiзувати та скомпiлювати одну з програм, розроблених у лабора- торнiй роботi № 3 для Вашого варiанта з введенням даних: 
 -- а) з клавiатури, б) з файлу та виведенням результатiв: в) на екран, г) у файл.
keyToConsoleF1 :: IO ()
keyToConsoleF1 = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (f1a x)
    putStrLn "Result" 
    print z
-- > keyToConsoleF1
-- Enter massive: 
-- [1,1,1,5,5,3, 1,1,222,222,222,222]
-- Result
-- [1,5,3,1,222]
keyToFileF1 :: IO ()
keyToFileF1 = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (f1a x)
    writeFile "f1_output.txt" ("Result " ++ show z)
-- > keyToFileF1
-- Enter massive: 
-- [1,1,1,5,5,3, 1,1,222,222,222,222]

keyToConsoleF2 :: IO ()
keyToConsoleF2 = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2a x y)
    putStrLn "Result" 
    print z
-- > keyToConsoleF2
-- Enter first number: 
-- 3
-- Enter second number: 
-- 19
-- Result
-- 57

keyToFileF2 :: IO ()
keyToFileF2 = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (f2a x y)
    writeFile "f2_output.txt" ("Result " ++ show z)
-- > keyToFileF2
-- Enter first number: 
-- 3
-- Enter second number: 
-- 19

f :: String -> [Int]
f = read

ff :: String -> Int
ff = read

fff :: Char -> Int
fff = read . pure

fileToFileF1 :: IO ()
fileToFileF1 = do        
        s <- readFile "f1_input.txt"
        let r = f1a (f s)
        writeFile "f1_output.txt" ("Result " ++ show r)
-- > fileToFileF1
fileToConsoleF1 :: IO ()
fileToConsoleF1 = do        
        s <- readFile "f1_input.txt"
        let r = f1a s
        putStrLn "Result" 
        print r
-- > fileToConsoleF1
-- Result
-- "[1,1,1,5,5,3, 1,1,2,2,2,2]"
fileToFileF2 :: IO ()
fileToFileF2 = do        
        s <- readFile "f2_input.txt"
        let numb1 = fff (head s)
        let numb2 = ff (tail s)
        let r = f2a numb1 numb2
        writeFile "f2_output.txt" ("Result " ++ show r)
-- > fileToFileF2
fileToConsoleF2 :: IO ()
fileToConsoleF2 = do        
        s <- readFile "f2_input.txt"
        let numb1 = fff (head s)
        let numb2 = ff (tail s)
        let r = f2a numb1 numb2
        putStrLn "Result" 
        print r
-- > fileToConsoleF2
-- Result
-- 57
-- Висновок: в ході лабораторної роботи ознайомились з модульною органiзацiєю програм та засобами введення- виведення. Набули досвiду компiляцiї Haskell-програм.
