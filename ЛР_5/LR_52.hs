{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Лабораторна робота №5_2
-- студентки групи КН-31 підгрупа 1
-- Івахненко Ірини
import Yesod
import Control.Applicative
import Data.Text (Text, unpack)

data MFormExample = MFormExample

mkYesod "MFormExample" [parseRoutes|
/ RootR GET
|]

instance Yesod MFormExample

instance RenderMessage MFormExample FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Завдання 2. Знайти найменше спiльне кратне двох чисел.

f1a :: Eq a => [a] -> [a]
f1a (x:xs)
    | xs == [] = [x]
    | x == head (xs) = f1a xs
    | otherwise = x : f1a xs

f :: String -> [Int]
f = read

function1Form :: Html -> MForm Handler (FormResult [Int], Widget)
function1Form extra = do
    (firstNumRes, firstNumView) <- mreq textField "this is not used" Nothing
    let num = unpack <$> firstNumRes
    let z = f <$> num 
    let result = f1a <$> z
    
    let widget = do
            toWidget
                [lucius|
                    p{
                        font-size: 16px;
                    }
                    h1{
                        text-align: center;
                    }
                    h2{
                        text-align: center;
                    }
                    header, .task{
                        background: #fff;
                    }
                    .task{
                        padding: 20px;
                    }
                    body{
                        background-color: rgba(148, 211, 253, 0.7);
                    }
                    .res{
                        border: 1px solid black;
                        padding: 10px;
                        text-align: center;
                        background: rgba(217, 222, 226, 0.7);
                        font-weight: bold;
                    }
                    .btn{
                        background-color: #008CBA;
                        border: none;
                        color: white;
                        padding: 10px;
                        font-size: 14px;
                        transition: all 0.3s ease-out;
                    }
                    .btn:hover{
                        background: #27a8d3;
                    }
                |]

            [whamlet|
                #{extra}
                <p class="task">
                    Enter massive #
                    ^{fvInput firstNumView}
                    \. #
                    <input type=submit value="Result" class="btn">
            |]
    return (result, widget)

getRootR :: Handler Html
getRootR = do
    ((res, widget), enctype) <- runFormGet function1Form   
    defaultLayout
        [whamlet|
            <header>
                <h1> Лабораторна робота веб-застосунок засобами фреймворка Yesod   
                <h2> Виконала студентка групи КН-31 Івахненко І. І.
            <div class="task">
                <h2> Завдання на лабораторну роботу №1
                <p> Послiдовнiсть тотожних елементiв списку замiнити одним елементом, напр.: [1,1,1,5,5,3, 1,1,222,222,222,222] ⇒ [1,5,3,1,222].

            <p class="res">Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
        |]

main :: IO ()
main = warp 3000 MFormExample