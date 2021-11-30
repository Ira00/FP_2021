{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Control.Applicative
import Data.Text (Text)

data MFormExample = MFormExample

mkYesod "MFormExample" [parseRoutes|
/ RootR GET
|]

instance Yesod MFormExample

instance RenderMessage MFormExample FormMessage where
  renderMessage _ _ = defaultFormMessage

f2a :: (Integral a) => a -> a -> a
f2a x y =  (x `div` (gcd x y)) * y


function1Form :: Html -> MForm Handler (FormResult Int, Widget)
function1Form extra = do
    (firstNumRes, firstNumView) <- mreq intField "this is not used" Nothing
    (secondNumRes, secondNumView) <- mreq intField "neither is this" Nothing
    let result = f2a <$> firstNumRes <*> secondNumRes
    
    let widget = do
            toWidget
                [lucius|
                    ##{fvId firstNumView} {
                        width: 3em;
                    }
                    ##{fvId secondNumView} {
                        width: 3em;
                    }
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
                    First number is #
                    ^{fvInput firstNumView}
                    \ and second number is #
                    ^{fvInput secondNumView}
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
                <p> Знайти найменше спiльне кратне двох чисел.

            <p class="res">Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
        |]

main :: IO ()
main = warp 3000 MFormExample