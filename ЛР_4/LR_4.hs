-- Лабораторна робота №4
-- студентки групи КН-31 підгрупа 1
-- Івахненко Ірини

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення нових типiв та класiв типiв i їх використання.


-- Варіант 6

-- :load ЛР_4//LR_4.hs

-- Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути 
-- книгою (автор/спiвавтори, назва, мiсто, видавництво, рiк), 
-- статтею (автор/спiвавтори, назва статтi, назва журналу, рiк, номер журналу, сторiнки) 
-- або тезами доповiдi (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Визначте функцiї для :
-- 6. визначення чи є публiкацiя з певною назвою книгою, статтею або тезами;

data Book = Book String String String String Int deriving (Eq, Show)
author1 :: Book -> String
author1 (Book author _ _ _ _)=author
name1 :: Book ->String
name1 (Book _ name _ _ _)=name
country1 :: Book -> String
country1 (Book _ _ country _ _)=country
publication1 :: Book -> String
publication1 (Book _ _ _ publication _)=publication
year1 :: Book -> Int
year1 (Book _ _ _ _ year)=year

book1 :: [Book]
book1 = [(Book "a" "First" "a" "a" 2012), (Book "b" "Second" "b" "b" 2012)]


elem' :: String -> [Book] -> Bool
elem' _ [] = False
elem' x (y : ys) = (x == name1 y) || elem' x ys

data Article = Article String String String Int Int Int deriving (Eq, Show)
author2 :: Article ->String
author2 (Article author _ _ _ _ _)=author
name2 :: Article -> String
name2 (Article _ name _ _ _ _)=name
nameJournal :: Article -> String
nameJournal (Article _ _ nameJ _ _ _) = nameJ
yearArt :: Article -> Int
yearArt (Article _ _ _ year _ _) = year
number :: Article -> Int
number (Article _ _ _ _ num _)=num
pages :: Article -> Int
pages (Article _ _ _ _ _ p)=p
article1 :: [Article]
article1 = [(Article "q" "Third" "q" 2010 34 60), (Article "s" "Forth" "s" 2012 12 610)]

elem1' :: String -> [Article] -> Bool
elem1' _ [] = False
elem1' x (y : ys) = (x == name2 y) || elem1' x ys

data Thesis = Thesis String String String String Int Int deriving (Eq, Show)

author3 :: Thesis -> String
author3 (Thesis author _ _ _ _ _)=author
name3 :: Thesis -> String
name3 (Thesis _ name _ _ _ _)=name
nameConf :: Thesis -> String
nameConf (Thesis _ _ n _ _ _) = n
countryThes :: Thesis -> String
countryThes (Thesis _ _ _ country _ _) = country
yearThes :: Thesis -> Int
yearThes (Thesis _ _ _ _ y _) = y
pagesThes :: Thesis -> Int
pagesThes (Thesis _ _ _ _ _ p)=p

thesis1 :: [Thesis]
thesis1 = [(Thesis "w" "Fifth" "w" "w" 2009 90), (Thesis "c" "Sixth" "c" "c" 2001 210), ((Thesis "l" "Seventh" "l" "l" 2011 90))]

elem2' :: String -> [Thesis] -> Bool
elem2' _ [] = False
elem2' x (y : ys) = (x == name3 y) || elem2' x ys


check :: String -> [Char]
check a
  | elem' a book1 = "Book"
  | elem1' a article1 = "Article"
  | elem2' a thesis1 = "Thesis"
  | otherwise = "Invalid name"

-- check "First"
-- result - "Book"

-- check "Sixth"
-- result - "Thesis"

-- check "Forth"
-- result - "Article"

-- check "A"
-- result - "Invalid name"
-- 93. статистика по базi за типом публiкацiй.

statistic :: [([Char], Int)]
statistic = [("Book", length book1), ("Article", length article1), ("Thesis", length thesis1)]

-- statistic
-- result - [("Book",2),("Article",2),("Thesis",3)]
-- Висновок: в ході лабораторної роботи ознайомились з системою типiв та класiв типiв. Набули досвiду визначення нових типiв та класiв типiв i їх використання.

