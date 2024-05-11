module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars

import Data.Char
import Data.List
import Data.Maybe



-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################


dene :: String
dene = "w84,w41,w56,w170,,,,w65,"

dene1 :: String
dene1 = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"

dene2 :: String
dene2 = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,w16,w84,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x ys = case break (==x) ys of
    (ys1, []) -> [ys1]
    (ys1, _:ys2) -> ys1 : splitOn x ys2

sayi :: [String]
sayi = map show ([1..255])

satir :: String -> Bool
satir [] = True
satir (x:xs) = ((x == 'w' || x == 'b') && (xs `elem` sayi)) 



satirkontrol :: String -> Bool
satirkontrol x = let a = splitOn ',' x in 
  all (satir) a


virgulsayisi :: String  -> Bool
virgulsayisi str = if((length $ filter (== ',') str) == 8) then True else False

slashsayisi :: String  -> Bool
slashsayisi str = if((length $ filter (== '/') str) == 8) then True else False


validateFEN :: String -> Bool

validateFEN[] = False
validateFEN x = let a = (splitOn '/' x) in
  if((slashsayisi x) && (all (virgulsayisi) a) && (all(satirkontrol) a) ) then True else False
  







-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################


stringToInt :: Int -> String -> Int
stringToInt base digits
    = sign * (foldl acc 0 $ concatMap digToInt digits1)
      where
      splitSign ('-' : ds) = ((-1), ds)
      splitSign ('+' : ds) = ( 1  , ds)
      splitSign ds         = ( 1  , ds)
      (sign, digits1)      = splitSign digits
      digToInt c
          | c >= '0' && c <= '9'
              = [ord c - ord '0']
          | c >= 'A' && c <= 'Z'
              =  [ord c - ord 'A' + 10]
          | c >= 'a' && c <= 'z'
              =  [ord c - ord 'a' + 10]
          | otherwise
              = []
      acc i1 i0
          = i1 * base + i0


decimalStringToInt :: String -> Int
decimalStringToInt  = stringToInt 10

parcakoy :: String -> Cell
parcakoy [] = Empty
parcakoy (x:xs) = let a = (decimalStringToInt xs) in 
  if x == 'w' 
    then Piece White a
  else 
    Piece Black a

fenabol :: String -> [Cell]
fenabol x = let a = splitOn ',' x in
  map parcakoy a


buildBoard :: String -> Board
buildBoard [] = []
buildBoard x = let a = splitOn '/' x in
  map fenabol a



-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

--data Pos = Pos { col :: Char, row :: Int } deriving Show

sat :: [Int]
sat = [1,2,3,4,5,6,7,8,9]

sut :: [Char]
sut = ['a','b','c','d','e','f','g','h','i']

--carkoy :: Char -> Char -> [Pos]
--carkoy a b = if(a == b) then (Pos a row):[]

tupeltopos :: (Char,Int) -> Pos 
tupeltopos (x,y) = Pos x y   

--line:: Pos -> Pos -> [Pos] 
--line (Pos stch stin) (Pos endch endin) = let a = zip ([stch .. endch]) ([stin .. endin]) in (map (tupeltopos) a)


--line _ _ = []
line :: Pos -> Pos -> [Pos]
line (Pos startc ints) (Pos endc inte) = if(startc == endc) && (ints == inte) then (Pos endc inte):[]
  else if(startc < endc) && (ints == inte) then (Pos startc ints) : line (Pos (chr(ord(startc)+1)) (ints)) (Pos endc inte)  --sağ
  else if(startc > endc) && (ints == inte) then (Pos startc ints) : line (Pos (chr(ord(startc)-1)) (ints)) (Pos endc inte)  --sol
  else if(startc > endc) && (ints > inte) then (Pos startc ints) : line (Pos (chr(ord(startc)-1)) (ints-1)) (Pos endc inte)  --sol aşağı
  else if(startc < endc) && (ints > inte) then (Pos startc ints) : line (Pos (chr(ord(startc)+1)) (ints-1)) (Pos endc inte)  --sağ aşağı
  else if(startc < endc) && (ints < inte) then (Pos startc ints) : line (Pos (chr(ord(startc)+1)) (ints+1)) (Pos endc inte)  --sağ yukarı
  else if(startc > endc) && (ints < inte) then (Pos startc ints) : line (Pos (chr(ord(startc)-1)) (ints+1)) (Pos endc inte)  --sol yukarı
  else if(startc == endc) && (ints > inte) then (Pos startc ints) : line (Pos (startc) (ints-1)) (Pos endc inte)  --aşşağ
  else (Pos startc ints) : line (Pos (startc) (ints+1)) (Pos endc inte)  --yukarı
  
--if(startc == endc) && (ints < inte) then 

