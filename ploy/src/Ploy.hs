module Ploy where  -- do NOT CHANGE export of module

import Board
import Data.Char
import Data.List
import Data.Maybe
import Data.Bits

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

unitcW :: Cell -> Bool -- Siyah kazanmışsa True verir tüm listeye uygularsan
unitcW (Empty) = True
unitcW (Piece a b) = 
  if(a == White && b == 85) 
    then True 
  else if (a == White && b == 170)
    then True
  else False


beyazkon :: [Cell] -> [Bool] --beyaz kaybettiyse full true verir
beyazkon [] = []
beyazkon ((Empty):xs) = (True): beyazkon xs
beyazkon ((Piece a b):xs) = 
  if(a == White && b == 85) 
    then (True): beyazkon xs 
  else if (a == White && b == 170)
    then (True) : beyazkon xs
  else if(a == Black)
    then (True): beyazkon xs
  else (False) : beyazkon xs
  

siyahkon :: [Cell] -> [Bool]
siyahkon [] = []
siyahkon ((Empty):xs) = (True): siyahkon xs
siyahkon ((Piece a b):xs) = 
  if(a == Black && b == 85) 
    then (True): siyahkon xs 
  else if (a == Black && b == 170)
    then (True) : siyahkon xs
  else if(a == White)
    then (True) : siyahkon xs
  else (False) : siyahkon xs
  


gameFinished :: Board -> Bool
gameFinished [] = False
gameFinished x = let a = map siyahkon x in
  let b = map and a in
    let c = map beyazkon x in
      let d = map and c in
        if(and(b) == True || and(d) == True)
          then True
        else False

--gameFinished _ = False



-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################



--Board = [[Cell]]
--data Cell = Piece Player Int | Empty deriving Show
--data Move = Move {start :: Pos, target :: Pos, turn :: Int}
--data Pos = Pos { col :: Char, row :: Int } deriving Show

--sut :: [Char]
--sut = ['a','b','c','d','e','f','g','h','i']




whatpiece :: Move -> Board -> Cell --hangi parça hamleyi yapıyor
whatpiece (Move (Pos stch sti) (Pos ench eni) tr) x = let a = fromJust $ elemIndex stch sut in
  let b = x!!(8-(sti-1)) in
    let c = b!!a in
      c

--whopiece :: Move -> Board -> Cell -- gidilen yerde ne var
--whopiece (Move (Pos stch sti) (Pos ench eni) tr) x = let a = fromJust $ elemIndex ench sut in
--  let b = x!!(8-(eni-1)) in
--    let c = b!!a in
--      c




yonune :: Move -> Int
yonune (Move (Pos startc ints) (Pos endc inte) tr) =
  if((startc == endc) && (ints < inte)) --North
    then 0
  else if((startc == endc) && (ints > inte)) --south
    then 4
  else if((startc < endc) && (ints == inte)) --east
    then 2
  else if((startc > endc) && (ints == inte)) --west
    then 6
  else if((startc < endc) && (ints < inte)) --northeast
    then 1
  else if((startc < endc) && (ints > inte)) --southeast
    then 3
  else if((startc > endc) && (ints > inte)) --southwest
    then 5
  else 7

posdir :: Cell -> Int --posdir ile verilen cell in int ini alabilirim
posdir (Piece _ x) = x


nevar :: Pos -> Board -> Cell
nevar (Pos k s) x = let a = fromJust $ elemIndex k sut in
  let b = x!!(8-(s-1)) in
    let c = b!!a in
      c

yoldanevar :: [Pos] -> Board -> [Cell]
yoldanevar [] a = []
yoldanevar (x:xs) a = (nevar x a): yoldanevar xs a

tasrengi :: Cell -> Player
tasrengi (Piece White _) = White
tasrengi (Piece Black _) = Black

ginit :: [a] -> [a]
ginit [] = []
ginit x = init x


gitmehakki :: Int -> Int --gitme hakkı ile gidebileceği yerlere bakabilirim
gitmehakki x = 
  if(popCount x == 1)
    then 1
  else if(popCount x == 2)
    then 2
  else if(popCount x == 3)
    then 3
  else if(popCount x == 4)
    then 1
  else 0

--b9 dan b8 e

gidiyomu :: Board -> Move -> Bool
gidiyomu x (Move (Pos stch sti) (Pos ench eni) tr) = let a = nevar (Pos stch sti) x in --a = hamleyiyapan taş
  let b = nevar (Pos ench eni) x in -- gidilen yerde ne var
    let c = posdir a in --c = hamleyi yapan taşın int i
      let d = tasrengi a in -- d = hamleyi yapan taşın rengi
        let e = tasrengi b in -- e = gidilen yerdeki taşın rengi
          let f = line (Pos stch sti) (Pos ench eni) in -- f = [Pos] geçilen pozisyonlar
            let g = tail (yoldanevar f x) in -- g = yolumuzdaki celler
              let h = gitmehakki c in -- hamleyi yapan taşın gitme hakkı
                let i = length g in --yolumuzdaki cellerin uzunluğu
                  let j = init g in -- son eleman hariç yolumuz
                    if(h >= i && all (== Empty) j && d /= e && b /= Empty) --hedefte düşman taşı olan durum
                      then True
                    else if(h >= i && all (== Empty) g) --hedefin boş olduğu durum
                      then True
                    else False
                
gidiyomu2 :: Board -> Move -> Bool
gidiyomu2 x (Move (Pos stch sti) (Pos ench eni) tr) = let a = nevar (Pos stch sti) x in --a = hamleyiyapan taş
  let c = posdir a in --c = hamleyi yapan taşın int i
    let d = tasrengi a in -- d = hamleyi yapan taşın rengi
      let f = line (Pos stch sti) (Pos ench eni) in
        let k = (yoldanevar f x) in -- k = oynayan taşla beraber yoldaki celler
          let g = tail (yoldanevar f x) in -- g = yolumuzdaki celler
            let h = gitmehakki c in -- hamleyi yapan taşın gitme hakkı
              let i = length g in --yolumuzdaki cellerin uzunluğu
                let j = ginit g in -- son eleman hariç yolumuz
                  let b = nevar (Pos ench eni) x in -- gidilen yerde ne var
                    if(b /= Empty)
                      then let e = tasrengi b in -- e = gidilen yerdeki taşın rengi
                        if(h >= i && ((all (==Empty) j) || j == []) && d /= e && tr == 0 && tr <= 7) --gidilen yerde taş olduğu durumda
                          then True
                        else if(h >= i && ((all (==Empty) j) || j == []) && d /= e && (popCount c) == 1 && tr >= 0 && tr <= 7) --taş shieldsa hem dönüp hem de gidebilir
                          then True
                        else False
                    else 
                      if(h >= i && all (==Empty) g && tr == 0) --gidilen yerde taş yoksa
                        then True
                      else if(h >= i && all(==Empty) g && (popCount c) == 1 && tr >= 0 && tr <= 7) --taş shieldsa hem dönüp hem de gidebilir
                        then True
                      else False

        

cizgimi :: Move -> Bool --move çizgi üzerinde mi
cizgimi (Move (Pos stch sti) (Pos ench eni) tr) = 
  if(stch == ench)
    then True
  else if(sti == eni)
    then True
  else if((max(sti)(eni)) - (min(sti)(eni))) == ((max(ord(stch)) (ord(ench))) - (min(ord(stch)) (ord(ench))))
    then True
  else False


isValidMove :: Board -> Move -> Bool
isValidMove x (Move (Pos stch sti) (Pos ench eni) tr) = let a = nevar (Pos stch sti) x in --a = hamleyi yapan taş ne
  let b = posdir a in  -- b = hamleyi yapan taşın int i
    let c = yonune (Move (Pos stch sti) (Pos ench eni) tr) in -- c = yapılan hamlenin yönü ne --testBit b c
      let d = gidiyomu2 x (Move (Pos stch sti) (Pos ench eni) tr) in
        if(d == True && testBit b c == True && cizgimi (Move (Pos stch sti) (Pos ench eni) tr) == True) --yön ve kalanlar
          then True
        else if(d == True && testBit b c == True && popCount b == 1 && tr >= 0 && tr <= 7 && cizgimi (Move (Pos stch sti) (Pos ench eni) tr) == True) --shield olduğu takdirde dönebilir de
          then True
        else if(stch == ench && sti == eni && 7 >= tr && tr > 0 && cizgimi (Move (Pos stch sti) (Pos ench eni) tr) == True) -- ilerlemeyip sadece dönerlerse / gidiyomu ya bakmana gerek yok burda hallediyo
          then True
        else False




--isValidMove _ _ = False



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

--nevar :: Pos -> Board -> Cell
--nevar (Pos k s) x = let a = fromJust $ elemIndex k sut in
--  let b = x!!(8-(s-1)) in
--    let c = b!!a in
--      c

bos1 :: String
bos1 = ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"


bosboard :: Pos -> Cell -> Board --sadece elemanın olduğu boş bir board
bosboard (Pos ch inte) c = let a = fromJust $ elemIndex ch sut in
  let b = buildBoard bos1 in
    let d = b!!(8-(inte-1)) in
      let e = replace d a c in
        replace b (8-(inte-1)) e


replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

--posdir ile verilen cell in int ini alabilirim
--gitme hakkı ile gidebileceği yerlere bakabilirim
--erişebileceği yerler listesi yapabilirim [Pos] şeklinde --tümpostan 
-- tahta üzerindeki tüm pozisyonlar listesi yapmam lazım
-- şöyle yaparım verilen pozisyonu ve boş adamın yerleştiği boardumu giriş olarak  erişebileceği pos lar listemden
--tek tek bir move oluşturup isvalidmove fonksiyonunu tek tek 0 dan yediye kadar denetip true döndürdüğü takdirde listeye eklerim
--erişebileceğim postlar yapmama da gerek yok tümposlardan bakabilirim
--veya direkt tüm move lar listesi oluştururum olmayanı isValidmove la elerim
--iki pozisyondan move oluşturabilmmem lazım önce


tumpos :: [Pos] --tahtada gidilebilecek tüm poslar
tumpos = let a = line (Pos 'a' 9) (Pos 'i' 9) in
  let b = line (Pos 'a' 8) (Pos 'i' 8) in
    let c = line (Pos 'a' 7) (Pos 'i' 7) in
      let d = line (Pos 'a' 6) (Pos 'i' 6) in
        let e = line (Pos 'a' 5) (Pos 'i' 5) in
          let f = line (Pos 'a' 4) (Pos 'i' 4) in
            let g = line (Pos 'a' 3) (Pos 'i' 3) in
              let h = line (Pos 'a' 2) (Pos 'i' 2) in
                let i = line (Pos 'a' 1) (Pos 'i' 1) in
                  a ++ b ++ c ++ d ++ e ++ f ++ g ++ h ++ i

                
    

tummove :: Pos -> [Pos] -> [Move] --yapılabilecek tüm hamlelerin olduğu bi liste bunu tumposla çağırman lazım
tummove x [] = []
tummove (Pos stch sti) (x:xs) = 
  (Move (Pos stch sti) x 0):(Move (Pos stch sti) x 1):(Move (Pos stch sti) x 2):(Move (Pos stch sti) x 3):(Move (Pos stch sti) x 4):(Move (Pos stch sti) x 5):(Move (Pos stch sti) x 6):(Move (Pos stch sti) x 7):[] ++ tummove (Pos stch sti) xs


movelar :: [Move] -> Cell -> [Move]
movelar [] _ = []
movelar ((Move (Pos ch ints) (Pos ech inte) tr):xs) c = 
  let a = bosboard (Pos ch ints) c in
    if(isValidMove a (Move (Pos ch ints) (Pos ech inte) tr) == True)
      then (Move (Pos ch ints) (Pos ech inte) tr):[] ++ movelar xs c
    else movelar xs c






possibleMoves :: Pos -> Cell -> [Move]
possibleMoves x y = 
  let a = tumpos in
    let b = bosboard x y in
      let c = tummove x a in
        movelar c y
--possibleMoves _ _ = []



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

-- Boarddan seçilen playerın oyuncularının olduğu bi [pos] listesi almam lazım
--nevar a pos ve board verdiğim zaman bana cell de ne olduğunu söylüyo
--seçilen taşların yapmaya çalışacağı movelar listesi yapmam lazım

moveattempt :: [Pos] -> Cell -> [Move] --yapılabilecek move hamlelerinin hepsini koyuyo poselemeden gelen listeyle çağırılmalı
moveattempt [] _ = []
moveattempt (x:xs) y = let a = bosboard x y in
  let b = nevar x a in
    possibleMoves x b ++ moveattempt xs y

moveattempt2 :: [Pos] -> Board -> [Move] --yapılabilecek move hamlelerinin hepsini koyuyo poselemeden gelen listeyle çağırılmalı
moveattempt2 [] _ = []
moveattempt2 (x:xs) y = let a = nevar x y in
  possibleMoves x a ++ moveattempt2 xs y

poseleme :: [Pos] -> Board -> Player -> [Pos] --seçilen rengin taşlarının olduğu posları gösteriyo tumpos ile çağırılmalı
poseleme [] _ _ = []
poseleme (x:xs) y renk = let a = nevar x y in
  if(a /= Empty)
    then let b = tasrengi a in
    if(renk == b)
      then x:[] ++ poseleme xs y renk
    else poseleme xs y renk
  else poseleme xs y renk

moveeleme :: [Move] -> Board -> [Move] --moveattemptten gelen move listesiyle çalışacak
moveeleme [] _ = []
moveeleme (x:xs) y =
  if(isValidMove y x == True)
    then x:[] ++ moveeleme xs y
  else moveeleme xs y 



listMoves :: Board -> Player -> [Move]
listMoves x renk = let a = tumpos in
  let b = poseleme a x renk in
    let c = moveattempt2 b x in
      moveeleme c x
--listMoves _ _ = []