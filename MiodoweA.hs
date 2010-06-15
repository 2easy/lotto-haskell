module Miodowe (Komorka, Plansza, Generator, Zadanie(..),
                naPlanszy, plansza, rozwiaz) where

import Monad
import List ((\\), delete)

type Generator a = [a]

type Komorka = (Int,Int)
type Plansza = [Komorka]
data Zadanie = Zadanie {
        rozmiar :: Int,
        liczbaWysp :: Int,
        wielkoscWysp :: Int,
        czarne :: Plansza
     }

naPlanszy :: Int -> Komorka -> Bool
naPlanszy rozmiar (x,y) =
   abs y <= rozmiar && abs x + abs y <= 2 * rozmiar

sasiad :: Komorka -> Generator Komorka
sasiad (x,y) = [ (x+dx,y+dy) | dy <- [-1..1],
                               let adx = 2 - abs dy,
                               dx <- [-adx, adx] ]

plansza :: Int -> Plansza
plansza rozmiar = [(x,y) | x <- [-2 * rozmiar .. 2 * rozmiar],
                           y <- [-rozmiar .. rozmiar],
                           (x `mod` 2 == 0) == (y `mod` 2 == 0),
                           naPlanszy rozmiar (x,y)]

rozwiaz :: Zadanie -> Generator Plansza
rozwiaz zadanie = dfs wolne (czarne zadanie) (liczbaWysp zadanie) where
   wolne = plansza (rozmiar zadanie) \\ czarne zadanie
   dfs wolne czrn 0 = return$ wolne ++ czrn
   dfs [] _ _ = mzero
   dfs wolne@(pole:wolne') czrn lwysp =
      do (wolne'', czrn', wlk) <-
            wyspa (wolne, czrn, wielkoscWysp zadanie) pole
         guard$ wlk == 0
         dfs wolne'' czrn' (lwysp-1)
      `mplus` dfs wolne' (pole:czrn) lwysp
   wyspa stan@(wolne, czrn, wlk) pole
      | pole `elem` wolne =
           let wolne' = delete pole wolne in
              foldM wyspa (wolne', czrn, wlk-1) (sasiad pole)
              `mplus` return (wolne', pole:czrn, wlk)
      | otherwise = return stan
