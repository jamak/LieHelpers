module SemiSimple(
                  SemiSimple(..),
                  rank,
                  classify,
                  SemiSimple.rootSystem,
                  stdForm
                 ) where

import Data.Ratio

import Root
import RootSystem

import Graph

import Control.Monad

import Data.List(sort)

data SemiSimple = Product [SemiSimple]
                | A Int
                | B Int
                | C Int
                | D Int
                | E6 | E7 | E8
                | F4
                | G2
                  deriving (Show,Eq,Ord)

rank :: SemiSimple -> Int
rank (Product xs) = sum $ map rank xs
rank (A n) = n
rank (B n) = n
rank (C n) = n
rank (D n) = n
rank E6 = 6
rank E7 = 7
rank E8 = 8
rank F4 = 4
rank G2 = 2

zeros :: Int -> [Ratio Int]
zeros n = replicate n 0

minrep :: SemiSimple -> Int
minrep (Product xs) = sum $ map minrep xs
minrep (A n) = n + 1
minrep (D 1) = 2
minrep E6 = 8
minrep E7 = 8
minrep G2 = 3
minrep other = rank other

rootSystem' :: SemiSimple -> [[Ratio Int]]
rootSystem' (Product xs) = rootSystem'' [] xs
    where rootSystem'' :: [[Ratio Int]] -> [SemiSimple] -> [[Ratio Int]]
          rootSystem'' [] [] = []
          rootSystem'' [] (ss:sss) = rootSystem'' (rootSystem' ss) sss
          rootSystem'' (r:rest) left = padded:rootSystem'' rest left
              where pad = minrep (Product left)
                    rep = length r
                    pleft = zeros (total - pad - rep)
                    pright = zeros pad
                    padded = pleft++r++pright
          total = minrep (Product xs)
rootSystem' (A n) = [zeros i++[1,-1]++zeros (n-(i+1)) | i<-[0..(n-1)]]
rootSystem' (B n) = (1:zeros (n-1)):rootSystem' (A (n-1))
rootSystem' (C n) = (2:zeros (n-1)):rootSystem' (A (n-1))
rootSystem' (D n) = (1:1:zeros (n-2)):rootSystem' (A (n-1))
rootSystem' G2 = [[0,1,-1],[1,-2,1]]
rootSystem' F4 = [[1%2,-1%2,-1%2,-1%2],[0,1,0,0],[0,1,-1,0],[0,0,1,-1]]
rootSystem' E6 = [[1%2,1%2,-1%2,-1%2,-1%2,-1%2,-1%2,-1%2],
                  [0,1,1,0,0,0,0,0],
                  [0,1,-1,0,0,0,0,0],
                  [0,0,1,-1,0,0,0,0],
                  [0,0,0,1,-1,0,0,0],
                  [1%2,-1%2,-1%2,-1%2,-1%2,1%2,1%2,1%2]]
rootSystem' E7 = [[1%2,1%2,-1%2,-1%2,-1%2,-1%2,-1%2,-1%2],
                  [0,1,1,0,0,0,0,0],
                  [0,1,-1,0,0,0,0,0],
                  [0,0,1,-1,0,0,0,0],
                  [0,0,0,1,-1,0,0,0],
                  [0,0,0,0,1,-1,0,0],
                  [0,0,0,0,0,0,1,1]]
rootSystem' E8 = [[1%2,1%2,-1%2,-1%2,-1%2,-1%2,-1%2,-1%2],
                  [0,1,1,0,0,0,0,0],
                  [0,1,-1,0,0,0,0,0],
                  [0,0,1,-1,0,0,0,0],
                  [0,0,0,1,-1,0,0,0],
                  [0,0,0,0,1,-1,0,0],
                  [0,0,0,0,0,1,-1,0],
                  [0,0,0,0,0,0,1,-1]]

stdForm' :: [SemiSimple] -> [SemiSimple]
stdForm' (Product [x]:xs) = stdForm' [x] ++ stdForm' xs
stdForm' (Product []:xs) = stdForm' xs
stdForm' (Product y:xs) = stdForm' y ++ stdForm' xs
stdForm' (B 1:xs) = A 1:stdForm' xs
stdForm' (C 1:xs) = A 1:stdForm' xs
stdForm' (D 1:xs) = A 1:stdForm' xs
stdForm' (D 2:xs) = A 1:A 1:stdForm' xs
stdForm' (C 2:xs) = B 2:stdForm' xs
stdForm' (A 3:xs) = D 3:stdForm' xs
stdForm' (x:xs) = x:stdForm' xs
stdForm' [] = []

box [s] = s
box s = Product s

stdForm :: SemiSimple -> SemiSimple
stdForm (Product s) = box $ sort $ stdForm' s
stdForm s = box $ stdForm' [s]


rootSystem :: SemiSimple -> RootSystem
rootSystem s = generate $ map root $ rootSystem' s

edges :: [Root] -> [(Root,Root)]
edges rs = filter (\(r1,r2)->0/=dot r1 r2) es
    where es = filter (uncurry (<)) [(r1,r2)|r1<-rs, r2<-rs]
