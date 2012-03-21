import Data.List

--最長重複文字列を計算する
maxDupStr :: [Char] -> [Char]
maxDupStr = extract . chooseMax . calcLen . makePair . sort . tails

--隣り合う要素を組みにする
makePair :: [[Char]] -> [([Char], [Char])]
makePair xs = zip xs (tail xs)

--文字列の共通部分の長さを求める
calcLen :: [([Char], [Char])] -> [(Int, [Char])]
calcLen = map lenstr

lenstr :: ([Char], [Char]) -> (Int, [Char])
lenstr (xs, ys) = (comlen xs ys, xs)

comlen :: [Char] -> [Char] -> Int
comlen xs ys = length (takeWhile pairEq (zip xs ys))

pairEq :: (Char, Char) -> Bool
pairEq (x, y) = x == y

--共通部分が一番長い要素を取り出す
chooseMax :: [(Int, [Char])] -> (Int, [Char])
chooseMax = maximumBy compFst

compFst :: (Int, [Char]) -> (Int, [Char]) -> Ordering
compFst (n1, s1) (n2, s2) = compare n1 n2

--共通部分のみ残す
extract :: (Int, [Char]) -> [Char]
extract (i, xs) = take i xs
