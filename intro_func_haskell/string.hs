makeString :: Char -> Int -> [Char]
makeString c 0 = []
makeString c n = c : makeString c (n - 1)
