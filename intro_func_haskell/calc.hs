mul (i,x) = x * i
calc xs = foldl (+) 0 (map mul (zip [0..] xs))
