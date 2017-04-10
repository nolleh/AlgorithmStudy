-- pat :: String -> String -> Bool
pat pt str = f
    where
        spret = splitStar pt 0
        f = head spret
        s = last spret    


splitStar str i = splitStarIter str i []
splitStarIter :: String -> Int -> String -> [String]
splitStarIter [] i acc = [acc, ""]
splitStarIter (ch:chs) i acc
    | ch /= '*' && i == 0 = [acc ++ [ch], chs]
    | ch /= '*' = splitStarIter chs (i-1) $ acc ++ [ch]
    | otherwise = splitStarIter chs i $ acc ++ [ch]

main = do 
    print $ splitStar "*p*" 0
    print $ splitStar "p**p" 0
    print $ splitStar "p**pp" 1
    -- print $ pat "*p*" "help"
    -- print $ pat "*p*" "papa"
    -- print $ pat "p**p" "papa"