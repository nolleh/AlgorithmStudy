count :: [(Int, Int)] -> Int -> [Int] -> Int

count [] size picks
  | length picks == size = 1
  | otherwise = 0

count (x:xs) size picks = (count pick_rest size ((fst x):(snd x):picks)) + 
                          (count unpick_rest size picks)
  where pick_rest = [pair | pair <- xs, (fst pair) /= (fst x), (fst pair) /= (snd x), 
                                        (snd pair) /= (fst x), (snd pair) /= (snd x)]
        unpick_rest = xs


main = do 
  print $ count [(0,1)] 2 []
  print $ count [(0,1), (1,2), (2,3), (3,0), (0,2), (1,3)] 4 []
  print $ count [(0,1), (0,2), (1,2), (1,3), (1,4), (2,3), (2,4), (3,4), (3,5), (4,5)] 6 []

-- 4 6
-- 0 1 / 1 2 / 2 3 / 3 0 / 0 2 / 1 3
-- (0 1) (2 3)
-- (1 2) (3 0)
-- (0 2) (1 3)

