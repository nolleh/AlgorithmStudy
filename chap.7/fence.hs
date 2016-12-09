import Data.List (maximumBy)
import Data.Ord (comparing)

-- [(지금까지의 사각형 크기, 높이, 지속여부)]
fenceAdd :: Int -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
fenceAdd l acc = (l, l, True) : map (\(size, height, cont) -> 
    if cont && (l >= height) then (size + height, height, cont)
    else (size, height, False)) acc

fenceIter :: [Int] -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
fenceIter [] acc = acc
fenceIter (l:ls) acc = fenceIter ls $ fenceAdd l acc
-- O(n^2)
fence ls = getSize $ maximumBy (comparing getSize) $ fenceIter ls []
getSize (s, _, _) = s


-- O(nlgn)
fence2 :: [Int] -> Int
fence2 ls = maximum [left, right, (overlap (take mid ls) (drop mid ls) 1000 0)]
  where
    mid = div (length ls) 2
    left = fence2 $ take mid ls
    right = fence2 $ drop mid ls
  -- actually not n complexity, in FP
  -- 
    overlap :: [Int] -> [Int] -> Int -> Int -> Int
    overlap lfst@(lf:lfs) rgst@(rg:rgs) h cnt
      | rgsTurn = max (newExnt lf) $ overlap lfst rgs (newh lf) incnt
      | otherwise = max (newExnt rg) $ overlap lfs rgst (newh rg) incnt
      where 
        rgsTurn = mod cnt 2 == 0
        newh ph = min h ph
        newExnt ph = cnt * newh ph
        incnt = cnt + 1

main = do
  -- print $ fence [7, 1, 5, 9, 6, 7, 3] -- 20
  -- print $ fence [1, 4, 4, 4, 4, 1, 1] -- 16
  -- print $ fence [1, 8, 2, 2] -- 8
  print $ fence2 [7, 1, 5, 9, 6, 7, 3] -- 20
  print $ fence2 [1, 4, 4, 4, 4, 1, 1] -- 16
  print $ fence2 [1, 8, 2, 2] -- 8
