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
fence2 [] = 0
fence2 ls =
  maximum [fence2 left, fence2 right, overlap (reverse left) right 1000 1]
  where
    ln = length ls
    mid = div ln 2
    left = take mid ls
    right
      | mid == 0 = []
      | otherwise = drop mid ls
    overlap :: [Int] -> [Int] -> Int -> Int -> Int
    overlap [] _ _ _ = 0
    overlap _ [] _ _ = 0
    overlap lfst@(lf:lfs) rgst@(rg:rgs) h cnt
      | rgsTurn = max (newExnt rg) $ overlap lfst rgs (newh rg) incnt
      | otherwise = max (newExnt lf) $ overlap lfs rgst (newh lf) incnt
      where 
        rgsTurn = lf < rg
        newh ph = min h ph
        newExnt ph = cnt * (newh ph)
        incnt = cnt + 1

main = do
  print $ fence [7, 1, 5, 9, 6, 7, 3] -- 20
  print $ fence [1, 4, 4, 4, 4, 1, 1] -- 16
  print $ fence [1, 8, 2, 2] -- 8
  print $ fence2 [7, 1, 5, 9, 6, 7, 3] -- 20
  print $ fence2 [1, 4, 4, 4, 4, 1, 1] -- 16
  print $ fence2 [1, 8, 2, 2] -- 8
