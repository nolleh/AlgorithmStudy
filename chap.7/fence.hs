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
fence ls = getSize $ maximumBy (comparing getSize) $ fenceIter ls []
getSize (s, _, _) = s

main = do
  print $ fence [7, 1, 5, 9, 6, 7, 3]
  print $ fence [1, 4, 4, 4, 4, 1, 1]
  print $ fence [1, 8, 2, 2]
