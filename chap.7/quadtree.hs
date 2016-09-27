import Data.List
import Debug.Trace

weave :: String -> [String]
weave = 
  foldl 
    (\acc x -> 
      -- ing.. x 가 기존에 있었던 경우에도 합쳐버린다. 음 이건 안되는데 ~ 
        if x == 'x' then trace (show "acctake:" ++ show acc) $ accTake acc ++ [concat (xData acc)]
        else acc ++ [[x]]) []
  where sizeAcc acc = length acc
        xData acc = drop (sizeAcc acc - 4) acc
        accTake acc = take (sizeAcc acc - 4) acc

quadTree xs = trace (reverse xs) 
  weave $ reverse xs

main = do
  -- print $ ["b", "b", "w"] ++ [concat ["b", "b", "w"]] 
  -- print $ map (\x -> [x]) [1,2,3,4]
  -- print $ 'x':'b':'c':[]
  -- print $ quadTree "w"
  -- print $ quadTree "xbwwb" -- xwbbw
-- xbwxwbbwb -> x b w x(wbbw) b -> x x(wbbw) b b w -> x x(bwwb) b b w
-- [b, w, [w, b, b, w] b]
  print $ quadTree "xbwxwbbwb" -- xxbwwbbbw
  -- print $ quadTree "xxwwwbxwxwbbbwwxxxwwbbbwwwwbb" -- xxwbxwwxbbwwbwbxwbwwxwwwxbbwb
