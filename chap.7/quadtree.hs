import Data.List
-- weave = groupBy (\x y -> if snd x == 'x' then ) $
--   zip [0..]
-- quadTree = 


runQuadTree =
  print $ quadTree "w"
  print $ quadTree "xbwwb" -- xwbbw
-- xbwxwbbwb -> x b w x(wbbw) b -> x x(wbbw) b b w -> x x(bwwb) b b w
-- [b, w, [w, b, b, w] b]
  print $ quadTree "xbwxwbbwb" -- xxbwwbbbw
  print $ quadTree "xxwwwbxwxwbbbwwxxxwwbbbwwwwbb" -- xxwbxwwxbbwwbwbxwbwwxwwwxbbwb

main = do
  print $ runQuadTree