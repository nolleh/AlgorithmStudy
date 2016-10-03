
data QuadTree = X QuadTree QuadTree QuadTree QuadTree | Sx String | C Char deriving (Show)

flipQuadTree (Sx (a:b:c:d:[])) = Sx [c,d,a,b]
flipQuadTree (X q1 q2 q3 q4) = 
  X (flipQuadTree q3) (flipQuadTree q4) (flipQuadTree q1) (flipQuadTree q2)
flipQuadTree (C c) = C c

-- 디스플레이
quadTreeToStr :: QuadTree -> String
quadTreeToStr (Sx (a:b:c:d:[])) = "x(" ++ [a,b,c,d] ++ ")"
quadTreeToStr (X q1 q2 q3 q4) = "x(" ++ quadTreeToStr q1 ++ quadTreeToStr q2 ++ quadTreeToStr q3 ++ quadTreeToStr q4 ++ ")"
quadTreeToStr (C c) = [c]

strToQuadTree :: String -> QuadTree
strToQuadTree str = 
  reverseQuad $
  head $ -- 인자가 제대로 들어왔다면, 끝났을때 Quad (4개짜리) 로 다시 엮여있어야하므로, tail 은 필요 없다.
  foldl 
    (\acc x -> -- 뒤에 4개는 Q 로 만들고 앞은 그대로 엮는다
        if x == 'x' then (accTake acc)++[(makeQuad $ xData acc)]
        else acc ++ [(C x)]) [] $
    reverse str
  where sizeAcc acc = length acc
        xData acc = drop (sizeAcc acc - 4) acc
        accTake acc = take (sizeAcc acc - 4) acc
        makeQuad (q1:q2:q3:q4:[]) = X q1 q2 q3 q4
        reverseQuad (C c) = C c
        reverseQuad (X q1 q2 q3 q4) = X (reverseQuad q4) (reverseQuad q3) (reverseQuad q2)  (reverseQuad q1)

exercise = quadTreeToStr . flipQuadTree . strToQuadTree

main = do
  print $ exercise "w"
  print $ exercise "xbwwb"
  print $ exercise "xbwxwbbwb"
  print $ exercise "xxwwwbxwxwbbbwwxxxwwbbbwwwwbb"
  
-- 결과 --
-- "w"
-- "x(wbbw)"
-- "x(x(bwwb)bbw)"
-- "x(x(wbx(wwx(bbww)b)w)bx(wbww)x(wwwx(bbwb)))"

  -- 유닛테스트 -- 
  print $ quadTreeToStr $ flipQuadTree $ (C 'w')
  -- xbwwb -> xwbbw
  print $ quadTreeToStr $ flipQuadTree $ Sx "bwwb"
  -- xbwxwbbwb
  print $ quadTreeToStr $ flipQuadTree $ X (C 'b') (C 'w') (Sx "wbbw") (C 'b')
  -- x xwww b xw xwbbb ww xxxwwbbbwwwwbb
  -- -> xxwbxwwxbbww bwbxwbwwxwwwxbbwb
  print $ quadTreeToStr $ flipQuadTree $ X (Sx "wwwb") 
    (X (C 'w') (Sx "wbbb") (C 'w') (C 'w')) 
    (X (X (Sx "wwbb") (C 'b') (C 'w') (C 'w')) (C 'w') (C 'w') (C 'b')) 
    (C 'b')