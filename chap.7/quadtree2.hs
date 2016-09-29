
data QuadTree = X QuadTree QuadTree QuadTree QuadTree | Sx String | C Char deriving (Show)

flipQuadTree (Sx (a:b:c:d:[])) = Sx [c,d,a,b]
flipQuadTree (X q1 q2 q3 q4) = 
  X (flipQuadTree q3) (flipQuadTree q4) (flipQuadTree q1) (flipQuadTree q2)
flipQuadTree (C c) = C c

strQuadTree :: QuadTree -> String
strQuadTree (Sx (a:b:c:d:[])) = "x(" ++ [a,b,c,d] ++ ")"
strQuadTree (X q1 q2 q3 q4) = "x(" ++ strQuadTree q1 ++ strQuadTree q2 ++ strQuadTree q3 ++ strQuadTree q4 ++ ")"
strQuadTree (C c) = [c]

main = do
  print $ strQuadTree $ flipQuadTree $ (C 'w')
  -- xbwwb -> xwbbw
  print $ strQuadTree $ flipQuadTree $ Sx "bwwb"
  -- xbwxwbbwb
  print $ strQuadTree $ flipQuadTree $ X (C 'b') (C 'w') (Sx "wbbw") (C 'b')
  -- x xwww b xw xwbbb ww xxxwwbbbwwwwbb
  -- -> xxwbxwwxbbww bwbxwbwwxwwwxbbwb
  print $ strQuadTree $ flipQuadTree $ X (Sx "wwwb") 
    (X (C 'w') (Sx "wbbb") (C 'w') (C 'w')) 
    (X (X (Sx "wwbb") (C 'b') (C 'w') (C 'w')) (C 'w') (C 'w') (C 'b')) 
    (C 'b')

-- "w"
-- "x(wbbw)"
-- "x(x(bwwb)bbw)"
-- "x(x(wbx(wwx(bbww)b)w)bx(wbww)x(wwwx(bbwb)))"