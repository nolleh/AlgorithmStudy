
-- 각 인덱스의 시계를 돌리기 위해 필요한 스위치 번호들
switches = [[0,1,2], [3,7,9,11], [4,10,14,15], [0,4,5,6,7], [6,7,8,10,12],
            [0,2,14,15], [3,14,15], [4,5,7,14,15], [1,2,3,4,5], [3,4,5,9,13]]

type Clock = (Int, Int)
type Clocks = [Clock]

clockWise clocks = clockWiseInner zipedClocks zipedClocks 0
  where zipedClocks = zip [0..] clocks

clockWiseInner :: Clocks -> Clocks -> Int -> Int
clockWiseInner [] _ clicks = clicks 
clockWiseInner (x:xs) clocks clicks = 
  minimum $
  take 10 $ 
  map (\(cls, count) -> clockWiseInner xs cls count) $ -- > [Int]
    clocksWindTo12 (fst x) clocks clicks

clocksWindTo12 :: Int -> Clocks -> Int -> [(Clocks, Int)] -- [(clocks, count)]
clocksWindTo12 i clocks clicks
  | (snd clock == 0) || (snd clock == 12) = [(clocks, clicks)]
  | clicks > 10 = [(clocks, clicks)]
  | otherwise = 
      concatMap (\clos -> clocksWindTo12 i clos (clicks + 1)) $
      map (\effect-> clickSwitch effect clocks) (effectiveSwitches i) -- [[클릭해본 결과], [다른방법으로 클릭해본 결과]]
  where effectiveSwitches clockIdx = filter (elem clockIdx) switches
        clock = clocks !! i

-- i 번째 시계를 스위치를 눌러 한번 돌린다. 
clickSwitch :: [Int] -> [(Int, Int)] -> Clocks
clickSwitch switchEffects clocks = 
  mapIf (\(i, c) -> elem i switchEffects) (\(i, c)-> (i, clockWind c)) clocks

clockWind :: Int -> Int
clockWind clock = mod (clock + 3) 12

mapIf p f = map (\x -> if p x then f x else x)

main = do
  -- print $ clickSwitch (switches !! 0) $ zip [0..] [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]
  -- print $ clocksWindTo12 1 (zip [0..] [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]) 0
  -- print $ clocksWindTo12 2 (zip [0..] [12, 9, 3, 12, 6, 6, 9, 3, 12, 9, 12, 9, 12, 12, 6, 6]) 0
  print $ clockWise [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]
  print $ clockWise [12, 9, 3, 12, 6, 6, 9, 3, 12, 9, 12, 9, 12, 12, 6, 6]