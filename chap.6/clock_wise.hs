import Data.List
import Debug.Trace

-- 각 인덱스의 시계를 돌리기 위해 필요한 스위치 번호들
switches :: [[Int]]
switches = [[0,1,2], [3,7,9,11], [4,10,14,15], [0,4,5,6,7], [6,7,8,10,12],
            [0,2,14,15], [3,14,15], [4,5,7,14,15], [1,2,3,4,5], [3,4,5,9,13]]

type Clock = (Int, Int)
type Clocks = [Clock]

clockWise clocks = clockWiseInner zipedClocks zipedClocks 0 switches
  where zipedClocks = zip [0..] clocks

clockWiseInner :: Clocks -> Clocks -> Int -> [[Int]] -> (Clocks, Int)
clockWiseInner [] clocks clicks _ = (clocks, clicks)
clockWiseInner _ clocks clicks [[]] = (clocks, clicks)
clockWiseInner (x:xs) clocks clicks swts = 
  minimumBy minimumSnd $
  map (\(cls, count) -> if clicks - count == 0 then clockWiseInner xs cls count swts
      else clockWiseInner xs cls count uneffectiveSwitches) $ -- > [Int]
    clocksWindTo12 (fst x) clocks clicks swts
  where uneffectiveSwitches =
          doIf (not.null) (filter (not.elem (fst x))) swts

minimumSnd (a1, b1) (a2, b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1 == b2 = compare a1 a2

clocksWindTo12 :: Int -> Clocks -> Int -> [[Int]] -> [(Clocks, Int)]
clocksWindTo12 _ clocks clicks [[]] = [(clocks, clicks)]
clocksWindTo12 i clocks clicks swts
  | (snd clock == 0) || (snd clock == 12) = [(clocks, clicks)]
  | null effectiveSwitches = [(clocks, 1000)]
  | otherwise = 
      concatMap (\clos -> clocksWindTo12 i clos (clicks + 1) effectiveSwitches) $
      map (\effect -> clickSwitch effect clocks) effectiveSwitches -- [[클릭해본 결과], [다른방법으로 클릭해본 결과]]
  where effectiveSwitches = doIf (not.null) (filter (elem i)) swts 
        clock = clocks !! i

-- i 번째 시계를 스위치를 눌러 한번 돌린다. 
clickSwitch :: [Int] -> [(Int, Int)] -> Clocks
clickSwitch switchEffects clocks = 
  mapIf (\(i, c) -> elem i switchEffects) (\(i, c)-> (i, clockWind c)) clocks

clockWind :: Int -> Int
clockWind clock = mod (clock + 3) 12

doIf p f x = if p x then f x else x
mapIf p f = map (\x -> doIf p f x)

main = do
  -- print $ clickSwitch (switches !! 0) $ zip [0..] [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]
  -- print $ clocksWindTo12 1 (zip [0..] [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]) 0 switches
  -- print $ clocksWindTo12 15 (zip [0..] [12, 9, 3, 12, 6, 6, 9, 3, 12, 9, 12, 9, 12, 12, 6, 6]) 0 switches
  print $ clockWise [12, 6, 6, 6, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]
  print $ clockWise [12, 9, 3, 12, 6, 6, 9, 3, 12, 9, 12, 9, 12, 12, 6, 6]