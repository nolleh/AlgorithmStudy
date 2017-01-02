import Debug.Trace

fanMeeting hipers fans = 
  length $
  filter (== ln) $
  map (fanMeetingIter hipers) newFans
  where
    ln = length hipers
    lnFans = length fans 
    mid = div ln 2
    newFans = [take ln $ drop a fans | a <- [0..(lnFans - ln)]]

fanMeetingIter (h:[]) (f:[]) = 
  -- trace ("h:" ++ show h ++ "f:" ++ show f) $ 
  highfive h f
fanMeetingIter hipers fans = 
  fanMeetingIter (left hipers) (left fans) + fanMeetingIter (right hipers) (right fans)
  where
    ln = length hipers
    lnFans = length fans 
    mid = div ln 2
    left ls = take mid ls
    right ls
      | mid == 0 = []
      | otherwise = drop mid ls


highfive :: Char -> Char -> Int
highfive h f 
  | h == 'M' && f == 'M' = 0
  | otherwise = 1

main = do
  print $ fanMeeting "FFFMMM" "MMMFFF"
  print $ fanMeeting "FFFFF" "FFFFFFFFFF"
  print $ fanMeeting "FFFFM" "FFFFFMMMMF"
  print $ fanMeeting "MFMFMFFFMMMFMF" "MMFFFFFMFFFMFFFFFFMFFFMFFFFMFMMFFFFFFF"
  -- print $ highfive 'M' 'M' -- 0
  -- print $ highfive 'F' 'F' -- 1
  -- print $ highfive 'M' 'F' -- 1
  -- print $ highfive 'F' 'M' -- 1