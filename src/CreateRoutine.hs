module CreateRoutine where

import Types
import Data.Char
import Data.Maybe

createRoutine :: IO Routine
createRoutine = do
  putStrLn "What's the name of your routine?"
  name <- getLine
  putStrLn "What is the first day of the week on your routine?"
  let dayIndex = zip (show <$> [1..7]) [Mon,Tues,Wed,Thurs,Fri,Sat,Sun]
  print dayIndex
  f <- getLine
  let firstDayNum = head f
  firstDay <- if firstDayNum `notElem` "1234567"
              then putStrLn "Defaulting to Monday" >> return (Just Mon)
              else  return $ lookup [firstDayNum] dayIndex
  putStrLn "How many days is your training block?"
  blockLengthString <- getLine
  blockLength <- if all isDigit blockLengthString
                 then return (read blockLengthString :: Int)
                 else putStrLn "Not an int" >> createRoutine >> return 0 -- This should never be reached UwU
  let emptySchedule = take blockLength $ zip (firstDayOn (fromJust firstDay)) (repeat RestDay)
  return $ Routine { routineName = name , schedule = emptySchedule}


firstDayOn :: DayOfWeek -> [DayOfWeek]
firstDayOn = cycle . take 7 . iterate succ
