
module EditRoutine where

import Data.Char
import Types

editRoutine :: Routine -> IO Routine
editRoutine r = do
  putStrLn "Let's go day by day and add your exercises"
  tentativeSchedule <- mapM editDay $ schedule r
  print $ prettyPrint tentativeSchedule
  putStrLn "Are you satisfied with this? (D)one or (E)dit"
  i <- getKey
  if i == 'D'
    then return Routine {routineName = routineName r , schedule = tentativeSchedule}
    else error "edit not added" --TODO: Add some reasonable editing ability that does not retraverse the whoel thing
--use state monad?


editDay :: (DayOfWeek, TrainingSession) -> IO (DayOfWeek, TrainingSession)
editDay (day, session) = do
  putStrLn $ "Is " ++ show day ++ " fine how it is? (Y)es or (N)o"
  print session
  i <- getKey
  if i == 'Y' then return (day,session) else do
    putStrLn "What is the name of the exercise you'd like to add?"
--    print session
    name <- getLine
    putStrLn "How many sets? Type a numeral or R to supply a range"
    s <- getKey --TODO: Make it accept numbers bigger than 9
    sets <- mkRange s
    putStrLn "How many reps? Type a numeral or R to supply a range"
    r <- getKey
    reps <- mkRange r
    let newEx = Exercise {name = name, workTarget = Target sets reps}
    let newDay = if session == RestDay
                 then (day, TrainingSession "temp name" [newEx]) --TODO: Add logic to write dayName
                 else (day, addEx session newEx)
    putStrLn $ "Added" ++ show newEx
    editDay newDay


mkRange :: Char -> IO (Int,Int)
mkRange 'R' = do putStrLn "Whats the low end of the range?"
                 l <- getKey
                 putStrLn "Whats the high end of the range?"
                 b <- getKey
                 if (not . all isDigit) [l,b] then print "Not numerals" >> mkRange 'R' else return (digitToInt l, digitToInt b)
mkRange s   = if isDigit s then return (0,sets) else undefined
                                        where sets = digitToInt s
