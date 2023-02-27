module Main (main) where

import Data.Char
import Lib
import Types
import CreateRoutine
import EditRoutine
import CreateEntry

main :: IO ()
main = do
  print optionsPrompt
  readFile "./data/testFile.txt" >>= print
  selection <- getKey
  case selection of 'C' -> do routine <- createRoutine
                              newRoutine <- editRoutine routine
                              print newRoutine--let choice = schedule gslpRoutine !! (digitToInt (head selection) + 1)
                              writeFile "./data/testFile.txt" (show newRoutine) --TODO: Make this a real file format, maybe use aeson
                              readFile "./data/testFile.txt" >>= print
                    'L' -> print testRoutineData --Show available routines
                           --Select Routine
                           --Show Selected Routine
                           --Select Day
                           --pass selected day to createEntry
                           --receive entry
                           --Write File for now
                    'Q' -> return ()
                    _   -> putStrLn "Invalid Selection" >> main
  return ()

optionsPrompt :: String
optionsPrompt = "(C)reate a new routine | (L)og today's workout | (Q)uit"


testRoutineData :: [Routine]
testRoutineData = [gslpRoutine]

gslpRoutine :: Routine
gslpRoutine = Routine { routineName = "Greyskull LP"
                      , schedule    = gslpSchedule
                      }

gslpSchedule :: Schedule
gslpSchedule = zip (firstDayOn Mon) (weekOne <> weekTwo)

weekOne = [ TrainingSession "Monday " $ day1 aGroup
          , RestDay
          , TrainingSession "Wednesday" $ day2 bGroup
          , RestDay
          , TrainingSession "Friday" $ day3 aGroup
          , RestDay
          , RestDay]

weekTwo = [ TrainingSession "Monday" $ day1 bGroup
          , RestDay
          , TrainingSession "Wednesday" $ day2 aGroup
          , RestDay
          , TrainingSession "Friday" $ day3 bGroup
          , RestDay
          , RestDay]

--Alternating exercises
aGroup = [ "Overhead Press", "Chinups" ]
bGroup =  [ "Bench Press", "Barbell Rows" ]

day1 :: [String] -> [Exercise]
day1 alt = apply3x5 <$> alt <> ["Squats"]

day2 :: [String] -> [Exercise]
day2 alt = (apply3x5 <$> alt)
   <> [( apply3x5 "Deadlift") { workTarget = Target (0,1) (0,5) }] --ugly overload; too messy otherwise!

day3 :: [String] -> [Exercise]
day3 alt = apply3x5 <$> alt <> ["Squats"]

--A convenience function, since almost all of these are 3x5
apply3x5 :: String -> Exercise
apply3x5 s = Exercise { name = s
                      , workTarget = Target (0,3) (0,5)
                      }
