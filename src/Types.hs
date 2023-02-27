{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Char (toUpper)

data Routine = Routine { routineName :: String
                       , schedule    :: Schedule
                       }

type Schedule = [(DayOfWeek, TrainingSession)]

data DayOfWeek = Mon | Tues | Wed | Thurs | Fri | Sat | Sun deriving (Eq, Ord, Show, Enum)

data TrainingSession = RestDay
                     | TrainingSession String [Exercise]
                     deriving (Show, Eq)

addEx :: TrainingSession -> Exercise -> TrainingSession
addEx (TrainingSession s exs) e = TrainingSession s (exs ++ [e])
addEx _ _ = undefined

data Exercise = Exercise { name          :: String
                         , workTarget    :: Target
                         } deriving Eq-- style? RPT/TEMPO/etc.

data Target   = Target SetRange RepRange deriving Eq
type Sets     = Int
type SetRange = (Sets, Sets)
type RepRange = (Reps, Reps)
type Reps     = Int
type Month = Int
type Day = Int
type Year = Int

data Tag    = Easy | Sufficient | Hard
data Date   = MDY Month Day Year
type Note = String
type Weight = Int

instance Show Exercise where
  show (Exercise {..}) = name ++ ": " ++ show workTarget
           --uses RecordWildCard extension
instance Show Target where
  show (Target s r) = showRange s ++ "x" ++ showRange r
        where showRange (0,b) = show b
              showRange (a,b) = show a ++ "-" ++ show b

instance Show Routine where
  show (Routine {..}) = map toUpper routineName <> "\n" <> prettyPrint schedule

prettyPrint :: Schedule -> String
prettyPrint = concatMap (\d -> show' d ++ "\n")
    where show' (day, RestDay) = show day ++ ": Rest Day\n"
          show' (day, TrainingSession name xs) = show day ++ ":\n" ++ name ++ "\n" ++ concatMap (\x -> "    " ++ show x ++ "\n") xs

getKey :: IO Char --TODO: Move me out of here, I'm not a type
getKey = do
  i <- getLine
  let input = map toUpper i
  return $ head input --TODO: Take safe head


data Entry  = Entry { eName          :: String
                    , eWorkTarget    :: Target
                    , performedWork :: [WorkSet]
                    , date          :: Date
                    , wNote         :: Note
                    , wTag          :: Tag
                    , assocRoutine  :: String --TODO: Make RoutineName type?
                    }

data WorkSet = Set Reps Weight Tag Note
