import qualified Data.Map as Map

data Person = Person {
  name :: String,
  age :: Integer
} deriving (Show)

person = Person "xiaowang" 24

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber map = 
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "not founde"
    Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "takend"