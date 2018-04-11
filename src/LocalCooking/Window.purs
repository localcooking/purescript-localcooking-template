module LocalCooking.Window where


import Prelude
import Data.Tuple (Tuple (..))
import Data.Generic (class Generic, gEq, gShow)


data WindowSize
  = Desktop
  | Laptop
  | Tablet
  | Phone
  | Pager

derive instance genericWindowSize :: Generic WindowSize

instance eqWindowSize :: Eq WindowSize where
  eq = gEq

instance ordWindowSize :: Ord WindowSize where
  compare x y =
    if x == y
      then EQ
      else case Tuple x y of
        Tuple Pager _ -> LT
        Tuple _ Desktop -> LT
        Tuple Desktop _ -> GT
        Tuple _ Pager -> GT
        Tuple Phone _ -> case y of
          Pager -> GT
          _ -> LT
        Tuple Tablet _ -> case y of
          Pager -> GT
          Phone -> GT
          _ -> LT
        Tuple Laptop _ -> case y of
          Desktop -> LT
          _ -> GT

instance showWindowSize :: Show WindowSize where
  show = gShow



widthToWindowSize :: Int -> WindowSize
widthToWindowSize s
  | s <= 480 = Pager
  | s <= 960 = Phone
  | s <= 1280 = Tablet
  | s <= 1600 = Laptop
  | otherwise = Desktop
