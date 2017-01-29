module CodeWorldTypes where


data Color = Background | Black | Red | Green | Yellow | Gray
  deriving Show

black = Black
gray = Gray
red = Red
green = Green
yellow = Yellow

data Picture = Picture [StyledOperation]

instance Show Picture where
  show (Picture ops) = unlines $ "Picture [" : map show ops ++ ["]"]

type Origin = (Double, Double)

type Text = String
data MouseButton
data Point

data Event = KeyPress Text
           | KeyRelease Text
           | MousePress MouseButton Point
           | MouseRelease MouseButton Point
           | MouseMovement Point

data Operation =
  SolidRectangle Double Double |
  Circle Double |
  Text String
  deriving Show

type Tr = (Int,Int) -> Origin

type StyledOperation = (Color, Origin, Operation)
