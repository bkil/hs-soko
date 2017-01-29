{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorldConsole
import Data.Char (ord)


data Coord = C Int Int
type State = (Coord, Direction, List Coord)
data Direction = R | U | L | D

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

handleTime :: Double -> State -> State
handleTime _ c = c

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo a b c =
  case eqCoord a c of
    True  -> b
    False -> c

handleEvent :: Event -> State -> State
handleEvent ev s = if isWon s then s else handleEvent2 ev s

handleEvent2 :: Event -> State -> State
handleEvent2 ev (c,d,boxes) = let
  (c2, d2) = proposeMove ev (c,d)
  in case mazeWithBoxes boxes c2 of
    Ground  -> (c2, d2, boxes)
    Storage -> (c2, d2, boxes)
    Box     -> let
      (c3, _) = proposeMove ev (c2,d2)
      movedBoxes = mapList (moveFromTo c2 c3) boxes
      in case mazeWithBoxes boxes c3 of
        Ground  -> (c2, d2, movedBoxes)
        Storage -> (c2, d2, movedBoxes)
        _ ->  (c, d2, boxes)
    _ -> (c, d2, boxes)

proposeMove :: Event -> (Coord, Direction) -> (Coord, Direction)
proposeMove (KeyPress key) (c,_)
    | key == "Right" = adjacentCoord2 R c
    | key == "Up"    = adjacentCoord2 U c
    | key == "Left"  = adjacentCoord2 L c
    | key == "Down"  = adjacentCoord2 D c
proposeMove _ s      = s

draw :: State -> Picture
draw s@(c, d, b) = wonText & drawPlayer & pictureOfBoxes b & pictureOfMaze where
  drawPlayer = atCoord c (player2 d)
  wonText =
    if isWon s
      then scaled 3 3 $ text "You won!"
      else blank

pictureOfMaze :: Picture
pictureOfMaze = foldr (&) blank level where
  level = [drawt (C x y) | x <- [1..mazeWidth], y <- [1..mazeHeight]]
  drawt c = atCoord c $ drawTile $ noBoxMaze c

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq

initialState :: State
initialState = (playerPos, U, initialBoxes) where
  playerPos = head $ concat pos
  pos = [
          [C x y
          | Just x <- [lookup (ord 'x') $ zip (map ord row) [1..]]
          ]
        | (row,y) <- zip mazeData [1..]]

mazeWidth :: Int
mazeWidth  = maximum $ map length mazeData

mazeHeight :: Int
mazeHeight = length mazeData

mazeData :: [String]
mazeData = [
  "######",
  "#.   ##",
  "##  o ###",
  " ## o   ####",
  "  ##...x   ##",
  " ##  ### oo #",
  "##  ## ##   ##",
  "#  ##   ##   #",
  "####     #####"
  ]

mazeData2 :: [String]
mazeData2 = [
  "#####",
  "#.ox#",
  "#####"
  ]

maze :: Coord -> Tile
maze (C x y)
  | x <= 0 || x > mazeWidth  = Blank
  | y <= 0 || y > mazeHeight = Blank
  | otherwise = let
    row = mazeData !! (mazeHeight - y)
    in if x > length row then Blank
       else let
         ch = row !! (x - 1)
         in case ch of
              '#' -> Wall
              '.' -> Storage
              'o' -> Box
              _   -> Ground

initialBoxes :: List Coord
initialBoxes = list2List list where
  list = concat [[C x y
                | maze (C x y) == Box]
                | x <- [1..mazeWidth], y <- [1..mazeHeight]]

list2List :: [a] -> List a
list2List = foldl appendList Empty . map (\a -> Entry a Empty)

appendList :: List a -> List a -> List a
appendList Empty b = b
appendList (Entry e a) b = appendList a (Entry e b)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile _ = blank

wall, ground, storage, box, player :: Picture
wall    = colored black  $ solidRectangle 1 1
ground  = blank
storage = colored yellow $ solidRectangle 1 1
box     = colored green  $ solidCircle 0.25
player  = colored red    $ solidCircle 0.30

player2 :: Direction -> Picture
player2 U = translated 0 0.2 player
player2 L = rotated (pi/2) $ player2 U
player2 D = rotated (pi/2) $ player2 L
player2 R = rotated (pi/2) $ player2 D

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated x0 y0 pic where
  x0 = fromIntegral x - fromIntegral mazeWidth / 2
  y0 = fromIntegral y - fromIntegral mazeHeight / 2

adjacentCoord2 :: Direction -> Coord -> (Coord, Direction)
adjacentCoord2 d c = (adjacentCoord d c, d)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

data SSState world = StartScreen | Running world

startScreenInteractionOf ::
    world -> (Double -> world -> world) ->
    (Event -> world -> world) -> (world -> Picture) ->
    IO ()
startScreenInteractionOf state0 step handle draw
  = interactionOf state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

data Interaction world =
  Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw) = let
  handle' (KeyPress "Esc") _ = state0
  handle' e s = handle e s
  in Interaction state0 step handle' draw

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

exercise1 :: Interaction State
exercise1 = Interaction initialState handleTime handleEvent draw

main :: IO ()
main = runInteraction (resetable (withStartScreen exercise1))

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
   Box -> Ground
   t   -> t

mazeWithBoxes :: List Coord -> (Coord -> Tile)
mazeWithBoxes Empty coord = noBoxMaze coord
mazeWithBoxes (Entry c bx) coord =
  if eqCoord c coord
    then Box
    else mazeWithBoxes bx coord

eqCoord :: Coord -> Coord -> Bool
eqCoord (C u v) (C x y) = u == x && v == y

isWon :: State -> Bool
isWon (_, _, boxes) = allList $ mapList isOnStorage boxes

isOnStorage :: Coord -> Bool
isOnStorage c = case maze c of
  Storage -> True
  _ -> False

allList :: List Bool -> Bool
allList Empty = True
allList (Entry e xs) = e && allList xs
