module CodeWorldAsciiLibPrivate where

import CodeWorldTypes


type Field = [String]

defaultColor = gray

color2char Black  = '#'
color2char Red    = 'r'
color2char Green  = 'g'
color2char Yellow = 'y'
color2char Gray   = '.'
color2char Background = ' '

getExtent :: StyledOperation -> (Origin, Origin)

getExtent (_, (x, y), SolidRectangle w h) =
  ((x-w/2, y-h/2), (x+w/2, y+h/2))

getExtent (_, (x, y), Circle r) =
  ((x-r, y-r), (x+r, y+r))

getExtent (_, xy, Text t) =
  (xy, xy)

getScale :: [Origin] -> (Origin -> Double) -> Int -> Double -> (Double, Double)
getScale extents select dim aspect = let
  allZ = map ((aspect*) . select) extents
  fudge = 0.01
  minZ = minimum allZ - fudge
  maxZ = maximum allZ + fudge
  rangeZ = maxZ - minZ
  scaleZ = rangeZ / (fromIntegral dim - 1)
  in (minZ, scaleZ)

screenCoordinates :: [StyledOperation] -> Int -> Int -> Double -> Tr
screenCoordinates operations w h aspect = let
  extent2list (xy, uv) = [xy, uv]
  extents = concatMap (extent2list . getExtent) operations
  (minX, scaleX) = getScale extents fst w aspect
  (minY, scaleY) = getScale extents snd h 1
  scale = max scaleX scaleY
  in \(x, y) -> let
    x2 = (minX + fromIntegral (x-1) * scale) / aspect
    y2 =  minY + fromIntegral (h-y) * scale
    in (x2, y2)

putShape :: Tr -> Color -> (Origin -> Bool) -> Field -> Field
putShape tr color eq field = let
  char ij old =
    case eq (tr ij) of
      True -> color2char color
      _ -> old
  in [[char (i, j) old | (i,old)<-zip [1..] row ] | (j,row)<- zip [1..] field]

blitElement :: Tr -> Field -> StyledOperation -> Field

blitElement tr field (color,(x,y),SolidRectangle w h) =
  putShape tr color (\(i, j) -> i>=x-w/2 && i<x+w/2 && j>=y-h/2 && j<y+h/2) field

blitElement tr field (color,(x,y),Circle r) = let
  sqr x = x * x
  in putShape tr color (\(i, j) -> sqr (i-x) + sqr (j-y) < sqr r) field

blitElement tr (row1:field) (_,(_,_),Text t) = let
  cols = length row1
  newRow1 = take cols t ++ drop cols row1
  in (newRow1:field)

type DisplayProperties = (Int, Int, Double)

getAscii :: DisplayProperties -> Picture -> Field
getAscii (w, h, aspect) (Picture operations) = let
  tr = screenCoordinates operations w h aspect
  initialField = [[color2char Background | _<-[1..w]] | _<-[1..h]]
  in foldl (blitElement tr) initialField operations
