module CodeWorldAsciiLib
  ( module CodeWorldTypes
  , module CodeWorldAsciiLib
  ) where

import CodeWorldTypes
import CodeWorldAsciiLibPrivate


blank :: Picture
blank = Picture []

solidCircle :: Double -> Picture
solidCircle radius = Picture [(defaultColor, (0, 0), Circle radius)]

rectangle :: Double -> Double -> Picture
rectangle = solidRectangle

solidRectangle :: Double -> Double -> Picture
solidRectangle w h = Picture [(defaultColor, (0,0), SolidRectangle w h)]

path :: [Origin] -> Picture
path [(u,v), (x,y)] = let
  r = (abs(u-x) + abs(x-y))/4
  x0 = (u + x)/2
  y0 = (v + y)/2
  in translated x0 y0 $ solidCircle r

(&) :: Picture -> Picture -> Picture
(&) (Picture a) (Picture b) = Picture (b ++ a)

colored :: Color -> Picture -> Picture
colored color (Picture operations) = Picture $ map recolor operations where
  recolor (_,origin,operation) = (color, origin, operation)

translated :: Double -> Double -> Picture -> Picture
translated u v (Picture operations) = Picture $ map reposition operations where
  reposition (color,(x,y),operation) = (color, (x+u, y+v), operation)

rotated :: Double -> Picture -> Picture
rotated a (Picture operations) = Picture $ map rotate operations where
  rotate (color,(x,y),operation) = let
    x2 = x * cos a - y * sin a
    y2 = y * cos a + x * sin a
    in (color, (x2, y2), operation)

text :: String -> Picture
text t = Picture [(defaultColor, (0,0), Text t)]

scaled :: Double -> Double -> Picture -> Picture
scaled u v (Picture operations) = Picture $ map rescale operations where
  rescale (color,(x,y),operation) = (color, (u*x, u*y), rescaleOp operation)
  rescaleOp t@(Text _) = t
