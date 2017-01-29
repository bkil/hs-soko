module CodeWorldConsole
  ( module CodeWorldConsole
  , module CodeWorldAsciiLib
  ) where

import CodeWorldAsciiLib
import CodeWorldAsciiLibPrivate
import CodeWorldConsolePrivate


drawingOf :: Picture -> IO ()
drawingOf picture = do
  print picture
  display <- getScreenSize
  let trimmed = unlines $ trim $ getAscii display picture
  putStr trimmed

animationOf :: (Double -> Picture) -> IO ()
animationOf draw = mapM_ (run . fromIntegral) [0..12] where
  run t = do
    print t
    drawingOf (draw t)
    sleep 1

interactionOf :: world ->
                (Double -> world -> world) ->
                (Event -> world -> world) ->
                (world -> Picture) ->
                IO ()
interactionOf w time event display = do
  drawingOf $ display w
  key <- generateKey
  interactionOf (event key w) time event display
