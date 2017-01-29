module CodeWorldConsolePrivate where

import CodeWorldTypes
import CodeWorldAsciiLibPrivate

import System.Posix.Unistd (sleep)
import System.IO (hFlush, stdout)
import System.Environment (getEnv)

sleep = System.Posix.Unistd.sleep

trim = map $ reverse . dropWhile (== ' ') . reverse

getEnvDefault :: Read a => String -> a -> IO a
getEnvDefault key def = catch (getEnv key >>= return . read) (\_ -> return def)

-- you need to export these manually from bash or get them via tset
getScreenSize :: IO DisplayProperties
getScreenSize = do
  w  <- getEnvDefault "COLUMNS" 80
  h0 <- getEnvDefault "LINES"   25
  let h = h0 - 1
  return (w, h, fromIntegral w / fromIntegral h / (1920/1080))

generateKey :: IO (Event)
generateKey = do
  putStr " (type i,j,k,l and enter to move, empty=Esc, space=go) "
  hFlush stdout
  line <- getLine
  let key =
        case line of
          "i" -> "Up"
          "k" -> "Down"
          "j" -> "Left"
          "l" -> "Right"
          " " -> " "
          ""  -> "Esc"
          _   -> line
  return $ KeyPress key
