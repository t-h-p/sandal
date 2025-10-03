module Main (main) where

import Graphing
import Settings ( getSettings )

main :: IO ()
main = do
  settings <- getSettings
  _ <- saveDotGraph "." "output.png"
  putStrLn "ran successfully :)"
