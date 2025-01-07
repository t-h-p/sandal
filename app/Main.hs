module Main ( main ) where

import FileTree
import Graphing
import Data.GraphViz.Printing (renderDot)

main :: IO ()
main = do
    _ <- saveDotGraph "." "output.png"
    putStrLn "ran successfully :)"

