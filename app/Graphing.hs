module Graphing
  ( saveDotGraph
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import FileTree
import Settings ( Settings, squareDirs, getSettings)

fileParams :: (Labellable nl) => Attribute -> GraphvizParams Node nl el () nl
fileParams shape_arg =
  nonClusteredParams
    { globalAttributes = [GraphAttrs [RankDir FromLeft]]
    , fmtNode = formatSettings
    }
  where
    formatSettings (_, label) = [toLabel label, shape_arg]

squareOrEllipse :: Bool -> Attribute
squareOrEllipse square = if square then Shape Square else Shape Ellipse

fileVis :: (Graph gr) => Settings -> gr String String -> DotGraph Node
fileVis settings graph =
  let shape_arg = squareOrEllipse (squareDirs settings) in
  graphToDot (fileParams shape_arg) graph

-- weird mkGraph bullshit to specify concrete graph type Gr from PatriciaTree
createGr :: [LNode String] -> [LEdge String] -> Gr String String
createGr = mkGraph

buildDotGraph :: FilePath -> IO (DotGraph Node)
buildDotGraph filepath = do
  grTup <- buildFsGraph filepath
  settings <- getSettings
  return (fileVis settings (uncurry createGr grTup))

saveDotGraph :: FilePath -> FilePath -> IO FilePath
saveDotGraph start savepath = do
  gr <- buildDotGraph start
  runGraphviz gr Png savepath
