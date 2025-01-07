module Graphing
    ( saveDotGraph ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Tree
import FileTree


fileParams :: (Labellable nl) => GraphvizParams Node nl el () nl
fileParams = nonClusteredParams {
    globalAttributes = [GraphAttrs [RankDir FromLeft]],
    fmtNode = formatSettings}
    where
        formatSettings (nodeId,label) = [toLabel label, Shape Ellipse]



fileVis :: (Graph gr) => gr String String -> DotGraph Node
fileVis = graphToDot fileParams

buildDotGraph :: FilePath -> IO (DotGraph Node)
buildDotGraph filepath = do
    grTup <- buildFsGraph filepath
    return (fileVis (uncurry createGr grTup))

saveDotGraph :: FilePath -> FilePath -> IO FilePath
saveDotGraph start savepath = do
    gr <- buildDotGraph start
    runGraphviz gr Png savepath



mynodes :: [LNode String]
mynodes = [(1, "src"),(2, "other.rs"),(3, "other"),(4, "a.rs"), (5, "b.rs"), (6, "c.rs"), (7, ".hide"), (8, "content.txt"), (9, "licensing"), (10, "other_dir")]

myedges :: [LEdge String]
myedges = [(1, 2, "src -> other.rs"), (1, 3, "src -> other")
            , (3, 4, "other -> a.rs"), (10, 8, "other_dir -> content.txt")
            , (3, 5, "other -> b.rs"), (3, 6, "other -> c.rs")
            , (1, 7, "src -> .hide"), (1, 9, "src -> licensing")]


outputGraph :: Gr String String
outputGraph = mkGraph mynodes myedges

-- weird mkGraph bullshit to specify concrete graph type Gr from PatriciaTree
createGr :: [LNode String] -> [LEdge String] -> Gr String String
createGr = mkGraph

buildDotRep :: [LNode String] -> [LEdge String] -> DotGraph Node
buildDotRep nl el = fileVis (createGr nl el)

outputDot :: DotGraph Node
outputDot = fileVis outputGraph