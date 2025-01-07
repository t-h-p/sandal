module FileTree
    (buildFsGraph) where

import System.Directory
import Data.Graph.Inductive.Graph
import Data.Tree

type ErrNum = Int

-- if no directory arg provided, use current directory
tryPath :: String -> IO ErrNum
tryPath arg = do
    validDir <- doesDirectoryExist arg
    if validDir then return 0 else return 1

data FsNode = DirNode String | FileNode String

instance Show FsNode where
    show (DirNode name) = name
    show (FileNode name) = name

fsNodeToLNode :: Int -> FsNode -> LNode String
fsNodeToLNode nodeId (DirNode name) = (nodeId, name)
fsNodeToLNode nodeId (FileNode name) = (nodeId, name)

trimFilepath :: String -> String
trimFilepath filepath = reverse $ takeWhile (/= '/') $ reverse filepath

createFsNode :: FilePath -> IO (FsNode, [FilePath])
createFsNode filepath = do
    isDirectory <- doesDirectoryExist filepath
    if isDirectory
        then do
            contents <- listDirectory filepath
            let fullPaths = map (\p -> filepath ++ "/" ++ p) contents
            return (DirNode (trimFilepath filepath), fullPaths)
        else return (FileNode (trimFilepath filepath), [])

buildOgFileTree :: FilePath -> IO (Tree FsNode)
buildOgFileTree arg = do
    code <- tryPath arg
    if code == 0 then unfoldTreeM_BF createFsNode arg
    else error ("Exited with error code " ++ show code)

buildLNodes :: Tree FsNode -> [LNode String]
buildLNodes tree = fst $ traverseTree tree 0
    where
        traverseTree :: Tree FsNode -> Int -> ([LNode String], Int)
        traverseTree (Node fsNode subtrees) currentId =
            let
                currentNode = fsNodeToLNode currentId fsNode
                (childNodes, nextId) = foldl
                    (\(accNodes, nextId) subtree ->
                        let (subtreeNodes, newId) = traverseTree subtree nextId
                        in (accNodes ++ subtreeNodes, newId)
                    )
                    ([], currentId + 1)
                    subtrees
            in (currentNode : childNodes, nextId)

buildLEdges :: Tree FsNode -> [LEdge String]
buildLEdges tree = fst $ traverseTree tree 0 Nothing
    where
        traverseTree :: Tree FsNode -> Int -> Maybe Int -> ([LEdge String], Int)
        traverseTree (Node _ subtrees) currentId parentId =
            let
                currentEdges = case parentId of
                    Just pid -> [(pid, currentId, "-")]
                    Nothing  -> []
                (childEdges, nextId) = foldl
                    (\(accEdges, nextId) subtree ->
                        let (subtreeEdges, newId) = traverseTree subtree nextId (Just currentId)
                        in (accEdges ++ subtreeEdges, newId)
                    )
                    ([], currentId + 1)
                    subtrees
            in (currentEdges ++ childEdges, nextId)

convertOgFileTree :: Tree FsNode -> ([LNode String], [LEdge String])
convertOgFileTree tree = (buildLNodes tree, buildLEdges tree)

buildFsGraph :: FilePath -> IO ([LNode String], [LEdge String])
buildFsGraph filepath = do
    tree <- buildOgFileTree filepath
    return (convertOgFileTree tree)