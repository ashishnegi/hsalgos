{-# LANGUAGE BangPatterns #-}
module LongestEdgePathInDAG where

import Data.Map.Strict as Map
import Data.List as DL
import Data.Set as Set
import Data.Maybe as Maybe
import Debug.Trace
import Control.DeepSeq
-- import Data.Text.Lazy.IO as TIO

type OutGoingEdges = Map.Map NodeId [ NodeId ]
type NodesData = Map.Map NodeId Node

data DAG = AdjList
           { outGoingEdges :: OutGoingEdges
           , nodesData :: NodesData
           } deriving (Eq, Show)

data Node = Node
            { heightOfNode :: !Int
            } deriving (Eq, Show)

type NodeId = Int
type DAGDataPath = String
type SourceNodes = [NodeId]

data LongestEdgePath = LongestEdgePath
                       { pathLength :: !Int
                       , verticalDrop :: !Int
                       } deriving (Eq, Ord, Show)

edgesOfNode :: NodeId -> DAG -> [NodeId]
edgesOfNode nodeId graph =
  case Map.lookup nodeId (outGoingEdges graph) of
    Nothing -> []
    Just v -> v

nodeData :: NodeId -> DAG -> Node
nodeData nodeId graph =
  case Map.lookup nodeId (nodesData graph) of
    Nothing -> error ("nodeData : nodeId " ++ show nodeId ++ " does not exist in map")
    Just v -> v


longestEdgePath :: SourceNodes -> DAG -> LongestEdgePath
longestEdgePath sourceNodes graph =
  DL.foldl' longestPath' (LongestEdgePath 0 0) sourceNodes
  where
    longestPath' longPathTillNow nodeId =
      let nodeLongPath = longestPath nodeId
      in if pathLength nodeLongPath > pathLength longPathTillNow
         then nodeLongPath
         else longPathTillNow

    longestPath nodeId =
      -- this is dfs from this node
      -- since this is a dag, we do not need to take care of unvisited nodes.
      let (l, lowestVal) = depthFromNode nodeId graph
      in l { verticalDrop = heightOfDagNode nodeId - lowestVal }

    depthFromNode nodeId graph = DL.foldl' (goInDepth nodeId) (LongestEdgePath 1 0, heightOfDagNode nodeId) $ edgesOfNode nodeId graph

    heightOfDagNode nodeId = heightOfNode . nodeData nodeId $ graph
    goInDepth nodeId !pathTillNow@(LongestEdgePath l h, lowestVal) nextNodeId =
      let (newPath, lowVal) = depthFromNode nextNodeId graph
      in case compare (1 + (pathLength newPath)) l of
           LT -> pathTillNow
           GT -> (LongestEdgePath (1 + (pathLength newPath)) 0, lowVal)
           EQ -> if lowVal < lowestVal
                 then (LongestEdgePath (1 + (pathLength newPath)) 0, lowVal)
                 else pathTillNow

makeEdge :: NodeId -> NodeId -> DAG -> DAG
makeEdge fromId toId graph =
  let existingEdges = Map.findWithDefault [] fromId (outGoingEdges graph)
      newEdges = toId : existingEdges
  in graph { outGoingEdges = Map.insert fromId newEdges (outGoingEdges graph) }

-- SourceNodes are nodes that have no incoming edges.
makeDAG :: DAGDataPath -> IO (DAG, SourceNodes)
makeDAG filepath = do
  listOfListOfInts <- makeInteger <$> readLines filepath
  let [width, height] = head listOfListOfInts
      numNodes = width * height
      rows = (replicate width 1501) : (drop 1 listOfListOfInts) ++ [(replicate width 1501)]
      -- id generation was wrong.
      heightsWithNodeIdsRows = fmap (\ (row, rowId) -> fmap (\ (height, colId) -> (height, rowId * width + colId)) $ zip row [1..]) $ zip rows [1..]
      emptyGraph = AdjList Map.empty $ Map.fromList (fmap (\(h, nid) -> (nid, Node h)) . concat . tail . init $ heightsWithNodeIdsRows)
      emptyNodesWithEdges = Set.empty
      threeRowsInOneGo = zip3 heightsWithNodeIdsRows (drop 1 heightsWithNodeIdsRows) (drop 2 heightsWithNodeIdsRows)
      (graph, nodesWithInEdges) = DL.foldl' makeGraph (emptyGraph, emptyNodesWithEdges) threeRowsInOneGo
      sourceNodes = Set.difference (Set.fromList . Map.keys . nodesData $ graph) nodesWithInEdges
  -- traceShow [take 10 . Map.keys . nodesData $ graph] (return (Set.toList sourceNodes))
  -- traceShow graph (return (Set.toList sourceNodes))
  -- traceShow sourceNodes (return (Set.toList sourceNodes))
  return $ (graph, Set.toList sourceNodes)

  where
    makeGraph (graphTillNow, nodesWithInEdges) (prevRow, row, nextRow) =
      let updownEdges = zip3 prevRow row nextRow
          (graph', nodesInEdges') =  addEdges (graphTillNow, nodesWithInEdges) updownEdges
          leftRightEdges = zip3 ((1501, 0) : row) (drop 1 row) (drop 2 row)
          (graph'', nodesInEdges'') = addEdges (graph', nodesInEdges') leftRightEdges
      in (graph'', nodesInEdges'')
    addEdges (g, n) edges =
      DL.foldl' (\ !(!g', !n') ((p, pId), (c, cId), (n, nId)) ->
                  let (g'', n'') = if c > p
                                   then (makeEdge cId pId g', Set.insert pId n')
                                   else (g', n')
                      (g''', n''') = if c > n
                                     then (makeEdge cId nId g'', Set.insert nId n'')
                                     else (g'', n'')
                  in (g''', n'''))
                (g, n)
                edges

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [[Int]]
makeInteger = (\x -> deepseq x x) . fmap getInts . makeWords
  where
    getInts = fmap read
    makeWords = fmap words

longestPath :: DAGDataPath -> IO LongestEdgePath
longestPath filepath = do
  !(!graph, !sourceNodes) <- makeDAG filepath
  return $ LongestEdgePath 0 0 -- longestEdgePath sourceNodes graph
