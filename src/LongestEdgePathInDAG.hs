module LongestEdgePathInDAG where

import Data.Map.Strict as Map
import Data.List as DL
import Data.Set as Set
import Data.Maybe as Maybe

type OutGoingEdges = Map.Map NodeId [ NodeId ]
type NodesData = Map.Map NodeId Node

data DAG = AdjList
           { outGoingEdges :: OutGoingEdges
           , nodesData :: NodesData
           } deriving (Eq, Show)

data Node = Node
            { nodeId :: NodeId
            , heightOfNode :: Int
            } deriving (Eq, Show)

type NodeId = Int
type DAGDataPath = String
type SourceNodes = [NodeId]

data LongestEdgePath = LongestEdgePath
                       { pathLength :: Int
                       , verticalDrop :: Int
                       } deriving (Eq, Ord, Show)

edgesOfNode :: NodeId -> DAG -> [NodeId]
edgesOfNode nodeId graph = (outGoingEdges graph) ! nodeId

nodeData :: NodeId -> DAG -> Node
nodeData nodeId graph = (nodesData graph) ! nodeId

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
      let deepestPath = depthFromNode nodeId graph
          heightOfDagNode nodeId = heightOfNode . nodeData nodeId $ graph
      in LongestEdgePath (length deepestPath) (heightOfDagNode (head deepestPath) - heightOfDagNode (last deepestPath))

    depthFromNode nodeId graph = DL.foldl' goInDepth [nodeId] $ edgesOfNode nodeId graph

    goInDepth pathTillNow nextNodeId =
      let newPath = depthFromNode nextNodeId graph
      in if length newPath > length pathTillNow -- calculating length again is costly.
         then newPath
         else pathTillNow

-- SourceNodes are nodes that have no incoming edges.
makeDAG :: DAGDataPath -> IO (DAG, SourceNodes)
makeDAG filePath = return $ (AdjList Map.empty Map.empty, [])
