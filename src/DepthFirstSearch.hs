module DepthFirstSearch where

import qualified Data.Set as Set
import qualified Data.List as DL
import qualified Data.Maybe as Maybe

import Debug.Trace

data Graph = AdjList
             { edges :: [ [ NodeId ] ]
             } deriving (Eq, Show)

type NodeId = Int

edgesOfNode :: NodeId -> Graph -> Maybe [NodeId]
edgesOfNode nodeId graph =
  if nodeId >= (length . edges $ graph)
  then Nothing
  else Just $ edges graph !! nodeId

data DFSResult = DFSResult { nodeId :: NodeId
                           , startTime :: Int
                           , endTime :: Int
                           } deriving (Eq, Show)

depthFirstSearch :: Graph -> ([DFSResult], Int, Set.Set NodeId)
depthFirstSearch graph = depthFirstSearch' graph (Set.fromList [0..(length . edges $ graph) - 1]) $ 0
  where
    depthFirstSearch' graph unvisitedNodes time =
      let node = if Set.null unvisitedNodes then Nothing else Just . Set.elemAt 0 $ unvisitedNodes
      in case node of
        Nothing -> ([], time, unvisitedNodes)
        Just n ->
          let rootUnVisitedNodes = Set.delete n unvisitedNodes
              (results, endTime, moreUnVisitedNodes) = depthFirstSearchNode graph n rootUnVisitedNodes time
              -- recursively take care of connected-components
              (moreResults, endTime', moreUnVisitedNodes') = depthFirstSearch' graph moreUnVisitedNodes endTime
          in (results ++ moreResults, endTime', moreUnVisitedNodes')


depthFirstSearchNode :: Graph -> NodeId -> Set.Set NodeId -> Int -> ([DFSResult], Int, Set.Set NodeId)
depthFirstSearchNode graph nodeId unvisitedNodes time =
  let edges = edgesOfNode nodeId graph
      (result, endTime, allUnVisitedNodes) =
        Maybe.maybe ([], time + 1, unvisitedNodes) walkOverEdges edges
  in (DFSResult nodeId time endTime : result, endTime + 1, allUnVisitedNodes)

  where
    walkOverEdges edges =
      DL.foldl' (\ acc@(results, time, visitedNodesTill) edgeNodeId ->
                  if Set.member edgeNodeId $ visitedNodesTill
                  then let (moreResults, endTime, moreVisitedNodes) = depthFirstSearchNode graph edgeNodeId (Set.delete edgeNodeId visitedNodesTill) time
                       in (results ++ moreResults, endTime, moreVisitedNodes)
                  else acc)
                ([], time + 1, unvisitedNodes)
                edges
