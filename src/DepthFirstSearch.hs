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
depthFirstSearch graph = depthFirstSearch' graph Set.empty $ 0
                         
  where
    depthFirstSearch' graph visitedNodes time =
      let node = unvisitedNode graph visitedNodes          
      in case traceShow node node of
        Nothing -> ([], time, visitedNodes)
        Just n ->
          let rootVisitedNodes = Set.insert n visitedNodes
              (results, endTime, moreVisitedNodes) = depthFirstSearchNode graph n rootVisitedNodes time
              -- recursively take care of connected-components
              (moreResults, endTime', moreVisitedNodes') = depthFirstSearch' graph moreVisitedNodes endTime
          in (results ++ moreResults, endTime', moreVisitedNodes')


depthFirstSearchNode :: Graph -> NodeId -> Set.Set NodeId -> Int -> ([DFSResult], Int, Set.Set NodeId)
depthFirstSearchNode graph nodeId visitedNodes time =
  let edges = edgesOfNode nodeId graph
      (result, endTime, allVisitedNodes) =
        Maybe.maybe ([], time + 1, visitedNodes) walkOverEdges edges
  in traceShow result (DFSResult nodeId time endTime : result, endTime + 1, allVisitedNodes)

  where
    walkOverEdges edges =
      DL.foldl' (\ acc@(results, time, visitedNodesTill) edgeNodeId ->
                  if not . Set.member edgeNodeId $ visitedNodesTill
                  then let (moreResults, endTime, moreVisitedNodes) = depthFirstSearchNode graph edgeNodeId (Set.insert edgeNodeId visitedNodesTill) time
                       in (results ++ moreResults, endTime, moreVisitedNodes)
                  else acc)
                ([], time + 1, visitedNodes)
                edges

unvisitedNode :: Graph -> Set.Set NodeId -> Maybe NodeId
unvisitedNode graph visitedNodes =
  DL.foldr (\ nodeId resNodeId ->
            if not . Set.member nodeId $ visitedNodes
            then Just nodeId
            else resNodeId)
           Nothing
           [0 .. (length . edges $ graph) - 1]
