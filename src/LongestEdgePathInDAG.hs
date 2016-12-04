{-# LANGUAGE BangPatterns #-}
module LongestEdgePathInDAG where

import Data.Map.Strict as Map
import Data.List as DL
import Data.Set as Set
import Data.Maybe as Maybe
import Debug.Trace
import Control.DeepSeq

type OutGoingEdges = Map.Map NodeId [ NodeId ]
type NodesData = Map.Map NodeId Node

data DAG = AdjList
           { outGoingEdges :: !OutGoingEdges
           , nodesData :: !NodesData
           } deriving (Eq, Show)

instance NFData DAG where
  rnf dag = rnf (outGoingEdges dag) `seq` rnf (nodesData dag)

data Node = Node
            { heightOfNode :: !Int
            } deriving (Eq, Show)

instance NFData Node where
  rnf node = rnf (heightOfNode node)

type NodeId = Int
type DAGDataPath = String
type SourceNodes = [NodeId]

data LongestEdgePath = LongestEdgePath
                       { pathLength :: !Int
                       , verticalDrop :: !Int
                       , path :: ![NodeId]
                       } deriving (Show)

-- ignore path in LongestEdgePath for Eq and Ord ; path is just metadata.
instance Eq LongestEdgePath where
  (==) a b = (pathLength a, verticalDrop a) == (pathLength b, verticalDrop b)
instance Ord LongestEdgePath where
  (<=) a b = (pathLength a, verticalDrop a) <= (pathLength b, verticalDrop b)

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
  traceShow "in longestEdgePath" $ DL.foldl' longestPath' (LongestEdgePath 0 0 []) sourceNodes
  where
    longestPath' longPathTillNow nodeId =
      let nodeLongPath = longestPath nodeId
      in max nodeLongPath longPathTillNow

    longestPath nodeId =
      -- this is dfs from this node
      -- since this is a dag, we do not need to take care of unvisited nodes.
      let (l, lowestVal) = depthFromNode nodeId graph
      in l { verticalDrop = heightOfDagNode nodeId - lowestVal }

    depthFromNode nodeId graph = DL.foldl' (goInDepth nodeId) (LongestEdgePath 1 0 [nodeId], heightOfDagNode nodeId) $ edgesOfNode nodeId graph

    heightOfDagNode nodeId = heightOfNode . nodeData nodeId $ graph

    goInDepth nodeId pathTillNow@(LongestEdgePath !l _ _, lowestVal) nextNodeId =
      let (newPath, lowVal) = depthFromNode nextNodeId graph
          newLen = 1 + (pathLength newPath)
          retPath = newPath { pathLength = newLen
                            , path = (nodeId : (path newPath))
                            }
      in case compare newLen l of
           LT -> pathTillNow
           GT -> ( retPath, lowVal )
           EQ -> if lowVal < lowestVal
                 then (retPath, lowVal )
                 else pathTillNow

makeEdge :: NodeId -> NodeId -> DAG -> DAG
makeEdge fromId toId graph =
  let existingEdges = Map.findWithDefault [] fromId (outGoingEdges graph)
      newEdges = toId : existingEdges
  in graph { outGoingEdges = Map.insert fromId newEdges (outGoingEdges graph) }

-- SourceNodes are nodes that have no incoming edges.
makeDAG :: DAGDataPath -> IO (DAG, SourceNodes, Int, Int)
makeDAG filepath = do
  listOfListOfInts <- makeInteger <$> readLines filepath
  let ([width, height] : mainData') = listOfListOfInts
      numNodes = width * height
      -- take nRows as height and nCols as width
      mainData = fmap (take width) . take height $ mainData'
      rows = (replicate width 1501) : mainData ++ [(replicate width 1501)]
      heightsWithNodeIdsRows = force $ fmap (\ (row, rowId) -> fmap (\ (height, colId) -> (height, rowId * width + colId)) $ zip row [0..]) $ zip rows [0..]
      threeRowsInOneGo = zip3 heightsWithNodeIdsRows (drop 1 heightsWithNodeIdsRows) (drop 2 heightsWithNodeIdsRows)
      allData = fmap makeGraph threeRowsInOneGo
      allEdges = fmap (\(a, _, _) -> a) allData
      nodesWithInEdges = fmap (\(_, b, _) -> b) allData
      allNodes = fmap (\(_,_,c) -> c) allData
      graph = AdjList (Map.unions (fmap Map.fromList allEdges)) (Map.unions (fmap Map.fromList allNodes))
      sourceNodes = Set.difference (Set.fromList . Map.keys . nodesData $ graph) (Set.unions nodesWithInEdges)

  traceShow "before returning makeDAG" $ return ""
  return $ graph `seq` sourceNodes `seq` (graph, Set.toList sourceNodes, width, height)

  where
    makeGraph (prevRow, row, nextRow) =
      let updownEdges = zip3 prevRow row nextRow
          (edges', nodesInEdges', _) = addEdges updownEdges
          leftRightEdges = zip3 ((1501, 0) : row) row ((drop 1 row) ++ [(1501,0)])
          (edges'', nodesInEdges'', allNodes') = addEdges leftRightEdges
          allEdges = force $ zipWith (\(k, a) (_, b) -> (k, a ++ b)) edges' edges''
          allNodesInEdges = force $ Set.union (Set.fromList nodesInEdges') (Set.fromList nodesInEdges'')
          allNodes = force $ allNodes'
      in allEdges `seq` allNodesInEdges `seq` allNodes `seq` (allEdges, allNodesInEdges, allNodes)

    addEdges edges =
      DL.foldl' (\ (!g', !n', !allNodes') ((pH, pId), (cH, cId), (nH, nId)) ->
                  let (g'', n'') = if cH > pH
                                   then ([pId], pId : n')
                                   else ([], n')
                      (g''', n''') = if cH > nH
                                     then (nId : g'', nId : n'')
                                     else (g'', n'')
                  in ((cId, g''') : g', n''', (cId, Node cH) : allNodes'))
                ([], [], [])
                edges

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [[Int]]
makeInteger = fmap getInts . makeWords
  where
    getInts = fmap read
    makeWords = fmap words

longestPath :: DAGDataPath -> IO LongestEdgePath
longestPath filepath = do
  (graph, sourceNodes, width, height) <- makeDAG filepath
  let l = longestEdgePath sourceNodes graph
      p = traceShow "after longestEdgePath line: "  $ reverse $ path l
      coords = fmap (flip quotRem width) . fmap (flip (-) width) $ p
      cs = fmap show coords
      heights = fmap (heightOfNode . flip nodeData graph) p
      isValid = isValidPath p graph (width, height)
      graphNodes = fmap (flip edgesOfNode graph) p
  return $ traceShow ("in longestPath: ", cs, heights, isValid) l

isValidPath :: [NodeId] -> DAG -> (Int, Int) -> Bool
isValidPath path graph (width, height) =
  let heights = fmap (heightOfNode . flip nodeData graph) path
      incOrderHeights = any (\(a,b) -> a <= b) $ zip heights (drop 1 heights)
      validTransitions = all (isValidTransition width) $ zip path (drop 1 path)
  in incOrderHeights && validTransitions
  where
    isValidTransition width (nodeId1, nodeId2) =
      case nodeId1 - nodeId2 of
        1 -> nodeId1 `mod` width /= 0 -- left
        -1 -> nodeId1 `mod` width /= (width - 1) -- right
        w -> (w == -width) || (w == width) --up/down
