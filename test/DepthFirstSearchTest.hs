module DepthFirstSearchTest where

import qualified DepthFirstSearch as DFS
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import Debug.Trace

makeGraph :: DFS.Graph
makeGraph = DFS.AdjList [ [1, 2] -- 0
                  , [0, 3] -- 1
                  , [0] -- 2
                  , [1, 4] -- 3
                  , [3] -- 4
                  , [] -- 5
                  ]

test = hspec $ do
  describe "Dfs traversal" $ do
    it "should have right start/endtime for dfs traversal" $ do
      let results = DFS.depthFirstSearch makeGraph
      ([], 0, Set.empty) == traceShow results results
