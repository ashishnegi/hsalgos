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
      -- DFS.DFSResult nodeId startTime endTime
      ([ DFS.DFSResult 0 0 9
       , DFS.DFSResult 1 1 6
       , DFS.DFSResult 3 2 5
       , DFS.DFSResult 4 3 4
       , DFS.DFSResult 2 7 8
       , DFS.DFSResult 5 10 11
       ]
        , 12 -- final time
        , Set.empty) -- no node should be left.
        == results
