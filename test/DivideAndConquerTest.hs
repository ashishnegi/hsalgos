module DivideAndConquerTest where

import qualified DivideAndConquer as DC

import Test.Hspec
import Test.QuickCheck
import Data.List

test = hspec $ do
  describe "kth smallest number" $ do
    it "should get kth smallest number" $ do
      property $ \k list -> do
        let kth = DC.kthSmallest k (list :: [Int])
            lenList = length list
            sortedList = sort list
        if lenList > k && k >= 0
        then kth == Just (sortedList !! k)
        else kth == Nothing
