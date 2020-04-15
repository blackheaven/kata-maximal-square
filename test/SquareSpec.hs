module SquareSpec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import Square

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "maximalSquare" $ do
    it "no square should be zero" $ do
      maximalSquare ["00"
                   , "00"] `shouldBe` 0
      maximalSquare ["00101"
                   , "00110"] `shouldBe` 1
    it "no square should be zero" $ do
      maximalSquare ["00111"
                   , "00110"] `shouldBe` 4
    it "no square should be zero" $ do
      maximalSquare ["00111"
                   , "00111"
                   , "00111"] `shouldBe` 9
    -- it "is idempotent" $ property $
    --   \str -> strip str === strip (strip str)
  describe "extractPresentCoordinates" $ do
    it "one line should be indexed" $ do
      extractPresentCoordinates ["001"] `shouldBe` S.fromList [(0,2)]
  describe "indexLines" $ do
    it "one line should be indexed" $ do
      indexLines ["001"] `shouldBe` [((0,0), False), ((0,1), False), ((0,2), True)]
  describe "indexColumns" $ do
    it "columns should be indexed" $ do
      indexColumns "001" `shouldBe` [(0, False), (1, False), (2, True)]
  describe "neighbours" $ do
    it "some coordinates have three neighbours" $ do
      neighbours (4, 10) `shouldBe` [(5, 10), (5, 11), (4,11)]
    it "some coordinates have three neighbours" $ do
      neighbours (10, 4) `shouldBe` [(11, 4), (11, 5), (10,5)]
  describe "buildPresentNeighbours" $ do
    it "only one should remain" $ do
      buildPresentNeighbours (S.fromList [(0,0),(0,1),(1,0),(1,1)]) `shouldBe` S.fromList [(0,0)]
  describe "sizeSquare" $ do
    it "only first level" $ do
      sizeSquare (0,0) (S.fromList [(0,0)]) `shouldBe` 4
    it "two levels level" $ do
      sizeSquare (0,0) (S.fromList [(0,0),(0,1),(1,0),(1,1)]) `shouldBe` 9