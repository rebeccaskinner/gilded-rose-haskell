module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec = do
  describe "checkConjured" $ do
    it "returns true when an item name contains lowercase 'conjured'" $ do
      (checkConjured "conjured test") `shouldBe` True

    it "returns true when an item contains camelcase 'Conjured'" $ do
      (checkConjured "Conjured Test") `shouldBe` True

    it "returnse false when an item does not contain the term 'conjured'" $ do
      (checkConjured "Coin Test") `shouldBe` False

  describe "unconjure" $ do
    it "removes the term conjured" $ do
      "Test" `shouldSatisfy` (== unconjure "Conjured Test")

  describe "sellinTick" $ do
    it "does not change the sellin date of Sulfuras" $ do
      let item = GildedItem "Sulfuras" 10 50 in
        (sellinTick item) `shouldSatisfy` (== item)

    it "Does not set the sellin date to negative" $ do
      let item = GildedItem "Item" 0 50 in
        (sellinTick item) `shouldSatisfy` (== item)

    it "Decrements the sellin date" $ do
      let sellin = 5
          item = GildedItem "Item" sellin 50 in
        (gildedSellIn $ sellinTick item) `shouldSatisfy` (== (sellin - 1))

  describe "qualityTick" $ do
    it "Degrades the quality of an item" $ do
      let quality = 10
          item = GildedItem "Item" 10 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== (quality - 1))

    it "Does not degrade the quality of Sulfuras" $ do
      let item = GildedItem "Sulfuras" 10 80 in
        (qualityTick item) `shouldSatisfy` (== item)

    it "Always makes the quality of Sulfuras 80" $ do
      let item = GildedItem "Sulfuras" 10 80
          runs = replicate 1000 qualityTick in
        (gildedQuality $ foldl (\item' func -> func item') item runs) `shouldSatisfy` (== (gildedQuality item))

    it "Increases the quality of aged brie" $ do
      let quality = 10
          item = GildedItem "Aged Brie" 10 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== (quality + 1))

    it "Increases the quality of backstage passes by 2 when there are 10 days or less to sell" $ do
      let quality = 10
          item = GildedItem "Backstage passes" 9 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== (quality + 2))

    it "Increases the quality of backstage passes by 3 when there are 5 days or less to sell" $ do
      let quality = 10
          item = GildedItem "Backstage passes" 5 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== (quality + 3))

    it "Sets the quality to zero after backstage passes are expired" $ do
      let quality = 10
          item = GildedItem "Backstage passes" 0 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== 0)

    it "Increases the quality of backstage passes by 1 when there are more than 10 days to sell" $ do
      let quality = 10
          item = GildedItem "Backstage passes" 20 quality in
        (gildedQuality $ qualityTick item) `shouldSatisfy` (== (quality + 1))

    it "Does not increase the quality of an item over 50" $ do
      let quality = 50
          brie = qualityTick $ GildedItem "Aged Brie" 3 quality
          tickets = qualityTick $ GildedItem "Backstage passes" 3 quality in
        (map gildedQuality [brie, tickets]) `shouldSatisfy` (\x -> (maximum x) <= 50)

    it "Degrades quality twice as fast for conjured items" $ do
      let item = qualityTick $ qualityTick $ GildedItem "item" 10 50
          item' = qualityTick $ GildedItem "Conjured item" 10 50 in
        (gildedQuality item) `shouldSatisfy` (== (gildedQuality item'))
