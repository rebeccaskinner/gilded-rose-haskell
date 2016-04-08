module GildedRose where

import Data.Char
import Data.List
import Data.String.Utils

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

data GildedItem = GildedItem { gildedName :: String,
                               gildedSellIn :: Int,
                               gildedQuality :: Int
                             } deriving (Eq, Show)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

checkConjured :: String -> Bool
checkConjured = ("conjured" `isPrefixOf`) . (map toLower)

checkConjuredItem :: GildedItem -> Bool
checkConjuredItem = checkConjured . gildedName

unconjure :: String -> String
unconjure name =
    let clen = length "Conjured" in
    lstrip $ if "conjured" `isPrefixOf` (map toLower name) then drop clen name else name

unconjureItem :: GildedItem -> GildedItem
unconjureItem (GildedItem name sellin quality) = GildedItem (unconjure name) sellin quality

caseEq :: String -> String -> Bool
caseEq a b = (map toLower a) == (map toLower b)

sellinTick :: GildedItem -> GildedItem
sellinTick item@(GildedItem name sellin quality)
    | name `caseEq` "Sulfuras" = item
    | otherwise = GildedItem name (maximum [0, sellin - 1]) quality

qualityTick :: GildedItem -> GildedItem
qualityTick item@(GildedItem name sellin quality)
    | checkConjured name = conjuredQualityTick item
    | name `caseEq` "Sulfuras" = GildedItem name sellin 80
    | name `caseEq` "Aged Brie" = GildedItem name sellin (minimum [50, quality + 1])
    | (name `caseEq` "Backstage Passes") && (sellin <= 0) = GildedItem name sellin 0
    | (name `caseEq` "Backstage Passes") && (sellin <= 5) = GildedItem name sellin (minimum [50, quality +3])
    | (name `caseEq` "Backstage Passes") && (sellin <= 10) = GildedItem name sellin (minimum [50, quality + 2])
    | (name `caseEq` "Backstage Passes") && (sellin <= 1000) = GildedItem name sellin (minimum [50, quality + 1])
    | sellin <= 0 = GildedItem name sellin (maximum [0, quality - 2])
    | otherwise = GildedItem name sellin (quality - 1)

conjuredQualityTick :: GildedItem -> GildedItem
conjuredQualityTick item =
    let (GildedItem _ sellin' quality') = qualityTick $ qualityTick $ unconjureItem item in
    GildedItem (gildedName item) sellin' quality'

tick :: GildedItem -> GildedItem
tick = qualityTick . sellinTick

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let
        quality' =
          if name /= "Aged Brie"
             && name /= "Backstage passes to a TAFKAL80ETC concert"
          then
            if quality > 0
            then
              if name /= "Sulfuras, Hand of Ragnaros"
              then quality - 1
              else quality
            else quality
          else
            if quality < 50
            then
              quality + 1 +
                (if name == "Backstage passes to a TAFKAL80ETC concert"
                 then
                   if sellIn < 11
                   then
                     if quality < 49
                     then
                       1 + (if sellIn < 6
                            then
                              if quality < 48
                              then 1
                              else 0
                            else 0)
                     else 0
                   else 0
                 else 0)
            else quality

        sellIn' =
          if name /= "Sulfuras, Hand of Ragnaros"
          then sellIn - 1
          else sellIn
      in
        if sellIn' < 0
        then
          if name /= "Aged Brie"
          then
            if name /= "Backstage passes to a TAFKAL80ETC concert"
            then
              if quality' > 0
              then
                if name /= "Sulfuras, Hand of Ragnaros"
                then (Item name sellIn' (quality' - 1))
                else (Item name sellIn' quality')
              else (Item name sellIn' quality')
            else (Item name sellIn' (quality' - quality'))
          else
            if quality' < 50
            then (Item name sellIn' (quality' + 1))
            else (Item name sellIn' quality')
        else (Item name sellIn' quality')
