module CleanHousing where

import           Flow

rawHousingPath :: String
rawHousingPath = "data/housing.csv"

cleanedHousingPath :: String
cleanedHousingPath = "data/housing_cleaned.csv"

cleanHousingCSV :: IO ()
cleanHousingCSV = do
  rawData <- readFile rawHousingPath
  writeFile cleanedHousingPath (clean rawData)
  where
    clean rawData =
      rawData
        |> removeLeadingSpace
        |> fmap (\x -> if x == ' ' then ',' else x)
        |> removeCommas
        |> init
    removeLeadingSpace = unlines . fmap (drop 1) . lines
    removeCommas (',':',':xs) = removeCommas (',' : xs)
    removeCommas (x:xs)       = x : removeCommas xs
    removeCommas []           = []
