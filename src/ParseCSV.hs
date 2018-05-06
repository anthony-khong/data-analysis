module ParseCSV where

import           Text.CSV

housingPath :: String
housingPath = "data/expenditures.csv"

parse :: String -> IO CSV
parse filePath = unpackCSV <$> parseCSVFromFile filePath
  where
    unpackCSV (Left _)    = error "Parse error!"
    unpackCSV (Right csv) = csv
