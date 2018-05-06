{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module FramesTutorial where

import qualified Control.Foldl     as L
import qualified Data.Foldable     as F
import qualified Data.Vector       as V
import           Flow
import           Frames
import           Frames.CSV        (RowGen (..), readTableOpt, rowGen)
import           Lens.Micro
import           Lens.Micro.Extras
import           Pipes             hiding (Proxy)
import qualified Statistics        as S

tableTypes'
  (rowGen "data/housing_cleaned.csv")
  { rowTypeName = "House"
  , columnNames =
      [ "crime"
      , "zone"
      , "industrial"
      , "charles"
      , "nox"
      , "rooms"
      , "age"
      , "dist_to_ec"
      , "highway_accessibility"
      , "tax"
      , "pt_ratio"
      , "blacks"
      , "lower_status"
      , "median_value"
      ]
  , separator = ","
  }

houseStream :: MonadSafe m => Producer House m ()
houseStream = readTableOpt houseParser "data/housing_cleaned.csv"

loadHouse :: IO (Frame House)
loadHouse = inCoreAoS houseStream

go = do
    df <- loadHouse
    {-let out = take 5 $ F.toList df-}
    let out =
            view blacks <$> df
            |> F.toList
            |> V.fromList
            |> S.describe
    print out
