module Statistics where

import qualified Data.Vector       as V
import qualified Statistics.Sample as S

data Description = Description
  { min'   :: Double
  , mean   :: Double
  , stdDev :: Double
  , max'   :: Double
  } deriving (Show)

describe :: V.Vector Double -> Description
describe xs =
  Description
  {min' = minimum xs, mean = S.mean xs, stdDev = S.stdDev xs, max' = maximum xs}
