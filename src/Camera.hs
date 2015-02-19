module Camera where

import Vector

data Camera = Camera
  { position :: Vector
  , direction :: Vector
  , zoom :: Double
  } deriving (Eq, Show)
