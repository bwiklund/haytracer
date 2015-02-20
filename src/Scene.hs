module Scene where

import Shapes

data Scene = Scene
  { objects :: [Shape]
  } deriving (Eq, Show)
