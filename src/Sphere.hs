module Sphere where

import Vector

data Sphere = Sphere { position :: Vector, radius :: Float }

intersectDistance :: Sphere -> Ray -> Maybe Double
intersectDistance (Sphere {position = p, radius = r}) (Ray {origin = rOrigin, direction = rDir}) =
  if mag rDir < 1.01 -- stub
    then Just 1.0 -- stub
    else Nothing
