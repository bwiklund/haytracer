module Shapes where

import Control.Monad (mfilter)
import Data.Maybe
import Data.List (sortBy)
import Data.Function (on)

import Vector

data Shape = Sphere
  { position :: Vector
  , radius :: Double
  } deriving (Eq, Show)

type Collision = (Shape, Double)

-- Reference: http://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
-- NOTE: this can return negative results. use forwardIntersectRay for raytracing
intersectRay (Sphere {position = p, radius = r}) (Ray {origin = rOrigin, direction = rDir}) =
  let offset = p `sub` rOrigin
      lDotC = (normalize rDir) `dot` offset
      cSquared = offset `dot` offset
      underSqrt = lDotC * lDotC - cSquared + r * r
   in case compare underSqrt 0 of
      LT -> Nothing
      EQ -> Just lDotC
      GT -> Just $ let under = sqrt underSqrt
                       sol1 = lDotC + under
                       sol2 = lDotC - under
                       fn = if (sol1 > 0 && sol2 > 0) then min else max
                    in fn sol1 sol2

-- this is what we actually care about
forwardIntersectRay :: Shape -> Ray -> Maybe Double
forwardIntersectRay shape ray =
  mfilter (> 0) (intersectRay shape ray)

closestForwardIntersection :: [Shape] -> Ray -> Maybe Collision
closestForwardIntersection objects ray =
  let collisions = mapMaybe (\object -> fmap (\dist -> (object, dist)) (forwardIntersectRay object ray)) objects
      collisionsSorted = sortBy (compare `on` snd) collisions
   in listToMaybe collisionsSorted
