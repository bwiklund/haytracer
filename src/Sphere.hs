module Sphere where

import Control.Monad (mfilter)
import Data.Maybe
import Data.List

import Vector

data Sphere = Sphere
  { position :: Vector
  , radius :: Double
  } deriving (Eq, Show)

type Collision = (Sphere, Double)

-- Reference: http://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
-- NOTE: this can return negative results. use forwardIntersectDistance for raytracing
intersectDistance :: Sphere -> Ray -> Maybe Double
intersectDistance (Sphere {position = p, radius = r}) (Ray {origin = rOrigin, direction = rDir}) =
  let offset = p `sub` rOrigin
      lDotC = (normalize rDir) `dot` offset
      cSquared = offset `dot` offset
      underSqrt = lDotC * lDotC - cSquared + (r * r) -- TODO: will ghc memoize r*r?
   in case underSqrt of
     0 -> Just lDotC
     _ -> if underSqrt < 0
            then Nothing
            else let under = sqrt underSqrt
                     sol1 = lDotC + under
                     sol2 = lDotC - under
                  in if sol1 > 0 && sol2 > 0
                       then Just $ min sol1 sol2
                       else Just $ max sol1 sol2

-- this is what we actually care about
forwardIntersectDistance :: Sphere -> Ray -> Maybe Double
forwardIntersectDistance sphere ray =
  mfilter (> 0) (intersectDistance sphere ray)

closestForwardIntersection :: [Sphere] -> Ray -> Maybe Collision
closestForwardIntersection objects ray =
  let collisions = mapMaybe (\object -> fmap (\dist -> (object, dist)) (forwardIntersectDistance object ray)) objects
      collisionsSorted = sortBy (\a b -> compare (snd a) (snd b)) collisions
   in listToMaybe collisionsSorted
