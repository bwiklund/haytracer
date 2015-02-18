module Vector where

import System.Random

data Vector = Vector {x :: Double, y :: Double, z :: Double} deriving (Eq, Show)

add :: Vector -> Vector -> Vector
add (Vector ax ay az) (Vector bx by bz) = Vector (ax+bx) (ay+by) (az+bz)

sub :: Vector -> Vector -> Vector
sub (Vector ax ay az) (Vector bx by bz) = Vector (ax-bx) (ay-by) (az-bz)

mult :: Vector -> Double -> Vector
mult (Vector x y z) n = Vector (x*n) (y*n) (z*n)

mag :: Vector -> Double
mag v = sqrt (magSq v)

magSq :: Vector -> Double
magSq (Vector x y z) = x*x + y*y + z*z

normalize :: Vector -> Vector
normalize v = mult v (1.0 / (mag v))

dot :: Vector -> Vector -> Double
dot (Vector ax ay az) (Vector bx by bz) = ax*bx + ay*by + az*bz

-- TODO: dig up that even distribution random fn from a while ago.
randomVector :: StdGen -> Vector
randomVector stdGen =
  let (x:y:z:_) = take 3 (randoms stdGen :: [Double])
   in normalize $ (Vector (x - 0.5) (y - 0.5) (z - 0.5))

data Ray = Ray { origin :: Vector, direction :: Vector } deriving (Eq, Show)
