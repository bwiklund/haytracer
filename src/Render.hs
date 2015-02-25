module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Vector
import Shapes
import qualified Camera
import System.Random
import Color

data Photon = Photon
  { color :: Color
  , ray :: Ray
  , bounces :: Int
  } deriving (Eq, Show)

data PlateSettings = PlateSettings
  { width :: Int
  , height :: Int
  } deriving (Eq, Show)

data Plate = Plate
  { settings :: PlateSettings
  , pixels :: [Color]
  } deriving (Eq, Show)

newPhotonFromRay ray = Photon (Color 1 1 1) ray 0

-- TODO: actually respect dir instead of seding out width 0 0 1 as center of screen
-- the final `Double` is a coef to tell how much to randomly scatter the rays.
-- it's basically there so we can pass in fooStdGen and 0 for the test suite for simplicity
cameraRaysForPlate :: Camera.Camera -> PlateSettings -> StdGen -> Double -> [Ray]
cameraRaysForPlate (Camera.Camera pos dir zoom) (PlateSettings w h) stdGen aaCoef =
  let dw = fromIntegral w
      dh = fromIntegral h
      rayForPixel i j =
        let x = (((i + 0.5)/dw * 2.0 - 1.0) * zoom)
            y = (((j + 0.5)/dh * 2.0 - 1.0) * zoom)
            z = 1.0
         in Ray pos (Vector x y z)
   in [rayForPixel i j | i <- [0.0..dw-1.0], j <- [0.0..dh-1.0]]

photonCast :: Scene -> StdGen -> Photon -> Photon
photonCast scene stdGen photon@(Photon pColor (Ray pRayOrigin pRayDirection) pBounces) =
  let mIntersect = closestForwardIntersection (objects scene) (ray photon)
   in case mIntersect of
     Nothing -> photon -- we're done
     Just (object, dist) ->
       let diffuse = 0.97
           surfaceOffset = 0.0001 -- so rounding errors don't let photons penetrate
           nextColor = multColor (Color diffuse diffuse diffuse) pColor
           offsetVector = (normalize pRayDirection) `mult` (dist - surfaceOffset)
           newPosition = pRayOrigin `add` offsetVector
           bounceStdGen = snd $ (random stdGen :: (Int, StdGen))
           nextStdGen = snd $ next stdGen
           nextDirection = randomOnSphere bounceStdGen
           nextBounceNum = pBounces + 1
           nextRay = Ray newPosition nextDirection
           nextPhoton = Photon nextColor nextRay nextBounceNum
        in if nextBounceNum > 2
             then nextPhoton
             else photonCast scene nextStdGen nextPhoton

toBytes :: Plate -> B.ByteString
toBytes plate = let doubleToByteClamped = (min 255) . (max 0) . floor . (*255)
                    asDoubles = concat $ map toRgbArray $ pixels plate
                 in B.pack $ map doubleToByteClamped asDoubles

-- stub
applyEnvironmentLight :: Photon -> Photon
applyEnvironmentLight photon =
  let lightLevel = if (x (direction (ray photon))) < 0 then 1 else 0
      lightColor = Color lightLevel lightLevel lightLevel
      newColor = multColor (color photon) lightColor
   in Photon newColor (ray photon) (bounces photon)

-- TODO: check dimensions
addPlates :: Plate -> Plate -> Plate
addPlates p1 p2 =
  let colorsAdded = zipWith addColor (pixels p1) (pixels p2)
   in Plate (settings p1) colorsAdded

multiplyPlate :: Plate -> Double -> Plate
multiplyPlate plate@(Plate platesettings pixels) exposure =
  let color = Color exposure exposure exposure
      newPixels = map (multColor color) pixels
   in Plate platesettings newPixels

renderScene :: Scene -> Camera.Camera -> PlateSettings -> StdGen -> Int -> Plate
renderScene scene camera plateSettings stdGen numPasses =
  let passStdGens = take numPasses (map mkStdGen $ (randoms stdGen :: [Int]))
      passes = map (renderPass scene camera plateSettings) passStdGens -- todo: use fold and accumulate because memory
      summed = foldl1 addPlates passes
   in multiplyPlate summed (1/(fromIntegral numPasses))

renderPass :: Scene -> Camera.Camera -> PlateSettings -> StdGen -> Plate
renderPass scene camera plateSettings stdGen =
  let rays = cameraRaysForPlate camera plateSettings cameraRayStdGen 1
      photons = map newPhotonFromRay rays
      -- each photon needs it's OWN seeded RNG if this is to run in parallel and without IO
      stdGens = map mkStdGen $ (randoms stdGen :: [Int])
      cameraRayStdGen = stdGen -- for now
      photonResults = zipWith (\stdGen' photon -> photonCast scene stdGen' photon) stdGens photons
      envLitPhotons = map applyEnvironmentLight photonResults
      colors = map color envLitPhotons
   in Plate plateSettings colors

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
