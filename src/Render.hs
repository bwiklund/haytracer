module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Vector
import Sphere
import qualified Camera

data Color = Color Double Double Double
multiply (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

toRgbArray :: Color -> [Double]
toRgbArray (Color r g b) = [r, g, b]

data Photon = Photon {
  color :: Color,
  ray :: Ray,
  bounces :: Int
}
newPhotonFromRay ray = Photon (Color 1 1 1) ray 0

-- TODO: actually respect dir instead of seding out width 0 0 1 as center of screen
cameraRaysForPlate :: Camera.Camera -> PlateSettings -> [Ray]
cameraRaysForPlate (Camera.Camera pos dir zoom) (PlateSettings w h) =
  let dw = fromIntegral w
      dh = fromIntegral h
      rayForPixel i j = let x = (((i + 0.5)/dw * 2.0 - 1.0) * zoom)
                            y = (((j + 0.5)/dh * 2.0 - 1.0) * zoom)
                            z = 1.0
                         in Ray pos (Vector x y z)
   in [rayForPixel i j | i <- [0.0..dw-1.0], j <- [0.0..dh-1.0]]

photonCast :: Scene -> Photon -> Photon
photonCast scene photon =
  let mIntersect = intersectDistance (head $ objects scene) (ray photon)
   in case mIntersect of
     Nothing -> photon -- we're done
     Just dist -> let newColor = (multiply (Color 0.5 0.5 0.5) (color photon))
                      newPosition = (origin (ray photon)) `add` (mult (normalize (direction (ray photon))) dist)
                      newDirection = (direction (ray photon)) -- todo random
                   in Photon newColor (Ray newPosition newDirection) ((bounces photon)+1)

data PlateSettings = PlateSettings { width :: Int, height :: Int }

data Plate = Plate {
  settings :: PlateSettings,
  pixels :: [Color]
}

toBytes :: Plate -> B.ByteString
toBytes plate = B.pack (map (floor . (*255)) (concat $ map toRgbArray $ pixels plate))

-- stub
renderScene :: Scene -> Camera.Camera -> PlateSettings -> Plate
renderScene scene camera plateSettings =
  let rays = cameraRaysForPlate camera plateSettings
      colors = map (color . (photonCast scene) . newPhotonFromRay) rays
   in Plate plateSettings colors

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
