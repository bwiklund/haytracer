module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Vector
import Sphere

data Camera = Camera {
  position :: Vector,
  direction :: Vector,
  zoom :: Double
}

data Color = Color Double Double Double

toRgbArray :: Color -> [Double]
toRgbArray (Color r g b) = [r, g, b]

data RayCastResult = RayCastResult {
  ray :: Ray,
  color :: Color
}

-- TODO: actually respect dir instead of seding out width 0 0 1 as center of screen
cameraRaysForPlate :: Camera -> PlateSettings -> [Ray]
cameraRaysForPlate (Camera pos dir zoom) (PlateSettings w h) =
  let dw = fromIntegral w
      dh = fromIntegral h
      rayForPixel i j = Ray pos (Vector ((i/dw - 0.5) * zoom) ((j/dh - 0.5) * zoom) 1.0)
   in [rayForPixel i j | i <- [0.0..dw-1.0], j <- [0.0..dh-1.0]]

rayCast :: Scene -> Ray -> RayCastResult
rayCast scene ray@(Ray _ (Vector x y z)) =
  let mIntersect = intersectDistance (head $ objects scene ) ray
   in case mIntersect of
     Nothing -> RayCastResult ray (Color 0 0 0)
     Just dist -> RayCastResult ray (Color 1 1 1)

data PlateSettings = PlateSettings { width :: Int, height :: Int }

data Plate = Plate {
  settings :: PlateSettings,
  pixels :: [Color]
}

toBytes :: Plate -> B.ByteString
toBytes plate = B.pack (map (floor . (*255)) (concat $ map toRgbArray $ pixels plate))

-- stub
renderScene :: Scene -> Camera -> PlateSettings -> Plate
renderScene scene camera plateSettings =
  let rays = cameraRaysForPlate camera plateSettings
      results = map (rayCast scene) rays
      colors = map color results
   in Plate plateSettings colors

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
