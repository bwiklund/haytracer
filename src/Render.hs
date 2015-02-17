module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Vector

data Camera = Camera {
  position :: Vector,
  direction :: Vector,
  zoom :: Double
}

-- TODO: actually respect dir instead of seding out width 0 0 1 as center of screen
cameraRaysForPlate :: Camera -> PlateSettings -> [Ray]
cameraRaysForPlate (Camera pos dir zoom) (PlateSettings w h) =
  let dw = fromIntegral w
      dh = fromIntegral h
      rayForPixel i j = Ray pos (Vector ((i/dw - 0.5) * zoom) ((j/dh - 0.5) * zoom) 1.0)
   in [rayForPixel i j | i <- [0.0..dw-1.0], j <- [0.0..dh-1.0]]

-- move me?
toPixel :: Ray -> [Double]
toPixel (Ray (Vector x1 y1 z1) (Vector x2 y2 z2)) = [x2, y2, z2]

data PlateSettings = PlateSettings { width :: Int, height :: Int }

data Plate = Plate {
  settings :: PlateSettings,
  buffer :: [Double]
}

toBytes :: Plate -> B.ByteString
toBytes plate = B.pack (map (floor . (*255)) $ buffer plate)

-- stub
renderScene :: Scene -> Camera -> PlateSettings -> Plate
renderScene scene camera plateSettings =
  let rays = cameraRaysForPlate camera plateSettings
   in Plate plateSettings (concat (map toPixel rays))

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
