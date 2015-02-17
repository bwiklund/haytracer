module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Vector

data Camera = Camera {
  position :: Vector,
  direction :: Vector,
  zoom :: Float
}

cameraRaysForPlate :: Camera -> PlateSettings -> [Ray]
cameraRaysForPlate camera plateSettings = replicate ((width plateSettings) * (height plateSettings)) (Ray (position camera) (direction camera))

-- move me?
toPixel :: Ray -> [Double]
toPixel ray = [1.0,0.0,1.0]

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
   in Plate plateSettings (concat (map toPixel rays))--(BC.concat (B.pack (map toPixel rays)))

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
