module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Camera

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
  Plate plateSettings (replicate ((width plateSettings) * (height plateSettings) * 3) 0.5)

toRawTest :: Plate -> FilePath -> IO ()
toRawTest plate filePath = do
  let bytes = toBytes plate
  B.writeFile filePath bytes
