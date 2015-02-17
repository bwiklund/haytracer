module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene
import Camera

data RenderResult = RenderResult {
  width :: Int,
  height :: Int,
  buffer :: [Double]
}

toBytes :: RenderResult -> B.ByteString
toBytes renderResult = B.pack (map (floor . (*255)) $ buffer renderResult)

-- stub
renderScene :: Scene -> Camera -> RenderResult
renderScene scene camera = RenderResult 2 2 [1,0,1,0,1,0,1,0,1,0,1,0]

toRawTest :: RenderResult -> FilePath -> IO ()
toRawTest renderResult filePath = do
  let bytes = toBytes renderResult
  B.writeFile filePath bytes
