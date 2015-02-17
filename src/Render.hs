module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Scene

-- stub
data RenderResult = RenderResult

-- stub
toBytes :: RenderResult -> B.ByteString
toBytes renderResult = B.concat [B.pack [255,255,255,0,0,0,0,0,0,255,255,255]]

renderScene :: Scene -> RenderResult
renderScene scene = RenderResult

toRawTest :: RenderResult -> FilePath -> IO ()
toRawTest renderResult filePath = do
  let bytes = toBytes renderResult
  B.writeFile filePath bytes
