module Render where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

-- stub
data RenderResult = RenderResult

toRawTest :: RenderResult -> FilePath -> IO ()
toRawTest renderResult filePath = do
  let bytes = B.concat [B.pack [255,255,255,0,0,0,0,0,0,255,255,255]]
  B.writeFile filePath bytes
