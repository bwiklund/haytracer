module Main where

import Render
import Scene
import Vector
import Sphere
import qualified Camera
import System.Random


main = do
  stdGen <- newStdGen
  let testScene = Scene [Sphere (Vector 0 0 5) 1]
      testCamera = Camera.Camera (Vector 0 0 0) (Vector 0 0 1) 0.5
      testPlateSettings = PlateSettings {width = 200, height = 200}
   in toRawTest (renderScene testScene testCamera testPlateSettings stdGen) "output.raw"
