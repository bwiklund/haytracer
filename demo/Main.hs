module Main where

import Render
import Scene
import Vector
import Shapes
import qualified Camera
import System.Random


main = do
  stdGen <- newStdGen
  let testScene = Scene [ Sphere (Vector 0 0 5) 1
                        , Sphere (Vector 2.25 0 5) 1
                        , Sphere (Vector (-2.25) 0 5) 1
                        , Sphere (Vector 0 (2.25) 5) 1
                        , Sphere (Vector 0 (-2.25) 5) 1
                        ]

      testCamera = Camera.Camera (Vector 0 0 0) (Vector 0 0 1) 0.5
      testPlateSettings = PlateSettings {width = 200, height = 200}
   in toRawTest (renderScene testScene testCamera testPlateSettings stdGen 10) "output.raw"
