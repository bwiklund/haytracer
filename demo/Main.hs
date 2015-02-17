module Main where

import Render
import Scene
import Vector
import Sphere
import Camera

testScene = Scene [Sphere (Vector 0 0 0) 0.2]
testCamera = Camera (Vector 0 0 (-2)) (Vector 0 0 1) 0.5

main = toRawTest (renderScene testScene testCamera) "output.raw"
