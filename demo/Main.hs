module Main where

import Render
import Scene

main = toRawTest (renderScene Scene) "output.raw"
