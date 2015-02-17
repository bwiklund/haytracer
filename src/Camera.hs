module Camera where

import Vector

data Camera = Camera {
  pos :: Vector,
  dir :: Vector,
  zoom :: Float
}
