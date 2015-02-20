module Color where

data Color = Color
  { red :: Double
  , green :: Double
  , blue :: Double
  } deriving (Eq, Show)

multColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

addColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

toRgbArray (Color r g b) = [r, g, b]
