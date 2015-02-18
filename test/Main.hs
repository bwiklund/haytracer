module Main (main) where

import Test.HUnit

import Vector
import Sphere
import Render
import Camera

tests = TestList [

    TestCase $ assertEqual
      "Vector addition"
      (add (Vector 1 2 3) (Vector 2 4 6))
      (Vector 3 6 9),


    TestCase $ assertEqual
      "Vector subtraction"
      (sub (Vector 1 2 3) (Vector 2 4 6))
      (Vector (-1) (-2) (-3)),


    TestCase $ assertEqual
      "Vector scalar multiplication"
      (mult (Vector 1 2 3) 2)
      (Vector 2 4 6),


    TestCase $ assertEqual
      "Vector magnitude squared"
      (magSq (Vector 1 2 3))
      14,


    TestCase $ assertEqual
      "Vector magnitude"
      (mag (Vector 1 2 3))
      (sqrt 14),


    TestCase $ assertEqual
      "Vector normalize"
      (normalize (Vector 2 0 0))
      (Vector 1 0 0),


    TestCase $ assertEqual
      "Vector dot product"
      (dot (Vector 1 1 1) (Vector 1 1 1))
      3,


    TestCase $ assertEqual
      "Color mult"
      (multColor (Color 0.5 0.5 0.5) (Color 0.5 0.5 0.5))
      (Color 0.25 0.25 0.25),


    TestCase $ assertEqual
      "Color add"
      (addColor (Color 0.5 0.5 0.5) (Color 0.5 0.5 0.5))
      (Color 1 1 1),


    TestCase $ assertEqual
      "Plate add"
      (addPlates (Plate (PlateSettings 1 1) [Color 1 1 1]) (Plate (PlateSettings 1 1) [Color 1 1 1]))
      (Plate (PlateSettings 1 1) [Color 2 2 2]),


    TestCase $ assertEqual
      "Sphere ray intersection hit"
      (intersectDistance (Sphere (Vector 0 0 0) 1) (Ray (Vector 0 0 (-2)) (Vector 0 0 1)))
      (Just 1),


    TestCase $ assertEqual
      "Sphere ray intersection miss"
      (intersectDistance (Sphere (Vector 0 0 0) 1) (Ray (Vector 0 2 (-2)) (Vector 0 0 1)))
      Nothing,


    TestCase $ assertEqual
      "Sphere ray intersection miss"
      (intersectDistance (Sphere (Vector 0 0 0) 1) (Ray (Vector 0 0 (-2)) (Vector 0 1 1)))
      Nothing,


    TestCase $ assertEqual
      "Sphere ray intersection from inside"
      (intersectDistance (Sphere (Vector 0 0 0) 0.5) (Ray (Vector 0 0 0) (Vector 0 0 1)))
      (Just 0.5),


    TestCase $ assertEqual
      "Camera ray generation 1x1"
      (cameraRaysForPlate (Camera (Vector 0 0 0) (Vector 0 0 1) 0.5) (PlateSettings 1 1))
      [(Ray (Vector 0 0 0) (Vector 0 0 1))],


    TestCase $ assertEqual
      "Camera ray generation 2x2"
      (cameraRaysForPlate (Camera (Vector 0 0 0) (Vector 0 0 1) 0.5) (PlateSettings 2 2))
      [(Ray (Vector 0 0 0) (Vector (-0.25) (-0.25) 1)),
       (Ray (Vector 0 0 0) (Vector (-0.25) (0.25) 1)),
       (Ray (Vector 0 0 0) (Vector (0.25) (-0.25) 1)),
       (Ray (Vector 0 0 0) (Vector (0.25) (0.25) 1))]


  ]

main = do
  _ <- runTestTT $ tests
  return ()
