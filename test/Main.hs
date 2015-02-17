module Main (main) where

import Test.HUnit

import Vector

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
      (sqrt 14)

  ]

main = do
  _ <- runTestTT $ tests
  return ()
