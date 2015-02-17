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
      (Vector (-1) (-2) (-3))

  ]

main = do
  _ <- runTestTT $ tests
  return ()
