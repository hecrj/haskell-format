module Test where

addSomeIntegers :: Int
addSomeIntegers =
    3 + 2


testRequest :: Spec
testRequest =
    request `shouldRespondWith` 200


divideBy3 :: Int -> Int
divideBy3 =
    (/ 3)


divide3 :: Int -> Int
divide3 =
    (3 /)
