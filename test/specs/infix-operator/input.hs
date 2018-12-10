module Test where

addSomeIntegers :: Int
addSomeIntegers = 3 + 2

testRequest :: Spec
testRequest = request `shouldRespondWith` 200
