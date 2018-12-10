module Test where


test ::
    Int
    -> Int
    -> Either String (IO ())
test a b =
    Right $ print $ a + b
