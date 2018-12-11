module Test where


test :: Int -> Int -> IO ()
test a b =
    putStrLn $
        mconcat
            [ "Addition: "
            , show addition
            , ", Subtraction: "
            , show subtraction
            ]
    where
        addition =
            a + b

        subtraction =
            a - b


main :: IO ()
main =
    output 1
    where
        output 0 =
            test 0 0
        output a =
            test a 2
