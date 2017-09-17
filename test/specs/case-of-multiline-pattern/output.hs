module Test where

test :: Int -> IO ()
test n =
    case n of
        someLongName | someLongName == 0
                           || someLongName == 1 ->
            putStrLn "0 and 1 case"

        x | even x ->
            putStrLn "number is even"

        x | x == 3 ->
            putStrLn "3 case"

        _ ->
            putStrLn "default case"
