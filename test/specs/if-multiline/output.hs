module Test where

test :: Int -> IO ()
test n =
    if n > 10 then
        putStrLn "n > 10"
    else
        putStrLn "n <= 10"


testMultilineCondition :: Int -> IO ()
testMultilineCondition n =
    if n > 10
        && even n
    then
        putStrLn "n > 10 and even"
    else
        putStrLn "n <= 10 or odd"
