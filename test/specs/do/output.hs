module Test where


main :: IO ()
main = do
    line <- getLine
    putStrLn line


test :: IO ()
test =
    doSomething $ do
        one
        two


removeUnnecessaryDo :: IO ()
removeUnnecessaryDo =
    one
