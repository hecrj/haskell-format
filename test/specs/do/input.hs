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

test2 :: IO ()
test2 =
  case x of
    _ ->
      do
        one

    n -> do
      one
      two

insideLambda :: Int -> IO ()
insideLambda =
  \_ -> do
    one
    two

removeUnnecessaryDo :: IO ()
removeUnnecessaryDo = do
    one
