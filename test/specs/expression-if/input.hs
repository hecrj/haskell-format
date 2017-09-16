module Test where

test :: Int -> IO ()
test n = if n > 10 then putStrLn "n > 10" else putStrLn "n <= 10"
