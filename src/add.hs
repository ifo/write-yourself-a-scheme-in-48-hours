module Main where
import System.Environment

main =
  getArgs >>=
  \args -> putStrLn $ show $ read (args !! 0) + read (args !! 1)
