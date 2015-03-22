module Main (main) where

import System.Environment
import Server


main :: IO()
main = do
    [portStr] <- getArgs
    let port = read portStr :: Int
    runServer port
