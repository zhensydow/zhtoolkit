module Main(main) where

import System.Random( StdGen, getStdGen, randomRs )

chars :: String
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

randomList :: StdGen -> String
randomList = map (chars!!) . randomRs (0, length chars - 1)

main :: IO ()
main = do
  gen <- getStdGen
  putStrLn . take 8 $ randomList gen
