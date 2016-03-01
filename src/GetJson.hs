-- -----------------------------------------------------------------------------
module Main(main) where

-- -----------------------------------------------------------------------------
import System.Environment( getArgs, getProgName )

-- -----------------------------------------------------------------------------
printUsageInfo :: IO ()
printUsageInfo = do
  prog <- getProgName
  putStrLn $ "Usage: " ++ prog ++ " name file"
  putStrLn ""
  putStrLn " name"
  putStrLn "    Path to a json element. E.g: values[0].name"
  putStrLn " file"
  putStrLn "    json file"

-- -----------------------------------------------------------------------------
runGetJson :: String -> String -> IO ()
runGetJson path filename = do
  putStrLn $ "path = " ++ path
  putStrLn $ "file = " ++ filename

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then printUsageInfo
  else runGetJson (args!!0) (args!!1)

-- -----------------------------------------------------------------------------
