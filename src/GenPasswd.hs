-- -----------------------------------------------------------------------------
module Main(main) where

-- -----------------------------------------------------------------------------
import System.Random( StdGen, getStdGen, randomRs )
import System.Environment( getArgs, getProgName )
import System.Console.GetOpt(
  ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo )

-- -----------------------------------------------------------------------------
chars :: String
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- -----------------------------------------------------------------------------
randomList :: StdGen -> String
randomList = map (chars!!) . randomRs (0, length chars - 1)

-- -----------------------------------------------------------------------------
data Options = Options { numChars :: !Int }

defaultOptions :: Options
defaultOptions = Options 8

-- -----------------------------------------------------------------------------
options :: [OptDescr (Options -> Either String Options)]
options = [ Option "n" []
            (ReqArg setNumChars "N") "number of characters"]

-- -----------------------------------------------------------------------------
header :: IO String
header = do
  prog <- getProgName
  return $ "Usage: " ++ prog ++ " [Options]"

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  h <- header
  args <- getArgs
  case getOpt Permute options args of
    (xs, _, []) -> case foldl ebind (Right defaultOptions) xs of
      Right opts -> runGenPasswd opts
      Left err -> error $ err ++ usageInfo h options
    (_, _, zs) -> error $ concat zs ++ usageInfo h options

-- -----------------------------------------------------------------------------
ebind :: Either String Options -> (Options -> Either String Options)
         -> Either String Options
ebind x f = case x of
  Right opts -> f opts
  err -> err

-- -----------------------------------------------------------------------------
runGenPasswd :: Options -> IO ()
runGenPasswd opts = getStdGen >>= putStrLn . take (numChars opts) . randomList

-- -----------------------------------------------------------------------------
setNumChars :: String -> Options -> Either String Options
setNumChars n opts = case readMaybe n of
  Just num
    | num <= 0 -> Left "Error, number of chars must be > 0\n"
    | otherwise -> Right $ opts { numChars = num }
  _ -> Left $ "Error, invalid number syntax: " ++ n ++ "\n"

-- -----------------------------------------------------------------------------
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- -----------------------------------------------------------------------------
