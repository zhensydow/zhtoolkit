-- -----------------------------------------------------------------------------
module Main(main) where

-- -----------------------------------------------------------------------------
import System.Environment( getArgs, getProgName )
import System.Console.GetOpt(
  ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo )

-- -----------------------------------------------------------------------------
data Options = Options { startIndex :: !Int }
             deriving( Show )

defaultOptions :: Options
defaultOptions = Options 0

-- -----------------------------------------------------------------------------
options :: [OptDescr (Options -> Either String Options)]
options = [ Option "n" []
            (ReqArg setStartIndex "N") "start index of date"]

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
                     Right opts -> putStrLn . show $ opts
                     Left err -> error $ err ++ usageInfo h options
    (_, _, zs) -> error $ concat zs ++ usageInfo h options

-- -----------------------------------------------------------------------------
ebind :: Either String Options -> (Options -> Either String Options)
         -> Either String Options
ebind x f = case x of
  Right opts -> f opts
  err -> err

-- -----------------------------------------------------------------------------
setStartIndex :: String -> Options -> Either String Options
setStartIndex n opts = case readMaybe n of
  Just num
    | num < 0 -> Left $ "Error, index must be >= 0\n"
    | otherwise -> Right $ opts { startIndex = num }
  _ -> Left $ "Error, invalid number syntax: " ++ n ++ "\n"

-- -----------------------------------------------------------------------------
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- -----------------------------------------------------------------------------
