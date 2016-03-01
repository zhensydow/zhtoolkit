{-# LANGUAGE FlexibleContexts #-}
-- -----------------------------------------------------------------------------
module Main(main) where

-- -----------------------------------------------------------------------------
import System.Environment( getArgs, getProgName )
import qualified Data.Aeson as A
import qualified Text.Parsec as P
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Exception( IOException, catch )
import Control.Monad( liftM )
import System.IO( hPutStrLn, stderr )

-- -----------------------------------------------------------------------------
identifier :: P.Stream s m Char => P.ParsecT s u m String
identifier = do
  c <- P.letter
  cs <- P.many (P.alphaNum P.<|> P.char '_' )
  return (c:cs)

arraySubs :: P.Stream s m Char => P.ParsecT s u m Int
arraySubs = do
  _ <- P.char '['
  _ <- P.spaces
  n <- P.many1 P.digit
  _ <- P.spaces
  _ <- P.char ']'
  return . read $ n

expresionVal :: P.Stream s m Char => A.Value -> P.ParsecT s u m A.Value
expresionVal json = do
  name <- identifier
  val <- case json of
           A.Object obj -> return $ HM.lookup (T.pack name) obj
           _ -> return Nothing
  case val of
    Just v -> return v
    _ -> P.parserFail "value not found"

arrayVal :: P.Stream s m Char => A.Value -> P.ParsecT s u m A.Value
arrayVal json = do
  idx <- arraySubs
  val <- case json of
           A.Array arr -> return $ arr V.!? idx
           _ -> return Nothing
  case val of
    Just v -> return v
    _ -> P.parserFail "value not found"

getValueParser :: P.Stream s m Char => A.Value -> P.ParsecT s u m A.Value
getValueParser json = do
  _ <- P.optional (P.char '.')
  val1 <- P.choice [P.try (expresionVal json), P.try (arrayVal json)]
  P.try (getValueParser val1) P.<|> return val1

-- -----------------------------------------------------------------------------
getValue :: A.Value -> String -> Maybe A.Value
getValue val path = case P.parse (getValueParser val) "" path of
                    Right a -> Just a
                    Left _ -> Nothing

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
filedata :: String -> IO (Maybe BS.ByteString)
filedata f = catch
             (liftM Just (BS.readFile f))
             (\e -> do
                hPutStrLn stderr ("Error: " ++ show (e :: IOException))
                return Nothing)

-- -----------------------------------------------------------------------------
jsonFromFile :: String -> IO (Maybe A.Value)
jsonFromFile f = do
  d <- filedata f
  return $ maybe Nothing A.decode d

-- -----------------------------------------------------------------------------
cleanUpValue :: Maybe A.Value -> String
cleanUpValue (Just v) = case v of
                          A.String s -> T.unpack s
                          A.Number n -> show n
                          A.Bool b -> show b
                          A.Null -> "null"
                          _ -> ""
cleanUpValue _ = ""

-- -----------------------------------------------------------------------------
runGetJson :: String -> String -> IO ()
runGetJson path filename = do
  json <- jsonFromFile filename
  putStrLn . cleanUpValue . maybe Nothing (`getValue` path) $ json


-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then printUsageInfo
  else runGetJson (head args) (args!!1)

-- -----------------------------------------------------------------------------
