module DSW.SHACL.Transformation
  ( shaclToEvents
  ) where

import Control.Exception
import Data.ByteString as B
import Data.ByteString.UTF8 (fromString, toString)
import GHC.IO.Encoding
import System.Exit
import System.Process.ByteString

shaclToEvents :: String -> IO (Either String String)
shaclToEvents = perform "python3" ["python/shacl2events.py"]

-- ----------------------------------------------------
-- PRIVATE
-- ----------------------------------------------------
type Command = String -> IO (Either String String)

perform :: String -> [String] -> Command
perform cmd args input =
  catch
    (performUnsafe cmd args input)
    (\e -> do
       let err = show (e :: SomeException)
       return . Left $ "Exception: " <> err)

performUnsafe :: String -> [String] -> Command
performUnsafe cmd args input = do
  setLocaleEncoding utf8
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args (fromString input)
  case exitCode of
    ExitSuccess -> return . Right . toString $ stdout
    _ -> return . Left $ (show exitCode) <> ": " <> toString stderr
