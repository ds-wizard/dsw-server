module DSW.SHACL.Transformation
  ( shaclToEvents
  ) where

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.UTF8 (toString)
import GHC.IO.Encoding
import System.Exit
import System.Process.ByteString

shaclToEvents :: BS.ByteString -> IO (Either String BS.ByteString)
shaclToEvents = perform "python3" ["python/shacl2events.py"]

-- ----------------------------------------------------
-- PRIVATE
-- ----------------------------------------------------
type Command = BS.ByteString -> IO (Either String BS.ByteString)

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
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args (BS.toStrict input)
  case exitCode of
    ExitSuccess -> return . Right . BS.fromStrict $ stdout
    _ -> return . Left $ (show exitCode) <> ": " <> toString stderr
