import System.IO

import DSW.SHACL.Transformation (shaclToEvents)

main :: IO ()
main = do
  inp <- getContents
  result <- shaclToEvents inp
  case result of
    (Left err) -> putStrLn err
    (Right result) -> putStrLn result
