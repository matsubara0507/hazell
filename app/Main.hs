module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import qualified Hazell
import System.Environment (getArgs)

main :: IO ()
main = do
  path <- listToMaybe <$> getArgs
  Hazell.generate (fromMaybe "." path)
