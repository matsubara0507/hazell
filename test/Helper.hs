module Helper where

import           RIO

import           Prettyprinter
import           Prettyprinter.Render.String
import           Test.Tasty.HUnit

assertPrettyEqual :: Pretty a => String -> a -> Assertion
assertPrettyEqual expected actual =
  expected @=? renderString (layoutPretty defaultLayoutOptions $ pretty actual)
