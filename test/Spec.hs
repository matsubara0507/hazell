import qualified Spec.Bazel.Build as Bazel.Build
import qualified Spec.Bazel.Parser as Bazel.Parser
import qualified Spec.Bazel.Rule as Bazel.Rule
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Bazel" 
  [ Bazel.Rule.tests
  , Bazel.Build.tests
  , Bazel.Parser.tests
  ]
