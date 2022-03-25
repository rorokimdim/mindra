import Test.Tasty
import Test.Tasty.HUnit

import qualified Mindra.Diagrams.ParserTest
import qualified Mindra.Gloss.ParserTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Mindra.Diagrams.ParserTest.tests, Mindra.Gloss.ParserTest.tests]
