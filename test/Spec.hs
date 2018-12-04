import Test.Hspec

import qualified Data.List as List
import qualified Language.Haskell.Format as Format
import System.Directory

main :: IO ()
main = hspec $
  describe "Specs" $ do
    describe "Format" $
      runIO (listDirectory specsDir) >>= mapM_ testFormat . List.sort

    describe "Idempotence" $
      runIO (listDirectory specsDir) >>= mapM_ testIdempotence . List.sort


specsDir :: String
specsDir = "test/specs"

specFile :: FilePath -> FilePath -> FilePath
specFile dir file =
  specsDir ++ "/" ++ dir ++ "/" ++ file

testFormat :: FilePath -> Spec
testFormat dir =
  it dir $ do
    result <- readFile (specFile dir "output.hs")
    Format.file (specFile dir "input.hs")
      `shouldReturn` result

testIdempotence :: FilePath -> Spec
testIdempotence dir =
  it dir $ do
    result <- readFile (specFile dir "output.hs")
    Format.file (specFile dir "output.hs")
      `shouldReturn` result
