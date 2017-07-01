import Test.Hspec

import qualified Language.Haskell.Format as Format
import qualified Data.List as List
import System.Directory

main :: IO ()
main = hspec $
  describe "Specifications" $
    runIO (listDirectory specsDir) >>= mapM_ testSpec . List.sort

specsDir :: String
specsDir = "test/specs"

specFile :: FilePath -> FilePath -> FilePath
specFile dir file =
  specsDir ++ "/" ++ dir ++ "/" ++ file


testSpec :: FilePath -> Spec
testSpec dir =
  it dir $ do
    result <- readFile (specFile dir "output.hs")
    Format.file (specFile dir "input.hs") `shouldReturn` result
