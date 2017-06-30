import Test.Hspec

import Language.Haskell.Format as Format
import System.Directory
import System.IO

main :: IO ()
main = hspec $
  describe "Specifications" $
    runIO (listDirectory specsDir) >>= mapM_ testSpec

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
