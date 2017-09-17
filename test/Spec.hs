import Test.Hspec

import qualified Data.List as List
import qualified Language.Haskell.Format as Format
import System.Directory

main :: IO ()
main = hspec $
  describe "Specs" $
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
    Output <$> Format.file (specFile dir "input.hs")
      `shouldReturn` Output result

newtype Output = Output String deriving (Eq)

instance Show Output where
  show (Output s) = s
