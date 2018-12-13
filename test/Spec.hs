import System.Directory
import Test.Hspec
import qualified Data.List as List
import qualified Language.Haskell.Format as Format


main :: IO ()
main =
    hspec $
        describe "Specs" $ do
            describe "Format" $
                runIO (listDirectory specsDir) >>= mapM_ testFormat . List.sort

            describe "Idempotence" $
                runIO (listDirectory specsDir) >>= mapM_ testIdempotence . List.sort


specsDir :: String
specsDir =
    "test/specs"


specFile :: FilePath -> FilePath -> FilePath
specFile dir file =
    specsDir ++ "/" ++ dir ++ "/" ++ file


testFormat :: FilePath -> Spec
testFormat dir =
    it dir $ do
        result <- readFile (specFile dir "output.hs")
        Output <$> Format.file (specFile dir "input.hs") `shouldReturn` Output (Right result)


testIdempotence :: FilePath -> Spec
testIdempotence dir =
    it dir $ do
        result <- readFile (specFile dir "output.hs")
        Output <$> Format.file (specFile dir "output.hs") `shouldReturn` Output (Right result)


newtype Output
    = Output (Either String String)
    deriving Eq


instance Show Output where
    show (Output output) =
        case output of
            Left s ->
                s

            Right s ->
                s
