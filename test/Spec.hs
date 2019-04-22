import           ABISpec
import           MMTFSpec
import           System.IO
import           Test.Hspec

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hspec $ do
         -- MMTF
         mmtfCodecSpec
         mmtfParserSpec
         -- ABI
         abiExtractSpec
         abiCleanSpec
