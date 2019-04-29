import           ABISpec
import           MMTFSpec
import           UniprotSpec
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
         -- Uniprot
         uniprotSectionSpec
         uniprotFullSpec
