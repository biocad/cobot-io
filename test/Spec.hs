import           ABISpec
import           GBParserSpec
import           GBWriterSpec
import           MMTFSpec
import           SequenceSpec
import           System.IO
import           Test.Hspec
import           UniprotSpec

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hspec $ do
         -- MMTF
         mmtfCodecSpec
         mmtfParserSpec
         -- Sequence
         weightedSequenceSpec
         markedSequenceSpec
         markedAndWeightedSequenceSpec
         functionsSpec
         -- ABI
         abiExtractSpec
         abiCleanSpec
         -- Uniprot
         uniprotSectionSpec
         uniprotFullSpec
         -- GB
         gbParserSpec
         gbWriterSpec
