import           ABISpec
import           GBParserSpec
import           GBWriterSpec
import           MMTFSpec
import           SequenceSpec
import           System.IO
import           Test.Hspec
import           UniprotSpec
import           FastaParserSpec
import           FastaWriterSpec
import           FASTASpec
import           PlasmidDesignerSpec

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
         -- Fasta
         fastaParserSpec
         fastaSpec
         fastaWriterSpec
         --PlasmidDesigner
         plasmidDesignerSpec
