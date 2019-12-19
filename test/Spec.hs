import           ABISpec
import           FastaParserSpec
import           FASTASpec
import           FastaWriterSpec
import           GBParserSpec
import           GBWriterSpec
import           MAEParserSpec
import           MAESpec
import           MMTFSpec
import           SequenceSpec
import           System.IO
import           Test.Hspec
import           UniprotSpec
import           PDBSpec

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
         -- Mae
         maeParserSpec
         maeSpec
         -- PDB
         oneModelSpecP
         manyModelsSpecP
         noModelsSpecP
         allFieldsModelSpecP
         emptySpecP
         trashBetweenModelsSpecP
         onlyOneModelSpecP
         repeatedStringsSpecP
         emptyRemarkSpecP
         emptyModelSpecP
