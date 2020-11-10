import           ABISpec
import           FastaParserSpec
import           FASTASpec
import           FastaWriterSpec
import           GBParserSpec
import           GBWriterSpec
import           MAEParserSpec
import           MAESpec
import           MMTFSpec
import           PDBParserSpec
import           PDBSpec
import           PDBWriterSpec
import           SequenceSpec
import           StructureSpec
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
         rawPDBToModelConversionSingleChainSpec
         bondsRestoringTripeptideSpec
         bondsRestoringBiggerMoleculesSpec
         withSegmentIdentifierModelSpecP
         pdbWriterSpec
         -- Structure
         structureSpec
