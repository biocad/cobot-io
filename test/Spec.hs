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
         -- PDB
         oneModelSpecP
         manyModelsSpecP
         noModelsSpecP
         allFieldsModelSpecP
         wrongPDBSpecP 
    -- fileSpecP "test/PDB/1igt.pdb" -- works
    -- fileSpecP "test/PDB/gromacs1.pdb" -- with added strings works
    -- fileSpecP "test/PDB/gromacs2.pdb" -- does not work
    -- fileSpecP "test/PDB/1bvl.pdb" -- works
    -- fileSpecP "test/PDB/1g0y.pdb" -- works

