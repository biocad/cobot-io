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
        --  -- MMTF
        --  mmtfCodecSpec
        --  mmtfParserSpec
        --  -- Sequence
        --  weightedSequenceSpec
        --  markedSequenceSpec
        --  markedAndWeightedSequenceSpec
        --  functionsSpec
        --  -- ABI
        --  abiExtractSpec
        --  abiCleanSpec
        --  -- Uniprot
        --  uniprotSectionSpec
        --  uniprotFullSpec
         -- GB
         gbParserSpec
         gbWriterSpec
         -- Fasta
         fastaParserSpec
         fastaSpec
         fastaWriterSpec
         -- PDB
        --  pdbParserSpec
    print "AAAAAA"
    -- dottedAtomSpecP "ABS"
    testModelsSpecP "ABS"
    -- testAtomFromFile "test/PDB/testatom"
    titleSpecP "ABS"
    remarkSpecP "ABS"
    -- headerSpecP "ABS"
    print "one"
    pdbSpecP "ABS"
    print "many"
    pdbManyModelP "ABS"
    print "AAAAAA"
    print "C"
    pdbFileP "test/PDB/1igt.pdb" "test/PDB/1igt_out.pdb"
    -- pdbFileP "test/PDB/2dgc.pdb"
