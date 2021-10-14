{-# LANGUAGE OverloadedStrings #-}

module GBParserSpec where

import Bio.GB          (Feature (..), Form (..), GenBankSequence (..), Locus (..), Meta (..),
                        Reference (..), Source (..), Version (..), fromFile)
import Bio.GB.Parser   (rangeP)
import Bio.Sequence    (Border (..), Range (..), RangeBorder (..), preciseSpan,
                        unsafeMarkedSequence)
import Control.Lens    (_Left, over)
import Data.Text       (Text)
import Test.Hspec
import Text.Megaparsec (eof, errorBundlePretty, parse)

gbParserSpec :: Spec
gbParserSpec = describe "GenBank format parser." $ do
    rangeTests
    pAAVGFPSpecP "test/GB/pAAV-GFP-CellBioLab.gb"
    pAAVCMVSpecP "test/GB/pAAV_CMV_RPE65_PolyA_linkers.gb"
    dottedMetaSpecP "test/GB/pAAV-GFP-CellBioLab-dots.gb"
    unknownFieldsSpecP "test/GB/pIntA-TRBV.gb"
    baseCountWithSophisticatedRangesAndMultilineFeatures "test/GB/fromYanaWithLove.gb"

rangeTests :: Spec
rangeTests = describe "Range parser" $ do
    it "correctly parses a simple span" $ 
        greedyRangeP "69..420" `shouldBe` successful (Span (RangeBorder Precise 69) (RangeBorder Precise 420))
    it "correctly parses a span with the lower border exceeded" $
        greedyRangeP "<69..420" `shouldBe` successful (Span (RangeBorder Exceeded 69) (RangeBorder Precise 420))
    it "correctly parses a span with the upper border exceeded" $
        greedyRangeP "69..>420" `shouldBe` successful (Span (RangeBorder Precise 69) (RangeBorder Exceeded 420))
    it "correctly parses a span with both border exceeded" $ 
        greedyRangeP "<69..>420" `shouldBe` successful (Span (RangeBorder Exceeded 69) (RangeBorder Exceeded 420))
    it "does not parse a span with the lower border exceeded incorrectly" $ 
        greedyRangeP ">69..420" `shouldSatisfy` isFail
    it "does not parse a span with the upper border exceeded incorrectly" $ 
        greedyRangeP "69..<420" `shouldSatisfy` isFail

    it "correctly parses a 'between' statement" $ 
        greedyRangeP "41^42" `shouldBe` successful (Between 41 42)
    it "does not parse a 'between' statement witn border excession marks" $ 
        greedyRangeP "<41^42" `shouldSatisfy` isFail

    it "correctly parses a single point feature" $ 
        greedyRangeP "42" `shouldBe` successful (Point 42)
    it "does not parse a single point feature with border excession marks" $ 
        greedyRangeP "<3" `shouldSatisfy` isFail

    it "correctly parses a join() statement" $ 
        greedyRangeP "join(2,12..56)" `shouldBe` successful (Join [Point 2, Span (RangeBorder Precise 12) (RangeBorder Precise 56)])
    it "correctly parses a sophisticated join() statement" $ 
        greedyRangeP "join(2^3,<5..10,15,20..>28)" `shouldBe` successful (Join [Between 2 3, Span (RangeBorder Exceeded 5) (RangeBorder Precise 10), Point 15, Span (RangeBorder Precise 20) (RangeBorder Exceeded 28)])

    it "correctly parses a complement() statement" $ 
        greedyRangeP "complement(69..>420)" `shouldBe` successful (Complement (Span (RangeBorder Precise 69) (RangeBorder Exceeded 420)))
    it "correctly parses a join() incorporated into a complement()" $ 
        greedyRangeP "complement(join(2^3,<5..10,15,20..>28))" `shouldBe` successful (Complement (Join [Between 2 3, Span (RangeBorder Exceeded 5) (RangeBorder Precise 10), Point 15, Span (RangeBorder Precise 20) (RangeBorder Exceeded 28)]))
  where
    greedyRangeP :: Text -> Either String Range
    greedyRangeP = over _Left errorBundlePretty . parse (rangeP <* eof) ""
    
    successful :: a -> Either String a
    successful = Right

    isFail :: Either String a -> Bool
    isFail = null 

pAAVGFPSpecP :: FilePath -> Spec
pAAVGFPSpecP path = describe "pAAVGFP" $ do
    it "correctly parses meta information" $ do
        mt <- meta <$> fromFile path
        mt `shouldBe` pAAVGFPMeta

    it "correctly parses structSeq" $ do
        gbS <- gbSeq <$> fromFile path
        gbS `shouldBe` unsafeMarkedSequence pAAVGFPOrigin pAAVGFPFeatures

pAAVCMVSpecP :: FilePath -> Spec
pAAVCMVSpecP path = describe "pAAVCMV" $ do
    it "parses meta information" $ do
        mt <- meta <$> fromFile path
        mt `shouldBe` pAAVCMVMeta

    it "correctly parses structSeq" $ do
        gbS <- gbSeq <$> fromFile path
        gbS `shouldBe` unsafeMarkedSequence pAAVCMVOrigin pAAVCMVFeatures

dottedMetaSpecP :: FilePath -> Spec
dottedMetaSpecP path = describe "Meta with dots." $ do
    it "correctly parses meta information with all info set to dots" $ do
        mt <- meta <$> fromFile path
        mt `shouldBe` dottedMeta

unknownFieldsSpecP :: FilePath -> Spec
unknownFieldsSpecP path = describe "Unknown fields" $ do
    it "correctly parses and skip unknown fields before FEATURES" $ do
        mt <- meta <$> fromFile path
        name (locus mt) `shouldBe` "P2-32_pIntA-TRBV5-1_J1-1-Fc-lama-knob-EPEA"

baseCountWithSophisticatedRangesAndMultilineFeatures :: FilePath -> Spec
baseCountWithSophisticatedRangesAndMultilineFeatures path = describe "" $ do
    it "correctly parses the 'BASE COUNT' line and features with sophisticated ranges" $ do
        gbS <- gbSeq <$> fromFile path
        gbS `shouldBe`  unsafeMarkedSequence sophisticatedFeaturesSeq sophisticatedFeatures 

compPreciseSpan :: (Int, Int) -> Range
compPreciseSpan = Complement . preciseSpan

pAAVGFPMeta :: Meta
pAAVGFPMeta = Meta { locus=Locus "pAAV-GFP-CellBio" 5374 "ds-DNA" (Just Circular) (Just "SYN") "15-AUG-2016"
                   , definition=Just $ "Saccharomyces cerevisiae TCP1-beta gene, partial cds, and Axl2p\n(AXL2) and Rev7p (REV7) genes, complete cds."
                   , accession=Just $ "U49845"
                   , version=Just $ Version "U49845.1" $ Just "1293613"
                   , keywords=Just "."
                   , source=Just $ Source "synthetic DNA construct" $ Just "synthetic DNA construct\nsynthetic DNA construct hmmm interesting"
                   , references=[ Reference "1  (bases 1 to 5374)" (Just "avprok") (Just "Direct Submission") (Just "Exported Monday, August 15, 2016 from SnapGene 3.1.4 to Vector \nNTI(R) format\nhttp://www.snapgene.com") Nothing
                                , Reference "2  (bases 1 to 5028)" (Just "Roemer,T., Madden,K., Chang,J. and Snyder,M.") (Just "Selection of axial growth sites in yeast requires Axl2p, a novel\nplasma membrane glycoprotein") (Just "Genes Dev. 10 (7), 777-793 (1996)") (Just "8846915")
                                , Reference "3  (bases 1 to 5028)" (Just "Roemer,T.") (Just "Direct Submission") (Just "Submitted (22-FEB-1996) Terry Roemer, Biology, Yale University, New\nHaven, CT, USA") Nothing
                                ]
                   , comments=["VNTDATE|745891200|", "VNTDBDATE|778204800|", "VNTNAME|pAAV-GFP-CellBioLab|", "VNTAUTHORNAME|avprok|"]
                   }

pAAVGFPFeatures :: [(Feature, Range)]
pAAVGFPFeatures = [ ( (Feature "misc_feature" [ ("label", "Right ITR") ]
                                           ), preciseSpan (0, 129))
                                       , ( (Feature "enhancer" [ ("label", "CMV enhancer")
                                                               , ("note", "human cytomegalovirus immediate early enhancer")
                                                               ]
                                           ), preciseSpan (205, 508))
                                       , ( (Feature "promoter" [ ("label", "CMV promoter")
                                                               , ("note", "human cytomegalovirus (CMV) immediate early \npromoter")
                                                               ]
                                           ), preciseSpan (509, 711))
                                       , ( (Feature "misc_feature" [ ("label", "Human beta-globin Intron") ]
                                           ), preciseSpan (804, 1296))
                                       , ( (Feature "CDS" [ ("codon_start", "1")
                                                          , ("product", "enhanced GFP")
                                                          , ("label", "EGFP")
                                                          , ("note", "mammalian codon-optimized")
                                                          ]
                                           ), preciseSpan (1319, 2035))
                                       , ( (Feature "repeat_region" [ ("label", "Left ITR")
                                                                    , ("note", "inverted terminal repeat of adeno-associated virus \nserotype 2\noooooo")
                                                                    , ("prop", "1")
                                                                    ]
                                           ), compPreciseSpan (2636, 2776))
                                       ]

pAAVGFPOrigin :: String
pAAVGFPOrigin =
  "cctgcaggcagctgcgcgctcgctcgctcactgaggccgcccgggcgtcgggcgacctttggtcgcccggcctcagtgagcgagcgagcgcgcagagagggagtggccaactccatcactaggggttcctgcggccgcacgcgtctagttattaatagtaatcaattacggggtcattagttcatagcccatatatggagttccgcgttacataacttacggtaaatggcccgcctggctgaccgcccaacgacccccgcccattgacgtcaataatgacgtatgttcccatagtaacgtcaatagggactttccattgacgtcaatgggtggagtatttacggtaaactgcccacttggcagtacatcaagtgtatcatatgccaagtacgccccctattgacgtcaatgacggtaaatggcccgcctggcattatgcccagtacatgaccttatgggactttcctacttggcagtacatctacgtattagtcatcgctattaccatggtgatgcggttttggcagtacatcaatgggcgtggatagcggtttgactcacggggatttccaagtctccaccccattgacgtcaatgggagtttgttttgcaccaaaatcaacgggactttccaaaatgtcgtaacaactccgccccattgacgcaaatgggcggtaggcgtgtacggtgggaggtctatataagcagagctcgtttagtgaaccgtcagatcgcctggagacgccatccacgctgttttgacctccatagaagacaccgggaccgatccagcctccgcggattcgaatcccggccgggaacggtgcattggaacgcggattccccgtgccaagagtgacgtaagtaccgcctatagagtctataggcccacaaaaaatgctttcttcttttaatatacttttttgtttatcttatttctaatactttccctaatctctttctttcagggcaataatgatacaatgtatcatgcctctttgcaccattctaaagaataacagtgataatttctgggttaaggcaatagcaatatttctgcatataaatatttctgcatataaattgtaactgatgtaagaggtttcatattgctaatagcagctacaatccagctaccattctgcttttattttatggttgggataaggctggattattctgagtccaagctaggcccttttgctaatcatgttcatacctcttatcttcctcccacagctcctgggcaacgtgctggtctgtgtgctggcccatcactttggcaaagaattgggattcgaacatcgattgaattctgaatggtgagcaagggcgaggagctgttcaccggggtggtgcccatcctggtcgagctggacggcgacgtaaacggccacaagttcagcgtgtccggcgagggcgagggcgatgccacctacggcaagctgaccctgaagttcatctgcaccaccggcaagctgcccgtgccctggcccaccctcgtgaccaccctgacctacggcgtgcagtgcttcagccgctaccccgaccacatgaagcagcacgacttcttcaagtccgccatgcccgaaggctacgtccaggagcgcaccatcttcttcaaggacgacggcaactacaagacccgcgccgaggtgaagttcgagggcgacaccctggtgaaccgcatcgagctgaagggcatcgacttcaaggaggacggcaacatcctggggcacaagctggagtacaactacaacagccacaacgtctatatcatggccgacaagcagaagaacggcatcaaggtgaacttcaagatccgccacaacatcgaggacggcagcgtgcagctcgccgaccactaccagcagaacacccccatcggcgacggccccgtgctgctgcccgacaaccactacctgagcacccagtccgccctgagcaaagaccccaacgagaagcgcgatcacatggtcctgctggagttcgtgaccgccgccgggatcactctcggcatggacgagctgtacaagtactcagatctcgagctcaagtagggatcctctagagtcgacctgcagaagcttgcctcgagcagcgctgctcgagagatctacgggtggcatccctgtgacccctccccagtgcctctcctggccctggaagttgccactccagtgcccaccagccttgtcctaataaaattaagttgcatcattttgtctgactaggtgtccttctataatattatggggtggaggggggtggtatggagcaaggggcaagttgggaagacaacctgtagggcctgcggggtctattgggaaccaagctggagtgcagtggcacaatcttggctcactgcaatctccgcctcctgggttcaagcgattctcctgcctcagcctcccgagttgttgggattccaggcatgcatgaccaggctcagctaatttttgtttttttggtagagacggggtttcaccatattggccaggctggtctccaactcctaatctcaggtgatctacccaccttggcctcccaaattgctgggattacaggcgtgaaccactgctcccttccctgtccttctgattttgtaggtaaccacgtgcggaccgagcggccgcaggaacccctagtgatggagttggccactccctctctgcgcgctcgctcgctcactgaggccgggcgaccaaaggtcgcccgacgcccgggctttgcccgggcggcctcagtgagcgagcgagcgcgcagctgcctgcaggggcgcctgatgcggtattttctccttacgcatctgtgcggtatttcacaccgcatacgtcaaagcaaccatagtacgcgccctgtagcggcgcattaagcgcggcgggtgtggtggttacgcgcagcgtgaccgctacacttgccagcgccctagcgcccgctcctttcgctttcttcccttcctttctcgccacgttcgccggctttccccgtcaagctctaaatcgggggctccctttagggttccgatttagtgctttacggcacctcgaccccaaaaaacttgatttgggtgatggttcacgtagtgggccatcgccctgatagacggtttttcgccctttgacgttggagtccacgttctttaatagtggactcttgttccaaactggaacaacactcaaccctatctcgggctattcttttgatttataagggattttgccgatttcggcctattggttaaaaaatgagctgatttaacaaaaatttaacgcgaattttaacaaaatattaacgtttacaattttatggtgcactctcagtacaatctgctctgatgccgcatagttaagccagccccgacacccgccaacacccgctgacgcgccctgacgggcttgtctgctcccggcatccgcttacagacaagctgtgaccgtctccgggagctgcatgtgtcagaggttttcaccgtcatcaccgaaacgcgcgagacgaaagggcctcgtgatacgcctatttttataggttaatgtcatgataataatggtttcttagacgtcaggtggcacttttcggggaaatgtgcgcggaacccctatttgtttatttttctaaatacattcaaatatgtatccgctcatgagacaataaccctgataaatgcttcaataatattgaaaaaggaagagtatgagtattcaacatttccgtgtcgcccttattcccttttttgcggcattttgccttcctgtttttgctcacccagaaacgctggtgaaagtaaaagatgctgaagatcagttgggtgcacgagtgggttacatcgaactggatctcaacagcggtaagatccttgagagttttcgccccgaagaacgttttccaatgatgagcacttttaaagttctgctatgtggcgcggtattatcccgtattgacgccgggcaagagcaactcggtcgccgcatacactattctcagaatgacttggttgagtactcaccagtcacagaaaagcatcttacggatggcatgacagtaagagaattatgcagtgctgccataaccatgagtgataacactgcggccaacttacttctgacaacgatcggaggaccgaaggagctaaccgcttttttgcacaacatgggggatcatgtaactcgccttgatcgttgggaaccggagctgaatgaagccataccaaacgacgagcgtgacaccacgatgcctgtagcaatggcaacaacgttgcgcaaactattaactggcgaactacttactctagcttcccggcaacaattaatagactggatggaggcggataaagttgcaggaccacttctgcgctcggcccttccggctggctggtttattgctgataaatctggagccggtgagcgtgggtctcgcggtatcattgcagcactggggccagatggtaagccctcccgtatcgtagttatctacacgacggggagtcaggcaactatggatgaacgaaatagacagatcgctgagataggtgcctcactgattaagcattggtaactgtcagaccaagtttactcatatatactttagattgatttaaaacttcatttttaatttaaaaggatctaggtgaagatcctttttgataatctcatgaccaaaatcccttaacgtgagttttcgttccactgagcgtcagaccccgtagaaaagatcaaaggatcttcttgagatcctttttttctgcgcgtaatctgctgcttgcaaacaaaaaaaccaccgctaccagcggtggtttgtttgccggatcaagagctaccaactctttttccgaaggtaactggcttcagcagagcgcagataccaaatactgtccttctagtgtagccgtagttaggccaccacttcaagaactctgtagcaccgcctacatacctcgctctgctaatcctgttaccagtggctgctgccagtggcgataagtcgtgtcttaccgggttggactcaagacgatagttaccggataaggcgcagcggtcgggctgaacggggggttcgtgcacacagcccagcttggagcgaacgacctacaccgaactgagatacctacagcgtgagctatgagaaagcgccacgcttcccgaagggagaaaggcggacaggtatccggtaagcggcagggtcggaacaggagagcgcacgagggagcttccagggggaaacgcctggtatctttatagtcctgtcgggtttcgccacctctgacttgagcgtcgatttttgtgatgctcgtcaggggggcggagcctatggaaaaacgccagcaacgcggcctttttacggttcctggccttttgctggccttttgctcacatgt"

pAAVCMVMeta :: Meta
pAAVCMVMeta = Meta { locus=Locus "pAAV_CMV_RPE65_P" 6265 "ds-DNA" (Just Linear) Nothing "23-JAN-2019"
                   , definition=Nothing
                   , accession=Nothing
                   , version=Nothing
                   , keywords=Nothing
                   , source=Nothing
                   , references=[]
                   , comments=["Created by vasilyevaas@biocad.ru", "Core from file: pAAV_core_ITR2", "", "ApEinfo:methylated:1"]
                   }

pAAVCMVFeatures :: [(Feature, Range)]
pAAVCMVFeatures = [ ( (Feature "rep_origin" [ ("direction", "RIGHT")
                                            , ("note", "f1 bacteriophage origin of replication; arrow\r\nindicates direction of (+) strand synthesis")
                                            , ("label", "f1 ori")
                                            , ("ApEinfo_fwdcolor", "#999999")
                                            , ("ApEinfo_revcolor", "#999999")
                                            , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                            ]
                      ), preciseSpan (3742, 4197))
                    , ( (Feature "promoter" [ ("gene", "bla")
                                            , ("label", "AmpR promoter")
                                            , ("ApEinfo_fwdcolor", "#346ee0")
                                            , ("ApEinfo_revcolor", "#346ee0")
                                            , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                            ]
                        ), preciseSpan (4479, 4583))
                    , ( (Feature "CDS" [ ("codon_start", "1")
                                       , ("gene", "bla")
                                       , ("product", "beta-lactamase")
                                       , ("note", "confers resistance to ampicillin, carbenicillin,\r\nand related antibiotics")
                                       , ("label", "AmpR")
                                       , ("ApEinfo_fwdcolor", "#e9d024")
                                       , ("ApEinfo_revcolor", "#e9d024")
                                       , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                       ]
                        ), preciseSpan (4584, 5444))
                     , ( (Feature "misc_feature" [ ("label", "RightITR2")
                                                 , ("ApEinfo_fwdcolor", "#7eff74")
                                                 , ("ApEinfo_revcolor", "#7eff74")
                                                 , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                                 ]
                         ), preciseSpan (3527, 3667))
                      , ( (Feature "misc_feature" [ ("label", "A->T") ]
                          ), Complement (Point 199))
                      ]

pAAVCMVOrigin :: String
pAAVCMVOrigin =
  "CCTGCAGGCAGCTGCGCGCTCGCTCGCTCACTGAGGCCGCCCGGGCGTCGGGCGACCTTTGGTCGCCCGGCCTCAGTGAGCGAGCGAGCGCGCAGAGAGGGAGTGGCCAACTCCATCACTAGGGGTTCCTGCGGCCCGACATTCCGGAGGTACCTCTAGATTGGCAAACAGCTATTATGGGTATTATGGGTctagttattaatagtaatcaattacggggtcattagttcatagcccatatatggagttccgcgttacataacttacggtaaatggcccgcctggctgaccgcccaacgacccccgcccattgacgtcaataatgacgtatgttcccatagtaacgccaatagggactttccattgacgtcaatgggtggagtatttacggtaaactgcccacttggcagtacatcaagtgtatcatatgccaagtacgccccctattgacgtcaatgacggtaaatggcccgcctggcattatgcccagtacatgaccttatgggactttcctacttggcagtacatctacgtattagtcatcgctattaccatggtgatgcggttttggcagtacatcaatgggcgtggatagcggtttgactcacggggatttccaagtctccaccccattgacgtcaatgggagtttgttttgGcaccaaaatcaacgggactttccaaaatgtcgtaacaactccgccccattgacgcaaatgggcggtaggcgtgtacggtgggaggtctatataagcagagctcgtttagtgaaccgtcagatcgcctggagacgccatccacgctgttttgacctccatagaagacaccgggaccgatccagcctccgcggattcgaatcccggccgggaacggtgcattggaacgcggattccccgtgccaagagtgacgtaagtaccgcctatagagtctataggcccacaaaaaatgctttcttcttttaatatacttttttgtttatcttatttctaatactttccctaatctctttctttcagggcaataatgatacaatgtatcatgcctctttgcaccattctaaagaataacagtgataatttctgggttaaggcaatagcaatatttctgcatataaatatttctgcatataaattgtaactgatgtaagaggtttcatattgctaatagcagctacaatccagctaccattctgcttttattttatggttgggataaggctggattattctgagtccaagctaggcccttttgctaatcatgttcatacctcttatcttcctcccacagctcctgggcaacgtgctggtctgtgtgctggcccatcactttggcaaagaattgggatCGTAACTATAACGGTCCTAAGGTAGCGAAATGAGCATCCAAGTCGAGCACCCCGCAGGCGGGTACAAGAAGTTATTTGAAACGGTAGAAGAGCTGTCGTCGCCTCTGACCGCTCATGTAACAGGACGGATACCTCTGTGGTTGACAGGTTCTTTATTACGCTGCGGACCCGGCCTGTTCGAAGTTGGCTCCGAGCCCTTTTACCACCTGTTCGACGGCCAAGCACTGCTCCACAAGTTCGACTTTAAAGAGGGCCACGTGACGTACCATAGAAGGTTCATCAGGACGGATGCTTACGTCCGCGCAATGACGGAAAAGCGGATCGTAATCACGGAGTTCGGGACATGTGCCTTCCCCGACCCCTGTAAGAACATCTTCTCCCGCTTCTTTAGTTACTTTCGGGGCGTGGAGGTTACTGACAATGCCCTGGTTAACGTATACCCTGTCGGGGAAGACTATTACGCCTGCACAGAGACCAACTTCATTACGAAGATCAATCCTGAGACGCTGGAGACCATCAAGCAAGTCGATCTGTGCAATTACGTCTCGGTGAATGGCGCCACCGCGCATCCACACATTGAAAACGACGGCACTGTGTATAATATCGGCAACTGCTTCGGCAAGAACTTCTCGATAGCATACAACATTGTGAAGATCCCACCGCTTCAGGCTGACAAGGAGGACCCCATCTCAAAGTCGGAAATAGTGGTCCAGTTCCCCTGCTCAGACAGGTTCAAACCTAGCTATGTCCACTCGTTTGGTCTGACCCCGAATTATATCGTTTTCGTGGAGACCCCGGTCAAGATTAACCTGTTCAAATTCCTCAGCTCATGGTCCCTCTGGGGCGCAAACTACATGGACTGCTTCGAGTCCAACGAAACAATGGGCGTATGGCTCCATATCGCCGACAAGAAGCGGAAAAAGTACCTGAACAACAAGTACCGCACCTCGCCCTTCAACCTCTTTCATCACATCAACACCTACGAGGACAACGGCTTCCTCATCGTAGACCTCTGCTGCTGGAAGGGCTTCGAGTTTGTCTACAACTACCTGTACCTGGCAAACCTGAGGGAAAACTGGGAAGAGGTCAAAAAGAACGCCCGTAAGGCACCACAACCCGAGGTGCGCCGGTACGTCCTGCCCCTTAACATAGACAAAGCAGACACAGGGAAGAACTTAGTGACTTTGCCTAACACAACCGCGACTGCGATCCTATGTTCCGACGAGACTATCTGGCTGGAGCCTGAGGTGTTATTTTCTGGCCCCAGACAGGCATTCGAGTTTCCCCAGATTAACTATCAGAAGTACTGCGGAAAGCCATACACGTATGCCTACGGTCTCGGCCTGAATCATTTCGTGCCTGACCGCCTGTGTAAGTTGAACGTAAAAACTAAAGAAACCTGGGTGTGGCAGGAGCCCGACTCATACCCTTCCGAGCCCATATTTGTCAGTCACCCTGATGCACTGGAAGAGGACGATGGAGTGGTGTTGTCTGTCGTCGTGAGCCCAGGGGCCGGTCAGAAGCCAGCCTACCTATTGATTCTCAACGCGAAGGACCTGTCTGAGGTCGCGCGGGCGGAGGTCGAAATCAATATCCCCGTTACGTTCCACGGCCTCTTTAAGAAGTCGTAGGGATAACAGGGTAATGTCGACAAGCTTGCTAGCACGGGTGGCATCCCTGTGACCCCTCCCCAGTGCCTCTCCTGGCCCTGGAAGTTGCCACTCCAGTGCCCACCAGCCTTGTCCTAATAAAATTAAGTTGCATCATTTTGTCTGACTAGGTGTCCTTCTATAATATTATGGGGTGGAGGGGGGTGGTATGGAGCAAGGGGCAAGTTGGGAAGACAACCTGTAGGGCCTGCGGGGTCTATTGGGAACCAAGCTGGAGTGCAGTGGCACAATCTTGGCTCACTGCAATCTCCGCCTCCTGGGTTCAAGCGATTCTCCTGCCTCAGCCTCCCGAGTTGTTGGGATTCCAGGCATGCATGACCAGGCTCAGCTAATTTTTGTTTTTTTGGTAGAGACGGGGTTTCACCATATTGGCCAGGCTGGTCTCCAACTCCTAATCTCAGGTGATCTACCCACCTTGGCCTCCCAAATTGCTGGGATTACAGGCGTGAACCACTGCTCCCTTCCCTGTCCTTATGCATGGGCCCGTACGATCACTAGTGTACAGCGGCCGCAGGAACCCCTAGTGATGGAGTTGGCCACTCCCTCTCTGCGCGCTCGCTCGCTCACTGAGGCCGGGCGACCAAAGGTCGCCCGACGCCCGGGCTTTGCCCGGGCGGCCTCAGTGAGCGAGCGAGCGCGCAGCTGCCTGCAGGggcgcctgatgcggtattttctccttacgcatctgtgcggtatttcacaccgcatacgtcaaagcaaccatagtacgcgccctgtagcggcgcattaagcgcggcgggtgtggtggttacgcgcagcgtgaccgctacacttgccagcgccttagcgcccgctcctttcgctttcttcccttcctttctcgccacgttcgccggctttccccgtcaagctctaaatcgggggctccctttagggttccgatttagtgctttacggcacctcgaccccaaaaaacttgatttgggtgatggttcacgtagtgggccatcgccctgatagacggtttttcgccctttgacgttggagtccacgttctttaatagtggactcttgttccaaactggaacaacactcaactctatctcgggctattcttttgatttataagggattttgccgatttcggtctattggttaaaaaatgagctgatttaacaaaaatttaacgcgaattttaacaaaatattaacgtttacaattttatggtgcactctcagtacaatctgctctgatgccgcatagttaagccagccccgacacccgccaacacccgctgacgcgccctgacgggcttgtctgctcccggcatccgcttacagacaagctgtgaccgtctccgggagctgcatgtgtcagaggttttcaccgtcatcaccgaaacgcgcgagacgaaagggcctcgtgatacgcctatttttataggttaatgtcatgataataatggtttcttagacgtcaggtggcacttttcggggaaatgtgcgcggaacccctatttgtttatttttctaaatacattcaaatatgtatccgctcatgagacaataaccctgataaatgcttcaataatattgaaaaaggaagagtatgagtattcaacatttccgtgtcgcccttattcccttttttgcggcattttgccttcctgtttttgctcacccagaaacgctggtgaaagtaaaagatgctgaagatcagttgggtgcacgagtgggttacatcgaactggatctcaacagcggtaagatccttgagagttttcgccccgaagaacgttttccaatgatgagcacttttaaagttctgctatgtggcgcggtattatcccgtattgacgccgggcaagagcaactcggtcgccgcatacactattctcagaatgacttggttgagtactcaccagtcacagaaaagcatcttacggatggcatgacagtaagagaattatgcagtgctgccataaccatgagtgataacactgcggccaacttacttctgacaacgatcggaggaccgaaggagctaaccgcttttttgcacaacatgggggatcatgtaactcgccttgatcgttgggaaccggagctgaatgaagccataccaaacgacgagcgtgacaccacgatgcctgtagcaatggcaacaacgttgcgcaaactattaactggcgaactacttactctagcttcccggcaacaattaatagactggatggaggcggataaagttgcaggaccacttctgcgctcggcccttccggctggctggtttattgctgataaatctggagccggtgagcgtgggtctcgcggtatcattgcagcactggggccagatggtaagccctcccgtatcgtagttatctacacgacggggagtcaggcaactatggatgaacgaaatagacagatcgctgagataggtgcctcactgattaagcattggtaactgtcagaccaagtttactcatatatactttagattgatttaaaacttcatttttaatttaaaaggatctaggtgaagatcctttttgataatctcatgaccaaaatcccttaacgtgagttttcgttccactgagcgtcagaccccgtagaaaagatcaaaggatcttcttgagatcctttttttctgcgcgtaatctgctgcttgcaaacaaaaaaaccaccgctaccagcggtggtttgtttgccggatcaagagctaccaactctttttccgaaggtaactggcttcagcagagcgcagataccaaatactgttcttctagtgtagccgtagttaggccaccacttcaagaactctgtagcaccgcctacatacctcgctctgctaatcctgttaccagtggctgctgccagtggcgataagtcgtgtcttaccgggttggactcaagacgatagttaccggataaggcgcagcggtcgggctgaacggggggttcgtgcacacagcccagcttggagcgaacgacctacaccgaactgagatacctacagcgtgagctatgagaaagcgccacgcttcccgaagggagaaaggcggacaggtatccggtaagcggcagggtcggaacaggagagcgcacgagggagcttccagggggaaacgcctggtatctttatagtcctgtcgggtttcgccacctctgacttgagcgtcgatttttgtgatgctcgtcaggggggcggagcctatggaaaaacgccagcaacgcggcctttttacggttcctggccttttgctggccttttgctcacatgt"

dottedMeta :: Meta
dottedMeta = Meta { locus=Locus "pAAV-GFP-CellBio" 5374 "ds-DNA" (Just Circular) (Just "SYN") "15-AUG-2016"
                  , definition=Just $ "."
                  , accession=Just $ "."
                  , version=Just $ Version "." Nothing
                  , keywords=Just "."
                  , source=Just $ Source "." Nothing
                  , references=[ Reference "." Nothing Nothing Nothing Nothing
                               , Reference "." Nothing Nothing Nothing Nothing
                               ]
                  , comments=["."]
                  }

sophisticatedFeaturesSeq :: String
sophisticatedFeaturesSeq = "cctacagcgtgagctatgagaaagcgccacgcttcccgaagggagaaaggcggacaggtatccggtaagcggcagggtcggaacaggagagcgcacgagggagcttccagggggaaacgcctggtatctttatagtcctgtcgggtttcgccacctctgacttgagcgtcgatttttgtgatgctcgtcaggggggcggagcctatggaaaaacgccagcaacgcggcctttttacggttcctggccttttgctggccttttgctcacatgttctttcctgcgttatcccctgattctgtggataaccgtattaccgcctttgagtgagctgataccgctcgccgcagccgaacgaccgagcgcagcgagtcagtgagcgaggaagcgtacatttatattggctcatgtccaatatgaccgccatgttgacattgattattgactagaccgcgttacataacttacggtaaatggcccgcctggctgaccgcccaacgacccccgcccattgacgtcaataatgacgtatgttcccatagtaacgccaatagggactttccattgacgtcaatgggtggagtatttacggtaaactgcccacttggcagtacatcaagtgtatcatatgccaagtacgccccctattgacgtcaatgacggtaaatggcccgcctggcattatgcccagtacatgaccttatgggactttcctacttggcagtacatctacgtattagtcatcgctattaccatggtgatgcggttttggcagtacatcaatgggcgtggatagcggtttgactcacggggatttccaagtctccaccccattgacgtcaatgggagtttgttttggcaccaaaatcaacgggactttccaaaatgtcgtaacaactccgccccattgacgcaaatgggcggtaggcgtgtacggtgggaggtctatataagcagagctcgtttagtgaaccgtcagatcgcctggagacgccatccacgctgttttgacctccatagaagacaccgggaccgatccagcctccgcggccgggaacggtgcattggaacgcggattccccgtgccaagagtgacgtaagtaccgcctatagagtctataggcccacccccttggcttcttatgcatgctatactgtttttggcttggggtctatacacccccgcttcctcatgttataggtgatggtatagcttagcctataggtgtgggttattgaccattattgaccactcccctattggtgacgatactttccattactaatccataacatggctctttgccacaactctctttattggctatatgccaatacactgtccttcagagactgacacggactctgtatttttacaggatggggtctcatttattatttacaaattcacatatacaacaccaccgtccccagtgcccgcagtttttattaaacataacgtgggatctccacgcgaatctcgggtacgtgttccggacatgggctcatctccggtagcggcggagcttctacatccgagccctgctcccatgcctccagcgactcatggtcgctcggcagctccttgctcctaacagtggaggccagacttaggcacagcacgatgcccaccaccaccagtgtgccgcacaaggccgtggcggtagggtatgtgtctgaaaatgagctcggggagcgggcttgcaccgctgacgcatttggaagacttaaggcagcggcagaagaagatgcaggcagctgagttgttgtgttctgataagagtcagaggtaactcccgttgcggtgctgttaacggtggagggcagtgtagtctgagcagtactcgttgctgccgcgcgcgccaccagacataatagctgacagactaacagactgttcctttccatgggtcttttctgcagtcaccgtccttgacacgaagcttgccgccaccatggagaccgacaccctgctgctgtgggtgctgctgctgtgggtgcccgggtcgacgaagagctcatgagcggatacatatttgaatgtatttagaaaaataaacaaataggggtcagtgttacaaccaattaaccaattctgaacattatcgcgagcccatttatacctgaatatggctcataacaccccttgtttgcctggcggcagtagcgcggtggtcccacctgaccccatgccgaactcagaagtgaaacgccgtagcgccgatggtagtgtggggactccccatgcgagagtagggaactgccaggcatcaaataaaacgaaaggctcagtcgaaagactgggcctttcgcccgggctaattatggggtgtcgcccttggggtgagaccctcgagtgtacagaattcttactgatacgtgtccagatcaaccgctttcacgacctctaccagacacatgtgatcacggcgctcgtcgcggtctttgctcagtttggtgtggtaggtaatgtgatgataacgcgggatatgcactgccgcggagcccgccaacggacgattcatttggctgcatttggtaaccagtttttcggtcacaccttcaatatcgtacgcctggttgaactcaacgcggatgccattgttaacggtgtcaggcagaatatacagaatgcttggcgggcattggaatgcaacgttcttacgcagaatgtgaccgtctttcttaaagttctcaccagtcagcgtgacacgattgtagatagaaccgcgttcgtaggtaaccatagcacgcgtcttgtacacgccgtcgccttcgaagctgatggtacgctcttgggtataaccttccggcatggcgctcttaaagaaatccttgatgtggctcgggtacttgcgaaacactgaacaccgtagctcagggtgctcaccagggttgcccacgggacccggcaggtcgcccgtagtgcagatgtatttcgctttaatggtacccgtggtcgcgtcacccggtaccctcgcctttaatgataaatttcataccttcgacgtcgccttccagttcggtgatatacgggatctctttctcaaacagttttgcaccttccgtcaatgccgtcatatgtttacctcctaaggtctcgaaaagttaaacaaaattatttctaaagggaaaccgttgtggaattgtgagcgctcacaattccacatattataattgttatccgctcacaaagcaaataaatttttcatgatttcactgtgcatgaagctcgtaattgttatccgctcacaattaagggcgacacaaaatttattctaaatgataataaatactgataacatcttatagtttgtattatattttgtattatcgttgacatgtataattttgatatcaaaaactgattttccctttattattttcgagatttattttcttaattctctttaacaaactagaaatattgtatatacaaaaaatcataaataatagatgaatagtttaattataggtgttcatcaatcgaaaaagcaacgtatcttatttaaagtgcgttgcttttttctcatttataaggttaaataattctcatatatcaagcaaagtgacaggcgcccttaaatattctgacaaatgctctttccctaaactccccccataaaaaaacccgccgaagcgggtttttacgttatttgcggattaacgattactcgttatcagaaccgcccagggggcccgagcttaagactggccgtcgttttacaacacaagctcttccgtacggtggctgcaccatctgtcttcatcttcccgccatctgatgagcagttgaaatctggaactgcctctgttgtgtgcctgctgaataacttctatcccagagaggccaaagtacagtggaaggtggataacgccctccaatcgggtaactcccaggagagtgtcacagagcaggacagcaaggacagcacctacagcctcagcagcaccctgacgctgagcaaagcagactacgagaaacacaaagtctacgcctgcgaagtcacccatcagggcctgagctcgcccgtcacaaagagcttcaacaggggagagtgttaatagtctagacctaggtgatcataatcagccataccacatttgtagaggttttacttgctttaaaaaacctcccacacctccccctgaacctgaaacataaaatgaatgcaattgttgttgttaacttgtttattgcagcttataatggttacaaataaagcaatagcatcacaaatttcacaaataaagcatttttttcactgcattctagttgtggtttgtccaaactcatcaatgtatcttatcatgtctggagatctctagctagaggatcgatccccgccccggacgaactaaacctgactacgacatctctgccccttcttcgcggggcagtgcatgtaatcccttcagttggttggtacaacttgccaactgaaccctaaacgggtagcatatgcttcccgggtagtagtatatactatccagactaaccctaattcaatagcatatgttacccaacgggaagcatatgctatcgaattagggttagtaaaagggtcctaaggaacagcgatgtaggtgggcgggccaagataggggcgcgattgctgcgatctggaggacaaattacacacacttgcgcctgagcgccaagcacagggttgttggtcctcatattcacgaggtcgctgagagcacggtgggctaatgttgccatgggtagcatatactacccaaatatctggatagcatatgctatcctaatctatatctgggtagcataggctatcctaatctatatctgggtagcatatgctatcctaatctatatctgggtagtatatgctatcctaatttatatctgggtagcataggctatcctaatctatatctgggtagcatatgctatcctaatctatatctgggtagtatatgctatcctaatctgtatccgggtagcatatgctatcctaatagagattagggtagtatatgctatcctaatttatatctgggtagcatatactacccaaatatctggatagcatatgctatcctaatctatatctgggtagcatatgctatcctaatctatatctgggtagcataggctatcctaatctatatctgggtagcatatgctatcctaatctatatctgggtagtatatgctatcctaatttatatctgggtagcataggctatcctaatctatatctgggtagcatatgctatcctaatctatatctgggtagtatatgctatcctaatctgtatccgggtagcatatgctatcctcatgataagctgtcaaacatgagaattaattcttgaagacgaaagggcctcgtgatacgcctatttttataggttaatgtcatgataataatggtttcttagacgtcaggtggcacttttcggggaaatgtgcgcggaacccctatttgtttatttttctaaatacattcaaatatgtatccgctcatgagacaataaccctgataaatgcttcaataatattgaaaaaggaagagtatgagtattcaacatttccgtgtcgcccttattcccttttttgcggcattttgccttcctgtttttgctcacccagaaacgctggtgaaagtaaaagatgctgaagatcagttgggtgcacgagtgggttacatcgaactggatctcaacagcggtaagatccttgagagttttcgccccgaagaacgttttccaatgatgagcacttttaaagttctgctatgtggcgcggtattatcccgtgttgacgccgggcaagagcaactcggtcgccgcatacactattctcagaatgacttggttgagtactcaccagtcacagaaaagcatcttacggatggcatgacagtaagagaattatgcagtgctgccataaccatgagtgataacactgcggccaacttacttctgacaacgatcggaggaccgaaggagctaaccgcttttttgcacaacatgggggatcatgtaactcgccttgatcgttgggaaccggagctgaatgaagccataccaaacgacgagcgtgacaccacgatgcctgcagcaatggcaacaacgttgcgcaaactattaactggcgaactacttactctagcttcccggcaacaattaatagactggatggaggcggataaagttgcaggaccacttctgcgctcggcccttccggctggctggtttattgctgataaatctggagccggtgagcgtgggtctcgcggtatcattgcagcactggggccagatggtaagccctcccgtatcgtagttatctacacgacggggagtcaggcaactatggatgaacgaaatagacagatcgctgagataggtgcctcactgattaagcattggtaactgtcagaccaagtttactcatatatactttagattgatttaaaacttcatttttaatttaaaaggatctaggtgaagatcctttttgataatctcatgaccaaaatcccttaacgtgagttttcgttccactgagcgtcagaccccgtagaaaagatcaaaggatcttcttgagatcctttttttctgcgcgtaatctgctgcttgcaaacaaaaaaaccaccgctaccagcggtggtttgtttgccggatcaagagctaccaactctttttccgaaggtaactggcttcagcagagcgcagataccaaatactgttcttctagtgtagccgtagttaggccaccacttcaagaactctgtagcaccgcctacatacctcgctctgctaatcctgttaccagtggctgctgccagtggcgataagtcgtgtcttaccgggttggactcaagacgatagttaccggataaggcgcagcggtcgggctgaacggggggttcgtgcacacagcccagcttggagcgaacgacctacaccgaactgagata"

sophisticatedFeatures :: [(Feature, Range)]
sophisticatedFeatures = 
  [ (Feature "source" 
      [ ("organism", "synthetic DNA construct")
      , ("mol_type", "other DNA")
      ]
    , preciseSpan (0, 6950))

  , (Feature "rep_origin" 
      [ ("label", "pUCorigin and also a multiline property")
      , ("note", "/vntifkey=33")
      ]
    , Join [Point 0, preciseSpan (6550, 6950)])

  , (Feature "enhancer" 
      [ ("label", "cmv enhanser")
      , ("label", "cmv\\enhanser")
      , ("note", "/vntifkey=9")
      ]
    , preciseSpan (448, 857))

  , (Feature "misc_feature"
      [ ("label", "hCMV promoter")
      , ("label", "hCMV\\promoter")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (858, 983))

  , (Feature "intron"
      [ ("label", "IntronA")
      , ("note", "/vntifkey=15")
      ]
    , preciseSpan (1011, 1918))

  , (Feature "primer_bind" 
      [ ("label", "inv olig1") ]
    , preciseSpan (1501, 1521))

  , (Feature "misc_feature"
      [ ("label", "Kozak")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (1944, 1952))

  , (Feature "misc_feature"
      [ ("label", "Leader IgK")
      , ("note", "Leader IgK")
      , ("note", "/ugene_name=Leader\\ IgK")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (1953, 2009))

  , (Feature "misc_feature"   
      [ ("label", "START")
      , ("note", "START")
      , ("note", "/ugene_name=START")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (1953, 1955))

  , (Feature "misc_feature"   
      [ ("label", "GFP stuffer")
      , ("note", "GFP stuffer")
      , ("note", "/ugene_name=GFP\\ stuffer")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (2010, 3738))

  , (Feature "misc_feature"   
      [ ("label", "CK")
      , ("note", "CK")
      , ("note", "/ugene_name=CK")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (3739, 4059))

  , (Feature "misc_feature"   
      [ ("label", "STOP")
      , ("note", "STOP")
      , ("note", "/ugene_name=STOP")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (4060, 4065))

  , (Feature "misc_feature"  
      [ ("gene", "SV40_PA term")
      , ("label", "SV40_PA term")
      , ("label", "SV40_PA\\term")
      , ("note", "/vntifkey=21")
      ]
    , preciseSpan (4078, 4316))

  , (Feature "primer_bind"   
      [ ("label", "pEE_Clab") ]
    , preciseSpan (4349, 4369))

  , (Feature "rep_origin"     
      [ ("label", "EBV ori")
      , ("label", "EBV\\ori")
      , ("note", "/vntifkey=33")
      ]
    , preciseSpan (4581, 4974))

  , (Feature "CDS"          
      [ ("codon_start", "1")
      , ("label", "AmpR")
      , ("note", "/vntifkey=4")
      , ("translation", "MSIQHFRVALIPFFAAFCLPVFAHPETLVKVKDAEDQLGARVGYI\nELDLNSGKILESFRPEERFPMMSTFKVLLCGAVLSRVDAGQEQLGRRIHYSQNDLVEYS\nPVTEKHLTDGMTVRELCSAAITMSDNTAANLLLTTIGGPKELTAFLHNMGDHVTRLDRW\nEPELNEAIPNDERDTTMPAAMATTLRKLLTGELLTLASRQQLIDWMEADKVAGPLLRSA\nLPAGWFIADKSGAGERGSRGIIAALGPDGKPSRIVVIYTTGSQATMDERNRQIAEIGAS\nLIKHW")
      ]
    , preciseSpan (5542, 6402))
  ]
