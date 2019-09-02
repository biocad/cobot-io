{-# LANGUAGE OverloadedStrings #-}

module GBParserSpec where

import           Bio.GB       (Feature (..), Form (..), GenBankSequence (..),
                               Locus (..), Meta (..), Reference (..),
                               Source (..), Version (..), fromFile)
import           Bio.Sequence (Range, unsafeMarkedSequence)
import           Test.Hspec

gbParserSpec :: Spec
gbParserSpec = describe "GenBank format parser." $ do
    pAAVGFPSpecP "test/GB/pAAV-GFP-CellBioLab.gb"
    pAAVCMVSpecP "test/GB/pAAV_CMV_RPE65_PolyA_linkers.gb"
    dottedMetaSpecP "test/GB/pAAV-GFP-CellBioLab-dots.gb"

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
pAAVGFPFeatures = [ ( (Feature "misc_feature" True [ ("label", "Right ITR")
                                                                        ]
                                           ), (0, 130))
                                       , ( (Feature "enhancer" True [ ("label", "CMV enhancer")
                                                                    , ("note", "human cytomegalovirus immediate early enhancer")
                                                                    ]
                                           ), (205, 509))
                                       , ( (Feature "promoter" True [ ("label", "CMV promoter")
                                                                                                   , ("note", "human cytomegalovirus (CMV) immediate early \npromoter")
                                                                                                   ]
                                           ), (509, 712))
                                       , ( (Feature "misc_feature" True [ ("label", "Human beta-globin Intron")
                                                                                                            ]
                                           ), (804, 1297))
                                       , ( (Feature "CDS" True [ ("codon_start", "1")
                                                                                                , ("product", "enhanced GFP")
                                                                                                , ("label", "EGFP")
                                                                                                , ("note", "mammalian codon-optimized")
                                                                                                ]
                                           ), (1319, 2036))
                                       , ( (Feature "repeat_region" False [ ("label", "Left ITR")
                                                                                                         , ("note", "inverted terminal repeat of adeno-associated virus \nserotype 2\noooooo")
                                                                                                         , ("prop", "1")
                                                                                                         ]
                                           ), (2636, 2777))
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
pAAVCMVFeatures = [ ( (Feature "rep_origin" True [ ("direction", "RIGHT")
                                                                                                       , ("note", "f1 bacteriophage origin of replication; arrow\r\nindicates direction of (+) strand synthesis")
                                                                                                       , ("label", "f1 ori")
                                                                                                       , ("ApEinfo_fwdcolor", "#999999")
                                                                                                       , ("ApEinfo_revcolor", "#999999")
                                                                                                       , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                                                                                       ]
                      ), (3742, 4198))
                    , ( (Feature "promoter" True [ ("gene", "bla")
                                                                                                     , ("label", "AmpR promoter")
                                                                                                     , ("ApEinfo_fwdcolor", "#346ee0")
                                                                                                     , ("ApEinfo_revcolor", "#346ee0")
                                                                                                     , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                                                                                     ]
                        ), (4479, 4584))
                    , ( (Feature "CDS" True [ ("codon_start", "1")
                                                                                                , ("gene", "bla")
                                                                                                , ("product", "beta-lactamase")
                                                                                                , ("note", "confers resistance to ampicillin, carbenicillin,\r\nand related antibiotics")
                                                                                                , ("label", "AmpR")
                                                                                                , ("ApEinfo_fwdcolor", "#e9d024")
                                                                                                , ("ApEinfo_revcolor", "#e9d024")
                                                                                                , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                                                                                ]
                        ), (4584, 5445))
                     , ( (Feature "misc_feature" True [ ("label", "RightITR2")
                                                                                                         , ("ApEinfo_fwdcolor", "#7eff74")
                                                                                                         , ("ApEinfo_revcolor", "#7eff74")
                                                                                                         , ("ApEinfo_graphicformat", "arrow_data {{0 1 2 0 0 -1} {} 0}\r\nwidth 5 offset 0")
                                                                                                         ]
                         ), (3527, 3668))
                      , ( (Feature "misc_feature" False [ ("label", "A->T")
                                                                                                ]
                          ), (199, 200))
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
