{-# LANGUAGE OverloadedStrings #-}

module PDBSpec where

import           Bio.PDB
import           Bio.PDB.Type    (Atom (..), FieldType (..), PDB (..))
import           Data.Map.Strict (fromList, singleton)
import           Data.Text       as T
import           Data.Vector     (empty, fromList, singleton)
import           Test.Hspec


oneModelSpecP :: Spec
oneModelSpecP = describe "One model." $
        it "correctly parses pdb with only one model without strings \"MODEL\" & \"ENDMDL\"" $ do
        let mt  = fromTextPDB $ T.pack   ("HEADER header\n" ++
                                         "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                         "COMPND compnd\n" ++
                                         "SOURCE source\n" ++
                                         "KEYWDS keywds\n" ++
                                         "EXPDTA expdta\n" ++
                                         "AUTHOR  Masha\n" ++
                                         "REVDAT revdat\n" ++
                                         "REMARK   1 REFERENCE 1\n" ++
                                         "SEQRES seqres\n" ++
                                         "CRYST1 cryst1\n" ++
                                         "ORIGX1 origx1 n=1\n" ++
                                         "SCALE2 sclaen n=2\n" ++
                                         "ATOM   2032  OXT CYS A 214      -4.546 -29.673  26.796  1.0 143.51           O  \n" ++
                                         "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++
                                         "TER    2034      CYS A 214                                                      \n" ++
                                         "ATOM   2035  N   GLU B   1      18.637 -61.583  66.852  1.0 118.48           N  \n" ++
                                         "TER   12534      ARG D 474                                                      \n" ++
                                         "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                         "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++
                                         "ATOM   2036  CA  GLU B   1      19.722 -62.606  66.868  1.00 19.77           C  \n" ++
                                         "CONECT conect\n" ++
                                         "CONECT conect conect\n" ++
                                         "MASTER 1 2 3 4 5 6 7 8\n" ++
                                         "END"
                                         )
        mt `shouldBe` Right oneModelPDB

oneModelPDB :: PDB
oneModelPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                  , remarks = Data.Map.Strict.singleton 1 (Data.Vector.singleton "REFERENCE 1")
                  , models = Data.Vector.singleton $ Data.Vector.fromList [Data.Vector.fromList [Atom {atomSerial = 2032, atomName = "OXT ", atomAltLoc = 'C', atomResName = "YS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ',
                                                                           atomX = -4.546, atomY = -29.673, atomZ = 26.796, atomOccupancy = 1.0, atomTempFactor = 143.51, atomElement = "O",
                                                                           atomCharge = "  "},
                                                                           Atom {atomSerial = 2033, atomName = "H   ", atomAltLoc = 'C', atomResName = "YS", atomChainID = 'A',
                                                                           atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225, atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0,
                                                                           atomElement = "H", atomCharge = "  "}],
                                                                           Data.Vector.fromList [Atom {atomSerial = 2035, atomName = "N   ", atomAltLoc = 'G', atomResName = "LU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                           atomX = 18.637, atomY = -61.583, atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = "N", atomCharge = "  "},
                                                                           Atom {atomSerial = 2036, atomName = "CA  ", atomAltLoc = 'G', atomResName = "LU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                           atomX = 19.722, atomY = -62.606, atomZ = 66.868, atomOccupancy = 1.0, atomTempFactor = 19.77, atomElement = "C", atomCharge = "  "}]
                                                                        ]
                , otherFields = Data.Map.Strict.fromList [(HEADER, Data.Vector.fromList [" header"]),(COMPND,Data.Vector.fromList[" compnd"]),(SOURCE,Data.Vector.fromList[" source"]),(KEYWDS,Data.Vector.fromList[" keywds"]),(EXPDTA,Data.Vector.fromList[" expdta"]),(AUTHOR,Data.Vector.fromList["  Masha"]),(REVDAT,Data.Vector.fromList[" revdat"]),(SEQRES,Data.Vector.fromList [" seqres"]),
                                          (CRYST1,Data.Vector.fromList [" cryst1"]),(ORIGXn,Data.Vector.fromList[" origx1 n=1"]),(SCALEn,Data.Vector.fromList [" sclaen n=2"]),(MASTER,Data.Vector.fromList[" 1 2 3 4 5 6 7 8"])]
                }

manyModelsSpecP :: Spec
manyModelsSpecP = describe "Some models." $
    it "correctly parses pdb with many models - they have strings \"MODEL\" & \"ENDMDL\"" $ do
    let mt = fromTextPDB $ T.pack   ( "HEADER header\n" ++
                                      "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                      "COMPND compnd\n" ++
                                      "SOURCE source\n" ++
                                      "KEYWDS keywds\n" ++
                                      "EXPDTA expdta\n" ++
                                      "AUTHOR  Masha\n" ++
                                      "REVDAT revdat\n" ++
                                      "REMARK 1 REFERENCE 1\n" ++
                                      "REMARK 2 Reference 2_1/1\n" ++
                                      "SEQRES seqres\n" ++
                                      "CRYST1 cryst1\n" ++
                                      "ORIGX1 origx1 n=1\n" ++
                                      "SCALE2 sclaen n=2\n" ++
                                      "MODEL  4  \n" ++
                                      "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++
                                      "TER    2034      CYS A 214                                                      \n" ++
                                      "ANISOU anisou\n" ++
                                      "ENDMDL \n" ++
                                      "MODEL  5 \n" ++
                                      "ATOM   2035  N   GLU B   1      18.637-691.583  66.852  1.0 118.48           N  \n" ++
                                      "ATOM  12531 HH12 ARG D 474      45.558 -39.551 -49.936  1.00 15.00           H  \n" ++
                                      "ENDMDL \n" ++
                                      "MODEL  6 \n" ++
                                      "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                      "TER   12534      ARG D 474                                                      \n" ++
                                      "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++
                                      "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                      "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++
                                      "ENDMDL\n" ++
                                      "CONECT conect\n" ++
                                      "CONECT conect conect\n" ++
                                      "MASTER 1 2 3 4 5 6 7 8\n"
                                      )
    mt `shouldBe` Right manyModelsPDB

manyModelsPDB :: PDB
manyModelsPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                      , remarks = Data.Map.Strict.fromList [(1, Data.Vector.singleton "REFERENCE 1"), (2, Data.Vector.singleton "Reference 2_1/1")]
                      , models = Data.Vector.fromList [Data.Vector.fromList [Data.Vector.fromList [Atom {atomSerial = 2033, atomName = "H   ", atomAltLoc = 'C', atomResName = "YS",
                                                                                                    atomChainID = 'A', atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225,
                                                                                                    atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = "H", atomCharge = "  "}]
                                                                            ],
                                                      Data.Vector.fromList [Data.Vector.fromList [Atom {atomSerial = 2035, atomName = "N   ", atomAltLoc = 'G', atomResName = "LU",
                                                                                                   atomChainID = 'B', atomResSeq = 1, atomICode = ' ', atomX = 18.637, atomY = -691.583,
                                                                                                   atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = "N", atomCharge = "  "}],
                                                                            Data.Vector.fromList [Atom {atomSerial = 12531, atomName = "HH12", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 45.558, atomY = -39.551, atomZ = -49.936,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = "H", atomCharge = "  "}]
                                                                           ],
                                                      Data.Vector.fromList [Data.Vector.fromList [Atom {atomSerial = 12532, atomName = "HH21", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.457, atomY = -38.007, atomZ = -47.445,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = "H", atomCharge = "  "},
                                                                                                  Atom {atomSerial = 12533, atomName = "HH22", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.405, atomY = -39.268, atomZ = -48.629,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = "H", atomCharge = "  "}]
                                                                            ]
                                                        ]
                    , otherFields = Data.Map.Strict.fromList [(HEADER, Data.Vector.fromList [" header"]),(COMPND,Data.Vector.fromList[" compnd"]),(SOURCE,Data.Vector.fromList[" source"]),(KEYWDS,Data.Vector.fromList[" keywds"]),(EXPDTA,Data.Vector.fromList[" expdta"]),(AUTHOR,Data.Vector.fromList["  Masha"]),(REVDAT,Data.Vector.fromList[" revdat"]),(SEQRES,Data.Vector.fromList [" seqres"]),
                    (CRYST1,Data.Vector.fromList [" cryst1"]),(ORIGXn,Data.Vector.fromList[" origx1 n=1"]),(SCALEn,Data.Vector.fromList [" sclaen n=2"]),(MASTER,Data.Vector.fromList[" 1 2 3 4 5 6 7 8"])]
                     }

noModelsSpecP :: Spec
noModelsSpecP = describe "No models." $
    it "correctly parses pdb without models (no ATOM, TER, HETATM strings)" $ do
    let mt = fromTextPDB $ T.pack   ( "HEADER header\n" ++
                                      "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                      "COMPND compnd\n" ++
                                      "SOURCE source\n" ++
                                      "KEYWDS keywds\n" ++
                                      "EXPDTA expdta\n" ++
                                      "AUTHOR  Masha\n" ++
                                      "REVDAT revdat\n" ++
                                      "REMARK 1 REFERENCE 1\n" ++
                                      "SEQRES seqres\n" ++
                                      "CRYST1 cryst1\n" ++
                                      "ORIGX1 origx1 n=1\n" ++
                                      "SCALE2 sclaen n=2\n" ++
                                      "CONECT conect\n" ++
                                      "CONECT conect conect\n" ++
                                      "MASTER 1 2 3 4 5 6 7 8\n"
                                      )
    mt `shouldBe` Right noModelsPDB

noModelsPDB :: PDB
noModelsPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                  , remarks = Data.Map.Strict.singleton 1 (Data.Vector.singleton "REFERENCE 1")
                  , models = Data.Vector.empty
                  , otherFields = Data.Map.Strict.fromList [(HEADER, Data.Vector.fromList [" header"]),(COMPND,Data.Vector.fromList[" compnd"]),(SOURCE,Data.Vector.fromList[" source"]),(KEYWDS,Data.Vector.fromList[" keywds"]),(EXPDTA,Data.Vector.fromList[" expdta"]),(AUTHOR,Data.Vector.fromList["  Masha"]),(REVDAT,Data.Vector.fromList[" revdat"]),(SEQRES,Data.Vector.fromList [" seqres"]),
                                          (CRYST1,Data.Vector.fromList [" cryst1"]),(ORIGXn,Data.Vector.fromList[" origx1 n=1"]),(SCALEn,Data.Vector.fromList [" sclaen n=2"]),(MASTER,Data.Vector.fromList[" 1 2 3 4 5 6 7 8"])]
                }

allFieldsModelSpecP :: Spec
allFieldsModelSpecP = describe "PDB with all strings." $
    it "correctly parses pdb with all types of string" $ do
    let mt = fromTextPDB $ T.pack   ( "HEADER header\n" ++
                                      "OBSLTE obslte\n" ++
                                      "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                      "SPLIT split\n" ++
                                      "CAVEAT caveat\n" ++
                                      "COMPND compnd\n" ++
                                      "SOURCE source\n" ++
                                      "KEYWDS keywds\n" ++
                                      "EXPDTA expdta1\n" ++
                                      "EXPDTA expdta2\n" ++
                                      "NUMMDL nummdl\n" ++
                                      "MDLTYP mdltyp mdltyp\n" ++
                                      "AUTHOR  Masha\n" ++
                                      "REVDAT revdat\n" ++
                                      "SPRSDE sprsde\n" ++
                                      "JRNL jrnl\n" ++
                                      "REMARK 1 REFERENCE 1\n" ++
                                      "DBREF dbref\n" ++
                                      "DBREF1 dbref1\n" ++
                                      "DBREF2 dbref2_1\n" ++
                                      "DBREF2 dbref2_2\n" ++
                                      "SEQADV seqadv\n" ++
                                      "SEQRES seqres\n" ++
                                      "MODRES modres\n" ++
                                      "HET het\n" ++
                                      "HETNAM hetnam\n" ++
                                      "HETSYN hetsyn\n" ++
                                      "FORMUL of love\n" ++
                                      "HELIX helix\n" ++
                                      "SHEET sheet\n" ++
                                      "SSBOND ssbond\n" ++
                                      "LINK link\n" ++
                                      "CISPEP cispep\n" ++
                                      "SITE site\n" ++
                                      "CRYST1 cryst1\n" ++
                                      "ORIGX1 origx1 n=1\n" ++
                                      "SCALE2 sclaen n=2\n" ++
                                      "MTRIX3 matrixn n=3\n" ++
                                      "MODEL 1\n" ++
                                      "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                      "TER   12534      ARG D 474                                                      \n" ++
                                      "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                      "ENDMDL\n" ++
                                      "CONECT conect\n" ++
                                      "CONECT conect conect\n" ++
                                      "MASTER 1 2 3 4 5 6 7 8\n" ++
                                      "END"
                                      )
    mt `shouldBe` Right pdbWithAllFields

pdbWithAllFields :: PDB
pdbWithAllFields = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                       , models = Data.Vector.fromList [Data.Vector.fromList [Data.Vector.singleton Atom {atomSerial = 12532, atomName = "HH21", atomAltLoc = ' ', atomResName = "ARG",
                                     atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.457, atomY = -38.007, atomZ = -47.445,
                                     atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = "H", atomCharge = "  "}]]
                       , remarks = Data.Map.Strict.singleton 1 (Data.Vector.singleton "REFERENCE 1")
                       , otherFields = Data.Map.Strict.fromList [(HEADER, Data.Vector.fromList [" header"]),(OBSLTE, Data.Vector.fromList [" obslte"]), (SPLIT, Data.Vector.fromList [" split"]), (CAVEAT, Data.Vector.fromList [" caveat"]),
                                                                 (COMPND, Data.Vector.fromList [" compnd"]),(SOURCE, Data.Vector.fromList [" source"]),(KEYWDS, Data.Vector.fromList [" keywds"]),
                                                                 (EXPDTA, Data.Vector.fromList [" expdta1"," expdta2"]),(NUMMDL, Data.Vector.fromList [" nummdl"]),(MDLTYP, Data.Vector.fromList [" mdltyp mdltyp"]),
                                                                 (AUTHOR, Data.Vector.fromList ["  Masha"]),(REVDAT, Data.Vector.fromList [" revdat"]),(SPRSDE, Data.Vector.fromList [" sprsde"]),
                                                                 (JRNL, Data.Vector.fromList [" jrnl"]),(DBREF, Data.Vector.fromList ["dbref"]), (DBREF1, Data.Vector.fromList ["dbref1"]), (DBREF2, Data.Vector.fromList ["dbref2_1","dbref2_2"]),
                                                                 (SEQADV, Data.Vector.fromList [" seqadv"]),(SEQRES, Data.Vector.fromList [" seqres"]),(MODRES, Data.Vector.fromList [" modres"]),
                                                                 (HET, Data.Vector.fromList ["het"]), (HETNAM, Data.Vector.fromList ["hetnam"]), (HETSYN, Data.Vector.fromList ["hetsyn"]),(FORMUL, Data.Vector.fromList [" of love"]),(HELIX, Data.Vector.fromList [" helix"]),
                                                                 (SHEET, Data.Vector.fromList [" sheet"]),(SSBOND, Data.Vector.fromList [" ssbond"]),(LINK, Data.Vector.fromList [" link"]),
                                                                 (CISPEP, Data.Vector.fromList [" cispep"]),(SITE,Data.Vector.fromList [" site"]),(CRYST1, Data.Vector.fromList [" cryst1"]),(MTRIXn, Data.Vector.fromList [" matrixn n=3"]),
                                                                 (ORIGXn, Data.Vector.fromList [" origx1 n=1"]),(SCALEn, Data.Vector.fromList [" sclaen n=2"]),(MASTER, Data.Vector.fromList [" 1 2 3 4 5 6 7 8"])]
                       }

wrongPDBSpecP :: Spec
wrongPDBSpecP = describe "Wrong pdb (no KEYWDS)." $
    it "should fail and return Left" $ do
    let mt = fromTextPDB $ T.pack   ( "HEADER header\n" ++
                                      "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                      "COMPND compnd\n" ++
                                      "SOURCE source\n" ++
                                      "EXPDTA expdta\n" ++
                                      "AUTHOR  Masha\n" ++
                                      "REVDAT revdat\n" ++
                                      "REMARK 1 REFERENCE 1\n" ++
                                      "SEQRES seqres\n" ++
                                      "CRYST1 cryst1\n" ++
                                      "ORIGX1 origx1 n=1\n" ++
                                      "SCALE2 sclaen n=2\n" ++
                                      "CONECT conect\n" ++
                                      "CONECT conect conect\n" ++
                                      "MASTER 1 2 3 4 5 6 7 8\n"
                                      )
    mt `shouldBe` Left "string"

fileSpecP :: FilePath -> IO()
fileSpecP path = fromFilePDB path >>= print
