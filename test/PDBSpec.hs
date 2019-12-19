{-# LANGUAGE OverloadedStrings #-}

module PDBSpec where

import           Bio.PDB.Reader    (PDBWarnings (..), fromTextPDB)
import           Bio.PDB.Type      (Atom (..), FieldType (..), PDB (..))
import           Control.Exception (evaluate)
import           Control.Exception (ArithException, ErrorCall, IOException,
                                    SomeException)
import qualified Data.Map.Strict   (empty, fromList, singleton)
import           Data.Text         as T (Text, intercalate, length, lines, pack,
                                         replicate, take)
import qualified Data.Vector       as V (empty, fromList, singleton)
import           Test.Hspec


oneModelSpecP :: Spec
oneModelSpecP = describe "One model." $
        it "correctly parses pdb with only one model without strings \"MODEL\" & \"ENDMDL\"" $ do
        let mt  = fromTextPDB . lenghtenLines $ T.pack ( "HEADER header\n" ++
                                         "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                         "COMPND compnd\n" ++
                                         "SOURCE source\n" ++
                                         "KEYWDS keywds\n" ++
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
        mt `shouldBe` Right ([], Right oneModelPDB)

oneModelPDB :: PDB
oneModelPDB =  PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                   , remarks = Data.Map.Strict.singleton (Just 1) (V.singleton "REFERENCE 1")
                   , models = V.singleton $ V.fromList [V.fromList [Atom {atomSerial = 2032, atomName = " OXT", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ',
                                                                           atomX = -4.546, atomY = -29.673, atomZ = 26.796, atomOccupancy = 1.0, atomTempFactor = 143.51, atomElement = " O",
                                                                           atomCharge = "  "},
                                                                           Atom {atomSerial = 2033, atomName = " H  ", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A',
                                                                           atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225, atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0,
                                                                           atomElement = " H", atomCharge = "  "}],
                                                                           V.fromList [Atom {atomSerial = 2035, atomName = " N  ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                           atomX = 18.637, atomY = -61.583, atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = " N", atomCharge = "  "},
                                                                           Atom {atomSerial = 2036, atomName = " CA ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                           atomX = 19.722, atomY = -62.606, atomZ = 66.868, atomOccupancy = 1.0, atomTempFactor = 19.77, atomElement = " C", atomCharge = "  "}]
                                                                        ]
                  , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source"]),(KEYWDS,V.fromList[" keywds"]),(AUTHOR,V.fromList["  Masha"]),(REVDAT,V.fromList[" revdat"]),(SEQRES,V.fromList [" seqres"]),
                                                            (CRYST1,V.fromList [" cryst1"]),(ORIGX1,V.fromList[" origx1 n=1"]),(SCALE2,V.fromList [" sclaen n=2"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                   }

manyModelsSpecP :: Spec
manyModelsSpecP = describe "Some models." $
        it "correctly parses pdb with many models - they have strings \"MODEL\" & \"ENDMDL\" and text has disordered strings" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "HEADER header\n" ++
                                        "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                        "COMPND compnd\n" ++
                                        "SOURCE source\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "REMARK 2   Reference 2_1/1\n" ++
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
                                        "CONECT conect conect\n" ++
                                        "CONECT conect\n" ++
                                        "ENDMDL\n" ++
                                        "SEQRES seqres\n" ++
                                        "KEYWDS keywds\n" ++
                                        "EXPDTA expdta\n" ++
                                        "AUTHOR  Masha\n" ++
                                        "REVDAT revdat\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n"
                                      )
        mt `shouldBe` Right ([], Right manyModelsPDB)

manyModelsPDB :: PDB
manyModelsPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                    , remarks = Data.Map.Strict.fromList [(Just 1, V.singleton "REFERENCE 1"), (Just 2, V.singleton "Reference 2_1/1")]
                    , models = V.fromList [V.fromList [V.fromList [Atom {atomSerial = 2033, atomName = " H  ", atomAltLoc = ' ', atomResName = "CYS",
                                                                                                  atomChainID = 'A', atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225,
                                                                                                  atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                          ],
                                                    V.fromList [V.fromList [Atom {atomSerial = 2035, atomName = " N  ", atomAltLoc = ' ', atomResName = "GLU",
                                                                                                 atomChainID = 'B', atomResSeq = 1, atomICode = ' ', atomX = 18.637, atomY = -691.583,
                                                                                                 atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = " N", atomCharge = "  "}],
                                                                          V.fromList [Atom {atomSerial = 12531, atomName = "HH12", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                 atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 45.558, atomY = -39.551, atomZ = -49.936,
                                                                                                 atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                         ],
                                                    V.fromList [V.fromList [Atom {atomSerial = 12532, atomName = "HH21", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                 atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.457, atomY = -38.007, atomZ = -47.445,
                                                                                                 atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "},
                                                                                                Atom {atomSerial = 12533, atomName = "HH22", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                 atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.405, atomY = -39.268, atomZ = -48.629,
                                                                                                 atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                          ]
                                                      ]
                    , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source"]),(KEYWDS,V.fromList[" keywds"]),(EXPDTA,V.fromList[" expdta"]),(AUTHOR,V.fromList["  Masha"]),(REVDAT,V.fromList[" revdat"]),(SEQRES,V.fromList [" seqres"]),
                                                              (CRYST1,V.fromList [" cryst1"]),(ORIGX1,V.fromList[" origx1 n=1"]),(SCALE2,V.fromList [" sclaen n=2"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                    }

noModelsSpecP :: Spec
noModelsSpecP = describe "No models." $
        it "correctly parses pdb without models (no ATOM, TER, HETATM strings)" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "HEADER header\n" ++
                                        "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                        "COMPND compnd\n" ++
                                        "SOURCE source\n" ++
                                        "KEYWDS keywds\n" ++
                                        "EXPDTA expdta\n" ++
                                        "AUTHOR  Masha\n" ++
                                        "REVDAT revdat\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "SEQRES seqres\n" ++
                                        "CRYST1 cryst1\n" ++
                                        "ORIGX1 origx1 n=1\n" ++
                                        "SCALE2 sclaen n=2\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n"
                                      )
        mt `shouldBe` Right ([], Right noModelsPDB)

noModelsPDB :: PDB
noModelsPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                  , remarks = Data.Map.Strict.singleton (Just 1) (V.singleton "REFERENCE 1")
                  , models = V.empty
                  , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source"]),(KEYWDS,V.fromList[" keywds"]),(EXPDTA,V.fromList[" expdta"]),(AUTHOR,V.fromList["  Masha"]),(REVDAT,V.fromList[" revdat"]),(SEQRES,V.fromList [" seqres"]),
                                                            (CRYST1,V.fromList [" cryst1"]),(ORIGX1,V.fromList[" origx1 n=1"]),(SCALE2,V.fromList [" sclaen n=2"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                  }

allFieldsModelSpecP :: Spec
allFieldsModelSpecP = describe "PDB with all strings." $
        it "correctly parses pdb with all types of string" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "HEADER header\n" ++
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
                                        "JRNL   jrnl\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "DBREF dbref\n" ++
                                        "DBREF1 dbref1\n" ++
                                        "DBREF2 dbref2_1\n" ++
                                        "DBREF2 dbref2_2\n" ++
                                        "SEQADV seqadv\n" ++
                                        "SEQRES seqres\n" ++
                                        "MODRES modres\n" ++
                                        "HET    het\n" ++
                                        "HETNAM hetnam\n" ++
                                        "HETSYN hetsyn\n" ++
                                        "FORMUL of love\n" ++
                                        "HELIX helix\n" ++
                                        "SHEET sheet\n" ++
                                        "SSBOND ssbond\n" ++
                                        "LINK   link\n" ++
                                        "CISPEP cispep\n" ++
                                        "SITE   site\n" ++
                                        "CRYST1 cryst1\n" ++
                                        "ORIGX1 origx1 n=1\n" ++
                                        "SCALE2 sclaen n=2\n" ++
                                        "MTRIX3 matrixn n=3\n" ++
                                        "MODEL 1\n" ++
                                        "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                        "TER   12534      ARG D 474                                                      \n" ++
                                        "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                        "CONECT conect\n" ++
                                        "CONECT conect conect\n" ++
                                        "ENDMDL\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n" ++
                                        "END"
                                      )
        mt `shouldBe` Right ([], Right pdbWithAllFields)

pdbWithAllFields :: PDB
pdbWithAllFields = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                       , models = V.fromList [V.fromList [V.singleton Atom {atomSerial = 12532, atomName = "HH21", atomAltLoc = ' ', atomResName = "ARG",
                                     atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.457, atomY = -38.007, atomZ = -47.445,
                                     atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]]
                       , remarks = Data.Map.Strict.singleton (Just 1) (V.singleton "REFERENCE 1")
                       , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(OBSLTE, V.fromList [" obslte"]), (SPLIT, V.fromList ["split"]), (CAVEAT, V.fromList [" caveat"]),
                                                                 (COMPND, V.fromList [" compnd"]),(SOURCE, V.fromList [" source"]),(KEYWDS, V.fromList [" keywds"]),
                                                                 (EXPDTA, V.fromList [" expdta1"," expdta2"]),(NUMMDL, V.fromList [" nummdl"]),(MDLTYP, V.fromList [" mdltyp mdltyp"]),
                                                                 (AUTHOR, V.fromList ["  Masha"]),(REVDAT, V.fromList [" revdat"]),(SPRSDE, V.fromList [" sprsde"]),
                                                                 (JRNL, V.fromList [" jrnl"]),(DBREF, V.fromList ["dbref"]), (DBREF1, V.fromList [" dbref1"]), (DBREF2, V.fromList [" dbref2_1"," dbref2_2"]),
                                                                 (SEQADV, V.fromList [" seqadv"]),(SEQRES, V.fromList [" seqres"]),(MODRES, V.fromList [" modres"]),
                                                                 (HET, V.fromList [" het"]), (HETNAM, V.fromList [" hetnam"]), (HETSYN, V.fromList [" hetsyn"]),(FORMUL, V.fromList [" of love"]),(HELIX, V.fromList ["helix"]),
                                                                 (SHEET, V.fromList ["sheet"]),(SSBOND, V.fromList [" ssbond"]),(LINK, V.fromList [" link"]),
                                                                 (CISPEP, V.fromList [" cispep"]),(SITE,V.fromList [" site"]),(CRYST1, V.fromList [" cryst1"]),(MTRIX3, V.fromList [" matrixn n=3"]),
                                                                 (ORIGX1, V.fromList [" origx1 n=1"]),(SCALE2, V.fromList [" sclaen n=2"]),(MASTER, V.fromList [" 1 2 3 4 5 6 7 8"])]
                       }


emptySpecP :: Spec
emptySpecP = describe "empty PDB." $
        it "correctly parses empty pdb" $ do
        let mt = fromTextPDB ""
        mt `shouldBe` Right ([], Right emptyPdb)

emptyPdb :: PDB
emptyPdb = PDB { title = ""
               , models = V.empty
               , remarks = Data.Map.Strict.empty
               , otherFields = Data.Map.Strict.empty
               }

trashBetweenModelsSpecP :: Spec
trashBetweenModelsSpecP = describe "PDB has trash." $
        it "correctly parses pdb with trash string between models and other field strings" $ do
        let mt = fromTextPDB  . lenghtenLines $ T.pack ( "trash strings 1\n" ++
                                        "HEADER header\n" ++
                                        "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "trash strings 2\n" ++
                                        "REMARK 2   Reference 2_1/1\n" ++
                                        "MODEL  4  \n" ++
                                        "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++
                                        "TER    2034      CYS A 214                                                      \n" ++
                                        "ANISOU anisou\n" ++
                                        "ENDMDL \n" ++
                                        "CRYST1 cryst1\n" ++
                                        "trash strings 3\n" ++
                                        "SEQRES seqres\n" ++
                                        "MODEL  5 \n" ++
                                        "ATOM   2035  N   GLU B   1      18.637-691.583  66.852  1.0 118.48           N  \n" ++
                                        "ATOM  12531 HH12 ARG D 474      45.558 -39.551 -49.936  1.00 15.00           H  \n" ++
                                        "ENDMDL \n" ++
                                        "trash strings 4\n" ++
                                        "MODEL  6 \n" ++
                                        "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                        "TER   12534      ARG D 474                                                      \n" ++
                                        "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++
                                        "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                        "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++
                                        "CONECT conect conect\n" ++
                                        "ENDMDL\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n" ++
                                        "trash strings 5\n"
                                       )
        mt `shouldBe` Left "There are trash strings between model strings"

pdbWithoutTrash :: PDB
pdbWithoutTrash = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                      , remarks = Data.Map.Strict.fromList [(Just 1, V.singleton "REFERENCE 1"), (Just 2, V.singleton "Reference 2_1/1")]
                      , models = V.fromList [V.fromList [V.fromList [Atom {atomSerial = 2033, atomName = " H  ", atomAltLoc = ' ', atomResName = "CYS",
                                                                                                    atomChainID = 'A', atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225,
                                                                                                    atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                            ],
                                                      V.fromList [V.fromList [Atom {atomSerial = 2035, atomName = " N  ", atomAltLoc = ' ', atomResName = "GLU",
                                                                                                   atomChainID = 'B', atomResSeq = 1, atomICode = ' ', atomX = 18.637, atomY = -691.583,
                                                                                                   atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = " N", atomCharge = "  "}],
                                                                            V.fromList [Atom {atomSerial = 12531, atomName = "HH12", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 45.558, atomY = -39.551, atomZ = -49.936,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                           ],
                                                      V.fromList [V.fromList [Atom {atomSerial = 12532, atomName = "HH21", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.457, atomY = -38.007, atomZ = -47.445,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "},
                                                                                                  Atom {atomSerial = 12533, atomName = "HH22", atomAltLoc = ' ', atomResName = "ARG",
                                                                                                   atomChainID = 'D', atomResSeq = 474, atomICode = ' ', atomX = 47.405, atomY = -39.268, atomZ = -48.629,
                                                                                                   atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "}]
                                                                            ]
                                                        ]
                      , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(SEQRES,V.fromList [" seqres"]),
                                                                (CRYST1,V.fromList [" cryst1"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                      }

onlyOneModelSpecP :: Spec
onlyOneModelSpecP = describe "Only One model." $
        it "correctly parses pdb with only one model without other field/title/trash/remarks strings" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "ATOM   2032  OXT CYS A 214      -4.546 -29.673  26.796  1.0 143.51           O  \n" ++
                                         "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++
                                         "TER    2034      CYS A 214                                                      \n" ++
                                         "ATOM   2035  N   GLU B   1      18.637 -61.583  66.852  1.0 118.48           N  \n" ++
                                         "TER   12534      ARG D 474                                                      \n" ++
                                         "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                         "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++
                                         "ATOM   2036  CA  GLU B   1      19.722 -62.606  66.868  1.00 19.77           C  \n"
                                        )
        mt `shouldBe` Right ([], Right onlyOneModelPDB)

onlyOneModelPDB :: PDB
onlyOneModelPDB = PDB { title = ""
                      , remarks = Data.Map.Strict.empty
                      , models = V.singleton $ V.fromList [V.fromList [Atom {atomSerial = 2032, atomName = " OXT", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ',
                                                                              atomX = -4.546, atomY = -29.673, atomZ = 26.796, atomOccupancy = 1.0, atomTempFactor = 143.51, atomElement = " O",
                                                                              atomCharge = "  "},
                                                                              Atom {atomSerial = 2033, atomName = " H  ", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A',
                                                                              atomResSeq = 214, atomICode = ' ', atomX = -6.124, atomY = -27.225, atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0,
                                                                              atomElement = " H", atomCharge = "  "}],
                                                                              V.fromList [Atom {atomSerial = 2035, atomName = " N  ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                              atomX = 18.637, atomY = -61.583, atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = " N", atomCharge = "  "},
                                                                              Atom {atomSerial = 2036, atomName = " CA ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                                                                              atomX = 19.722, atomY = -62.606, atomZ = 66.868, atomOccupancy = 1.0, atomTempFactor = 19.77, atomElement = " C", atomCharge = "  "}]
                                                                              ]
                      , otherFields = Data.Map.Strict.empty
                      }

repeatedStringsSpecP :: Spec
repeatedStringsSpecP = describe "PDB with repeated other field strings." $
        it "correctly parses pdb with repeated other field strings (SOURCE)" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "HEADER header\n" ++
                                        "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                        "COMPND compnd\n" ++
                                        "EXPDTA expdta\n" ++
                                        "AUTHOR  Masha\n" ++
                                        "SOURCE source1\n" ++
                                        "KEYWDS keywds\n" ++
                                        "REVDAT revdat\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "SEQRES seqres\n" ++
                                        "CRYST1 cryst1\n" ++
                                        "ORIGX1 origx1 n=1\n" ++
                                        "SOURCE source2\n" ++
                                        "SCALE2 sclaen n=2\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n" ++
                                        "SOURCE source3\n" ++
                                        "END"
                                      )
        mt `shouldBe` Right ([], Right repeatedStringsPDB)

repeatedStringsPDB :: PDB
repeatedStringsPDB = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                         , remarks = Data.Map.Strict.singleton (Just 1) (V.singleton "REFERENCE 1")
                         , models = V.empty
                         , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source1", " source2", " source3"]),(KEYWDS,V.fromList[" keywds"]),(EXPDTA,V.fromList[" expdta"]),(AUTHOR,V.fromList["  Masha"]),(REVDAT,V.fromList[" revdat"]),(SEQRES,V.fromList [" seqres"]),
                                                                   (CRYST1,V.fromList [" cryst1"]),(ORIGX1,V.fromList[" origx1 n=1"]),(SCALE2,V.fromList [" sclaen n=2"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                         }

emptyRemarkSpecP :: Spec
emptyRemarkSpecP = describe "PDB with repeated remark strings without code." $
        it "correctly parses pdb with repeated remark strings without code" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "REMARK 111 remark111_1/2\n" ++
                                        "COMPND compnd\n" ++
                                        "SOURCE source1\n" ++
                                        "KEYWDS keywds\n" ++
                                        "REMARK 111 remark111_2/2\n" ++
                                        "REVDAT revdat\n" ++
                                        "REMARK 2   remark2_1/1\n" ++
                                        "SOURCE source2\n" ++
                                        "REMARK     empty remark 1/2\n" ++
                                        "END    \n" ++
                                        "REMARK     empty remark 2/2\n" ++
                                        "SCALE2 sclaen n=2\n"
                                      )
        mt `shouldBe` Right ([], Right pdbWithEmptyRemarks)

pdbWithEmptyRemarks :: PDB
pdbWithEmptyRemarks = PDB { title = ""
                         , remarks = Data.Map.Strict.fromList [(Just 2, V.fromList ["remark2_1/1"]), (Just 111, V.fromList ["remark111_1/2", "remark111_2/2"]), (Nothing, V.fromList ["empty remark 1/2", "empty remark 2/2"])]
                         , models = V.empty
                         , otherFields = Data.Map.Strict.fromList [(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source1", " source2"]),
                                                                    (KEYWDS,V.fromList[" keywds"]), (REVDAT,V.fromList[" revdat"]), (SCALE2,V.fromList [" sclaen n=2"])]
                         }

emptyModelSpecP :: Spec
emptyModelSpecP = describe "PDB with one empty model." $
        it "correctly parses pdb with one model without strings inside" $ do
        let mt = fromTextPDB . lenghtenLines $ T.pack ( "trash strings 1\n" ++
                                        "HEADER header\n" ++
                                        "TITLE STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME\n" ++
                                        "REMARK 1   REFERENCE 1\n" ++
                                        "trash strings 2\n" ++
                                        "REMARK 2   Reference 2_1/1\n" ++
                                        "MODEL  4  \n" ++
                                        "ENDMDL\n" ++
                                        "MASTER 1 2 3 4 5 6 7 8\n" ++
                                        "trash strings 5\n"
                                      )
        mt `shouldBe` Right ([], Right pdbEmptyModel)


pdbEmptyModel :: PDB
pdbEmptyModel = PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                    , remarks = Data.Map.Strict.fromList [(Just 1, V.singleton "REFERENCE 1"), (Just 2, V.singleton "Reference 2_1/1")]
                    , models = V.empty
                    , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),
                                                              (MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                    }

lenghtenLines :: Text -> Text
lenghtenLines text = longLinedText
   where
       textLines = T.lines text
       longTextLines = changeLine <$> textLines
       desiredLength = 80  -- cause it is max length in standart pdb file
       longLinedText = T.intercalate "\n" longTextLines

       changeLine :: Text -> Text
       changeLine line | T.length line > desiredLength = T.take desiredLength line
                       | T.length line < desiredLength = line <> T.replicate spacesCount " "
                       | otherwise = line
            where
                    spacesCount = desiredLength - T.length line

