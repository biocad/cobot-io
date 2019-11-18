{-# LANGUAGE OverloadedStrings #-}

module PDBSpec where

import           Bio.PDB      
import           Bio.PDB.Type (Atom (..), FieldData, FieldType, PDB(..))
import           Data.Text    as T
import           Test.Hspec
import           Bio.PDB.Parser (hetatmP)
import Data.Array (Array)
import Data.Map.Strict (Map)
import Debug.Trace


dottedAtomSpecP :: Text -> IO ()
dottedAtomSpecP path = let
                        -- mt = fromTextChain $ T.pack "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \nATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n"
                        mt = fromTextChain $ T.pack   ( "ATOM   2032  OXT CYS A 214      -4.546 -29.673  26.796  1.0 143.51           O  \n" ++
                                                        "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++ 
                                                        "TER    2034      CYS A 214                                                      \n" ++
                                                        "ATOM   2035  N   GLU B   1      18.637 -61.583  66.852  1.0 118.48           N  \n" ++ 
                                                        "ATOM   2036  CA  GLU B   1      19.722 -62.606  66.868  1.00 19.77           C  \n" ++
                                                        "ATOM   2037  C   GLU B   1      19.628 -63.429  65.597  1.0 110.34           C  \n" ++
                                                        "ATOM  12531 HH12 ARG D 474      45.558 -39.551 -49.936  1.00 15.00           H  \n" ++
                                                        "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                                        "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++ 
                                                        "TER   12534      ARG D 474                                                      \n" ++
                                                        "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                                        "HETATM12536  C2  NAG B 475       6.100 -18.729  -7.466  1.00 33.16           C  \n" ++
                                                        "HETATM12537  C3  NAG B 475       5.934 -18.550  -8.975  1.00 21.03           C  \n" ++
                                                        "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n")
                        
                        
                        in print mt

testModelsSpecP :: Text -> IO ()
testModelsSpecP path = do 
                    let mt = fromTextChain $ T.pack ("HELLO") in print mt
                    let
                        mt = fromTextChain $ T.pack   ( "MODEL  4  \n" ++
                                                        "ATOM   2032  OXT CYS A 214      -114.546-229.673-526.796  1.0 143.51           O  \n" ++
                                                        "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++ 
                                                        "TER    2034      CYS A 214                                                      \n" ++
                                                        "ENDMDL\n" ++
                                                        "MODEL  5 \n" ++ 
                                                        "ATOM   2035  N   GLU B   1      18.637-691.583  66.852  1.0 118.48           N  \n" ++ 
                                                        "ATOM   2036  CA  GLU B   1      19.722 -62.606  66.868  1.00 19.77           C  \n" ++
                                                        "ATOM   2037  C   GLU B   1      19.628 -63.429  65.597  1.0 110.34           C  \n" ++
                                                        "ATOM  12531 HH12 ARG D 474      45.558 -39.551 -49.936  1.00 15.00           H  \n" ++
                                                        "ENDMDL\n" ++
                                                        "MODEL  6 \n" ++ 
                                                        "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                                        "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++ 
                                                        "TER   12534      ARG D 474                                                      \n" ++
                                                        "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                                        "HETATM12536  C2  NAG B 475       6.100 -18.729  -7.466  1.00 33.16           C  \n" ++
                                                        "HETATM12537  C3  NAG B 475       5.934 -18.550  -8.975  1.00 21.03           C  \n" ++
                                                        "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++ 
                                                        "ENDMDL\n")
                        
                        
                        in print mt
                    let onemodel = fromTextChain $ T.pack ( "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                                            "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++ 
                                                            "TER   12534      ARG D 474                                                      \n" ++
                                                            "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                                            "HETATM12536  C2  NAG B 475       6.100 -18.729  -7.466  1.00 33.16           C  \n" ++
                                                            "HETATM12537  C3  NAG B 475       5.934 -18.550  -8.975  1.00 21.03           C  \n" ++
                                                            "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" 
                                                         )
                     in print onemodel



titleSpecP :: Text -> IO ()
titleSpecP path = let mt = fromTextTitle $ T.pack   ( "TITLE     RHIZOPUSPEPSIN COMPLEXED WITH REDUCED PEPTIDE INHIBITOR\n" ++
                                                        "TITLE     STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME BY                  \n" ++
                                                        "TITLE    2 CONTROLLED DEHYDRATION   \n" ++
                                                        "TITLE     NMR STUDY  OF OXIDIZED THIOREDOXIN MUTANT (C62A,C69A,C73A)\n" ++
                                                        "TITLE    2 MINIMIZED  AVERAGE STRUCTURE\n"
                                                         )
                        
                        in print mt

remarkSpecP :: Text -> IO ()
remarkSpecP path = let mt = fromTextRemark $ T.pack   ( "REMARK   1    \n" ++                                                                   
                                                        "REMARK   1 REFERENCE 1\n" ++                                                          
                                                        "REMARK   1  AUTH   L.J.HARRIS,S.B.LARSON,K.W.HASEL,J.DAY,A.GREENWOOD,       \n"  ++
                                                        "REMARK 500    SER A 153       42.86   -158.93     \n" ++
                                                        "REMARK 800 SITE_DESCRIPTION: BINDING SITE FOR RESIDUE MAN D 482                 \n" ++
                                                        "REMARK 800                                                                      \n" ++
                                                        "REMARK 800 SITE_IDENTIFIER: BC9                                                 \n" ++
                                                        "REMARK 800 EVIDENCE_CODE: SOFTWARE                                              \n" ++
                                                        "REMARK 800 SITE_DESCRIPTION: BINDING SITE FOR RESIDUE NAG D 483                 \n" ++
                                                        "REMARK 999                                                                      \n" ++
                                                        "REMARK 999 SEQUENCE                                                             \n" ++
                                                        "REMARK 999 THE INTACT ANTIBODY IS NUMBERED ACCORDING TO THE CONVENTION          \n" ++
                                                        "REMARK 999 OF E. KABAT [KABAT ET AL. (1991) SEQUENCES OF PROTEINS OF            \n" ++
                                                        "REMARK 999 IMMUNOLOGICAL INTEREST, 5TH ED., NATIONAL INSTITUTES OF              \n" ++
                                                        "REMARK 999 HEALTH, BETHESDA, MD].\n"
                                                       )
                        
                        in print mt
                                         

pdbSpecP :: Text -> IO ()
pdbSpecP path = let mt = fromTextPDB $ T.pack   ( "HEADER   1    \n" ++  
                                                  "OBSLTE 999         \n" ++
                                                   "TITLE 999 HEALTH, BETHESDA, MD].\n" ++ 
                                                   "CAVEAT caveat\n" ++
                                                   "COMPND compnd\n" ++
                                                   "SOURCE source\n" ++
                                                   "KEYWDS k\n" ++
                                                   "EXPDTA  exp \n" ++
                                                   "NUMMDL num\n" ++
                                                   "AUTHOR r \n" ++
                                                   "REVDATrevdat\n" ++
                                                   "REMARK 1  r11\n" ++
                                                   "REMARK 1  r12\n" ++
                                                   "REMARK 2  r2\n" ++
                                                   "SEQRES  seq\n" ++
                                                   "CRYST1 \n" ++ 
                                                   "ORIGXn origxn\n" ++
                                                   "SCALEn scalen \n" ++  
                                                   "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                                   "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++ 
                                                   "TER   12534      ARG D 474                            \n" ++
                                                   "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                                   "CONECT \n" ++
                                                   "CONECT fgfhfdhf\n" ++
                                                   "MASTER 1 2 3 4 \n"         
                                                )
                        
                        in print mt

pdbManyModelP :: Text -> IO ()
pdbManyModelP path = let mt = fromTextPDB $ T.pack   ( "HEADER   1    \n" ++  
                                                   "TITLE 999 HEALTH, BETHESDA, MD].\n" ++ 
                                                   "CAVEAT caveat\n" ++
                                                   "COMPND compnd\n" ++
                                                   "SOURCE source\n" ++
                                                   "KEYWDS k\n" ++
                                                   "EXPDTA  exp \n" ++
                                                   "NUMMDL num\n" ++
                                                   "AUTHOR r \n" ++
                                                   "REVDATrevdat\n" ++
                                                   "REMARK 1  r11\n" ++
                                                   "REMARK 1  r12\n" ++
                                                   "REMARK 2  r2\n" ++
                                                   "SEQRES  seq\n" ++
                                                   "CRYST1 \n" ++ 
                                                   "ORIGXn origxn\n" ++
                                                   "SCALEn scalen \n" ++
                                                   "MODEL  4  \n" ++
                                                   "ATOM   2033  H   CYS A 214      -6.124 -27.225  26.558  1.00 15.00           H  \n" ++ 
                                                   "TER    2034      CYS A 214                                                      \n" ++
                                                   "ENDMDL\n" ++
                                                   "MODEL  5 \n" ++ 
                                                   "ATOM   2036  CA  GLU B   1      19.722  62.606  66.868  1.00 19.77           C  \n" ++
                                                   "ATOM   2037  C   GLU B   1      19.628 -63.429  65.597  1.0 110.34           C  \n" ++
                                                   "ATOM  12531 HH12 ARG D 474      45.558 -39.551 -49.936  1.00 15.00           H  \n" ++
                                                   "ENDMDL\n" ++
                                                   "MODEL  6 \n" ++ 
                                                   "ATOM  12532 HH21 ARG D 474      47.457 -38.007 -47.445  1.00 15.00           H  \n" ++
                                                   "ATOM  12533 HH22 ARG D 474      47.405 -39.268 -48.629  1.00 15.00           H  \n" ++ 
                                                   "TER   12534      ARG D 474                                                      \n" ++
                                                   "HETATM12535  C1  NAG B 475       5.791 -20.194  -7.051  1.00 34.66           C  \n" ++
                                                   "HETATM12536  C2  NAG B 475       6.100 -18.729  -7.466  1.00 33.16           C  \n" ++
                                                   "HETATM12537  C3  NAG B 475       5.934 -18.550  -8.975  1.00 21.03           C  \n" ++
                                                   "HETATM12538  C4  NAG B 475       6.943 -19.507  -9.597  1.00 25.87           C  \n" ++ 
                                                   "ENDMDL\n" ++ 
                                                   "CONECT \n" ++
                                                   "CONECT fgfhfdhf\n" ++
                                                   "MASTER 1 2 3 4 \n"         
                                                )
                        
                        in print mt
                     
pdbFileP :: FilePath -> FilePath -> IO()
pdbFileP pathIn pathOut = do 
    mt <- fromFilePDB pathIn
    toFile mt pathOut
    print mt
