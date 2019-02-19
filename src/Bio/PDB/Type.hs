{-# LANGUAGE DuplicateRecordFields #-}
module Bio.PDB.Type where

import           Data.Text                      ( Text )
import           Data.Array                     ( Array )

data Header = Header { classification :: Text
                     , depDate        :: Text
                     , idCode         :: Text
                     }

data Obsoleted = Obsoleted { repDate      :: Text
                           , idCode       :: Text
                           , rIdCode      :: Array Int Text
                           }

data Caveat = Caveat { idCode       :: Text
                     , comment      :: Text
                     }

data Compound = Compound { molId            :: Text
                         , molecule         :: Text
                         , chain            :: Array Int Text
                         , fragment         :: Text
                         , synonym          :: Array Int Text
                         , enzymeCommission :: Array Int Int
                         , engineered       :: Bool
                         , mutation         :: Bool
                         , otherDetails     :: Array Int Text
                         }

data Organism = Organism { scientific :: Text
                         , common     :: Text
                         , taxId      :: Text
                         }

data ExpressionSystem = ExpressionSystem { name             :: Text
                                         , common           :: Text
                                         , taxId            :: Text
                                         , strain           :: Text
                                         , variant          :: Text
                                         , cellLine         :: Text
                                         , atcc             :: Text
                                         , organ            :: Text
                                         , tissue           :: Text
                                         , cell             :: Text
                                         , organelle        :: Text
                                         , cellularLocation :: Text
                                         , vectorType       :: Text
                                         , vector           :: Text
                                         , plasmid          :: Text
                                         , gene             :: Text
                                         , otherDetails     :: Text
                                         }

data Source = Source { molId            :: Text
                     , synthetic        :: Text
                     , fragment         :: Text
                     , organism         :: Organism
                     , strain           :: Text
                     , variant          :: Text
                     , cellLine         :: Text
                     , atcc             :: Text
                     , tissue           :: Text
                     , cell             :: Text
                     , organelle        :: Text
                     , secretion        :: Text
                     , cellularLocation :: Text
                     , plasmid          :: Text
                     , gene             :: Text
                     , expressionSystem :: ExpressionSystem
                     , otherDetails     :: Text
                     }

--
data Title = Title { header    :: Header
                   , obsoleted :: Obsoleted
                   , title     :: Text
                   , split     :: Array Int Text
                   , caveat    :: Caveat
                   , compound  :: Array Int Compound
                   -- TODO
                   }

data PrimaryStructure = PrimaryStructure -- TODO

data Heterogen = Heterogen -- TODO

data Helix = Helix {

                   }

data Sheet = Sheet {

                   }

data Secondary = Secondary { helixes :: Array Int Helix
                           , sheets  :: Array Int Sheet
                           }

data ConnectivityAnnotation = ConnectivityAnnotation -- TODO

data Miscellaneous = Miscellaneous -- TODO

data Transformation = Transformation -- TODO

data Coordinate = Coordinate {

                             }

data Connectivity = Connectivity -- TODO

data Bookkeeping = Bookkeeping -- TODO

data PDB = PDB { title                  :: Title
               , primaryStructure       :: PrimaryStructure
               , heterogen              :: Heterogen
               , secondary              :: Secondary
               , connectivityAnnotation :: ConnectivityAnnotation
               , miscellaneous          :: Miscellaneous
               , transformation         :: Transformation
               , coordinate             :: Coordinate
               , connectivity           :: Connectivity
               , bookkeeping            :: Bookkeeping
               }