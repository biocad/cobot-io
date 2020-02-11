module Bio.MMTF.Decode where

import           Bio.MMTF.Decode.Codec
import           Bio.MMTF.Decode.MessagePack
import           Bio.MMTF.Type

import           Control.Monad               ((>=>))
import           Data.ByteString.Lazy        (empty)
import           Data.Char                   (ord)
import           Data.Map.Strict             (Map)
import           Data.MessagePack            (Object)
import           Data.Text                   (Text, pack)
import           Data.Vector                 (Vector, fromList)

-- | Parses format data from ObjectMap
--
formatData :: MonadFail m => Map Text Object -> m FormatData
formatData mp = do v <- atP mp "mmtfVersion"  asStr
                   p <- atP mp "mmtfProducer" asStr
                   pure $ FormatData v p

-- | Parses model data from ObjectMap
--
modelData :: MonadFail m => Map Text Object -> m ModelData
modelData mp = ModelData . l2v <$> atP mp "chainsPerModel" asIntList

-- | Parses chain data from ObjectMap
--
chainData :: MonadFail m => Map Text Object -> m ChainData
chainData mp = do gpc <- atP mp "groupsPerChain" asIntList
                  cil <- codec5 . parseBinary <$> atP   mp "chainIdList"   asBinary
                  cnl <- codec5 . parseBinary <$> atPMD mp "chainNameList" asBinary empty
                  pure $ ChainData (l2v gpc) (l2v cil) (l2v cnl)

-- | Parses atom data from ObjectMap
--
atomData :: MonadFail m => Map Text Object -> m AtomData
atomData mp = do ail' <-       codec8 . parseBinary <$> atPMD mp "atomIdList"    asBinary empty
                 all' <- c2s . codec6 . parseBinary <$> atPMD mp "altLocList"    asBinary empty
                 bfl' <-      codec10 . parseBinary <$> atPMD mp "bFactorList"   asBinary empty
                 xcl' <-      codec10 . parseBinary <$> atP   mp "xCoordList"    asBinary
                 ycl' <-      codec10 . parseBinary <$> atP   mp "yCoordList"    asBinary
                 zcl' <-      codec10 . parseBinary <$> atP   mp "zCoordList"    asBinary
                 ol' <-        codec9 . parseBinary <$> atPMD mp "occupancyList" asBinary empty
                 pure $ AtomData (l2v ail') (l2v all') (l2v bfl') (l2v xcl') (l2v ycl') (l2v zcl') (l2v ol')

-- | Parses group data from ObjectMap
--
groupData :: MonadFail m => Map Text Object -> m GroupData
groupData mp = do gl' <-                                        atP   mp "groupList"          asObjectList >>= traverse (transformObjectMap >=> groupType)
                  gtl' <-              codec4 . parseBinary <$> atP   mp "groupTypeList"     asBinary
                  gil' <-              codec8 . parseBinary <$> atP   mp "groupIdList"       asBinary
                  ssl' <- fmap ssDec . codec2 . parseBinary <$> atPMD mp "secStructList"     asBinary empty
                  icl' <-        c2s . codec6 . parseBinary <$> atPMD mp "insCodeList"       asBinary empty
                  sil' <-              codec8 . parseBinary <$> atPMD mp "sequenceIndexList" asBinary empty
                  pure $ GroupData (l2v gl') (l2v gtl') (l2v gil') (l2v ssl') (l2v icl') (l2v sil')

-- | Parses group type from ObjectMap
--
groupType :: MonadFail m => Map Text Object -> m GroupType
groupType mp = do fcl' <-          atP mp "formalChargeList" asIntList
                  anl' <-          atP mp "atomNameList"     asStrList
                  el'  <-          atP mp "elementList"      asStrList
                  bal' <- l2pl <$> atP mp "bondAtomList"     asIntList
                  bol' <-          atP mp "bondOrderList"    asIntList
                  gn'  <-          atP mp "groupName"        asStr
                  slc' <-          atP mp "singleLetterCode" asChar
                  cct' <-          atP mp "chemCompType"     asStr
                  pure $ GroupType (l2v fcl') (l2v anl') (l2v el') (l2v bal') (l2v bol') gn' slc' cct'

-- | Parses structure data from ObjectMap
--
structureData :: MonadFail m => Map Text Object -> m StructureData
structureData mp = do ttl' <-                                  atPMD mp "title"               asStr        ""
                      sid' <-                                  atPMD mp "structureId"         asStr        ""
                      dd'  <-                                  atPMD mp "depositionDate"      asStr        ""
                      rd'  <-                                  atPMD mp "releaseDate"         asStr        ""
                      nb'  <-                                  atP   mp "numBonds"            asInt
                      na'  <-                                  atP   mp "numAtoms"            asInt
                      ng'  <-                                  atP   mp "numGroups"           asInt
                      nc'  <-                                  atP   mp "numChains"           asInt
                      nm'  <-                                  atP   mp "numModels"           asInt
                      sg'  <-                                  atPMD mp "spaceGroup"          asStr        ""
                      uc'  <-                  (>>= ucDec) <$> atPM  mp "unitCell"            asFloatList
                      nol' <-                       m44Dec <$> atPMD mp "ncsOperatorList"     asFloatList []
                      bal' <-                                  atPMD mp "bioAssemblyList"     asObjectList [] >>= traverse (transformObjectMap >=> bioAssembly)
                      el'  <-                                  atPMD mp "entityList"          asObjectList [] >>= traverse (transformObjectMap >=> entity)
                      res' <-                                  atPM  mp "resolution"          asFloat
                      rf'  <-                                  atPM  mp "rFree"               asFloat
                      rw'  <-                                  atPM  mp "rWork"               asFloat
                      em'  <-                                  atPMD mp "experimentalMethods" asStrList []
                      btl' <-  l2pl . codec4 . parseBinary <$> atPMD mp "bondAtomList"        asBinary empty
                      bol' <-         codec2 . parseBinary <$> atPMD mp "bondOrderList"       asBinary empty
                      pure $ StructureData ttl' sid' dd' rd' nb' na'
                                          ng' nc' nm' sg' uc' (l2v nol')
                                           (l2v bal') (l2v el') res' rf'
                                           rw' (l2v em') (l2v btl') (l2v bol')

-- | Parses bio assembly data from ObjectMap
--
bioAssembly :: MonadFail m => Map Text Object -> m Assembly
bioAssembly mp = do nme' <- atP mp "name"          asStr
                    tlt' <- atP mp "transformList" asObjectList >>= traverse (transformObjectMap >=> transform)
                    pure $ Assembly (l2v tlt') nme'

-- | Parses transform data from ObjectMap
--
transform :: MonadFail m => Map Text Object -> m Transform
transform mp = do cil' <- atP mp "chainIndexList" asIntList
                  mtx' <- atP mp "matrix"         asFloatList >>= m44Dec
                  pure $ Transform (l2v cil') mtx'

-- | Parses entity data from ObjectMap
--
entity :: MonadFail m => Map Text Object -> m Entity
entity mp = do cil' <- atP mp "chainIndexList" asIntList
               dsc' <- atP mp "description"    asStr
               tpe' <- atP mp "type"           asStr
               sqc' <- atP mp "sequence"       asStr
               pure $ Entity (l2v cil') dsc' tpe' sqc'

-- Helper functions

-- | Converts list of chars to list of one-sized
-- (or zero-sized in case of zero) strings
c2s :: [Char] -> [Text]
c2s [] = []
c2s (x:xs) | ord x == 0 = "":c2s xs
           | otherwise  = (pack [x]):c2s xs

-- | Converst list to an array
--
l2v :: [a] -> Vector a
l2v = fromList

-- | List to list of pairs
--
l2pl :: [a] -> [(a, a)]
l2pl []       = []
l2pl (x:y:xs) = (x,y) : l2pl xs
l2pl _        = error "Cannot convert a list of odd length to a list of pairs"
