{-# LANGUAGE ExtendedDefaultRules #-}

module Bio.FileFormat.GB where

import           Control.Lens         (makeLenses, (^.), view)
import           Control.Monad.Except
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

import           Bio.IR.ISequence
import           Bio.IR.Meta

default (Text)

data Locus = Locus { _name       :: Text
                   , _length     :: Int
                   , _molType    :: Text
                   , _gbDivision :: Text
                   , _modDate    :: Text
                   }
  deriving (Show, Eq)

makeLenses ''Locus

data Version = Version { _idNumber :: Text
                       , _giId     :: Text
                       }
  deriving (Show, Eq)

makeLenses ''Version

data Source = Source { _sourceTitle :: Text
                     , _organism    :: Text
                     }
  deriving (Show, Eq)

makeLenses ''Source

data Reference = Reference { _reference :: Text
                           , _authors   :: Text
                           , _title     :: Text
                           , _journal   :: Text
                           , _pubmed    :: Text
                           , _dirSub    :: Text
                           }
  deriving (Show, Eq)

makeLenses ''Reference

data Feature = Feature { _fname  :: Text
                       , _frange :: Range
                       , _fdict  :: Map Text Text
                       }
  deriving (Show, Eq)

makeLenses ''Feature

data GenBank = GenBank { _locus      :: Locus
                       , _definition :: Text
                       , _accession  :: Text
                       , _version    :: Version
                       , _keywords   :: [Text]
                       , _source     :: Source
                       , _references :: [Reference]
                       , _features   :: [Feature]
                       , _origin     :: Maybe Text
                       }
  deriving (Show, Eq)

makeLenses ''GenBank

------------------------

metaGB :: GenBank -> MetaMap
metaGB gb = M.fromList $ concatMap ($ gb) [ metaNameGB, metaLocusGB, metaDefinitionGB
                                          , metaAccessionGB, metaVersionGB, metaKeywordsGB
                                          , metaSourceGB, metaReferencesGB, metaFeaturesGB
                                          ]

metaLocusGB :: GenBank -> [(MetaKey, Value)]
metaLocusGB gb = [ "molType"    =: gb ^. locus . molType
                 , "gbDivision" =: gb ^. locus . gbDivision
                 , "modDate"    =: gb ^. locus . modDate
                 ]

metaVersionGB :: GenBank -> [(MetaKey, Value)]
metaVersionGB gb = [ "idNumber" =: gb ^. version . idNumber
                   , "giId"     =: gb ^. version . giId
                   ]

metaSourceGB :: GenBank -> [(MetaKey, Value)]
metaSourceGB gb = [ "sourceTitle" =: gb ^. source . sourceTitle
                  , "organism"    =: gb ^. source . organism
                  ]

metaNameGB :: GenBank -> [(MetaKey, Value)]
metaNameGB gb = [ MetaName =: gb ^. locus . name ]

metaDefinitionGB :: GenBank -> [(MetaKey, Value)]
metaDefinitionGB gb = [ "definition" =: gb ^. definition ]

metaAccessionGB :: GenBank -> [(MetaKey, Value)]
metaAccessionGB gb = [ "accession" =: gb ^. accession ]

metaKeywordsGB :: GenBank -> [(MetaKey, Value)]
metaKeywordsGB gb = [ "keywords" =: gb ^. keywords ]

metaFeaturesGB :: GenBank -> [(MetaKey, Value)]
metaFeaturesGB gb = [ "features" =: M.fromList . metaFeature <$> gb ^. features ]
  where
    metaFeature :: Feature -> [(MetaKey, Value)]
    metaFeature feat = let dict = M.fromList [ MetaName =: feat ^. fname ] <>
                                  (T <$> M.mapKeys MetaLabel (feat ^. fdict))
                       in  [ MetaRange (feat ^. frange) =: dict ]

metaReferencesGB :: GenBank -> [(MetaKey, Value)]
metaReferencesGB gb = [ "references" =: M.fromList . metaReference <$> gb ^. references ]
  where
    metaReference :: Reference -> [(MetaKey, Value)]
    metaReference ref = [ "reference" =: ref ^. reference
                        , "authors"   =: ref ^. authors
                        , "title"     =: ref ^. title
                        , "journal"   =: ref ^. journal
                        , "pubmed"    =: ref ^. pubmed
                        , "dirSub"    =: ref ^. dirSub
                        ]

markingFeaturesGB :: GenBank -> [(Range, Label)]
markingFeaturesGB gb = markingFeature <$> gb ^. features
  where
    markingFeature :: Feature -> (Range, Label)
    markingFeature feat = (feat ^. frange, feat ^. fname)

-------------------------------------------

gbMeta :: MonadError MetaError m => Meta ISequence -> m (Text -> GenBank)
gbMeta ms = do locus'      <- gbLocusMeta ms
               definition' <- gbDefinitionMeta ms
               accession'  <- gbAccessionMeta ms
               version'    <- gbVersionMeta ms
               keywords'   <- gbKeywordsMeta ms
               source'     <- gbSourceMeta ms
               references' <- gbReferencesMeta ms
               features'   <- gbFeaturesMeta ms
               pure $ GenBank locus' definition' accession' version'
                              keywords' source' references' features' . Just

gbLocusMeta :: MonadError MetaError m => Meta ISequence -> m Locus
gbLocusMeta (Meta meta' iseq') = do locus'      <- getMetaField (MetaLabel "locus") meta'
                                    name'       <- getMetaField MetaName meta'
                                    let length' = T.length $ iseq' ^. sequ
                                    molType'    <- getMetaField (MetaLabel "molType") locus'
                                    gbDivision' <- getMetaField (MetaLabel "gbDivision") locus'
                                    modDate'    <- getMetaField (MetaLabel "modDate") locus'
                                    pure $ Locus name' length' molType' gbDivision' modDate'

gbDefinitionMeta :: MonadError MetaError m => Meta ISequence -> m Text
gbDefinitionMeta (Meta meta' _) = getMetaField (MetaLabel "definition") meta'

gbAccessionMeta :: MonadError MetaError m => Meta ISequence -> m Text
gbAccessionMeta (Meta meta' _) = getMetaField (MetaLabel "accession") meta'

gbVersionMeta :: MonadError MetaError m => Meta ISequence -> m Version
gbVersionMeta (Meta meta' _) = do version'  <- getMetaField (MetaLabel "version") meta'
                                  idNumber' <- getMetaField (MetaLabel "idNumber") version'
                                  giId'     <- getMetaField (MetaLabel "giId") version'
                                  pure $ Version idNumber' giId'

gbKeywordsMeta :: MonadError MetaError m => Meta ISequence -> m [Text]
gbKeywordsMeta (Meta meta' _) = getMetaField (MetaLabel "keywords") meta'

gbSourceMeta :: MonadError MetaError m => Meta ISequence -> m Source
gbSourceMeta (Meta meta' _) = do source'      <- getMetaField (MetaLabel "source") meta'
                                 sourceTitle' <- getMetaField (MetaLabel "sourceTitle") source'
                                 organism'    <- getMetaField (MetaLabel "organism") source'
                                 pure $ Source sourceTitle' organism'

gbReferencesMeta :: MonadError MetaError m => Meta ISequence -> m [Reference]
gbReferencesMeta (Meta meta' _) = do references' <- getMetaField (MetaLabel "references") meta'
                                     traverse gbReferenceMeta references'
  where
    gbReferenceMeta :: MonadError MetaError m => MetaMap -> m Reference
    gbReferenceMeta r = do reference' <- getMetaField (MetaLabel "reference") r
                           authors'   <- getMetaField (MetaLabel "authors") r
                           title'     <- getMetaField (MetaLabel "title") r
                           journal'   <- getMetaField (MetaLabel "journal") r
                           pubmed'    <- getMetaField (MetaLabel "pubmed") r
                           dirSub'    <- getMetaField (MetaLabel "dirSub") r
                           pure $ Reference reference' authors' title' journal' pubmed' dirSub'

gbFeaturesMeta :: forall m.MonadError MetaError m => Meta ISequence -> m [Feature]
gbFeaturesMeta (Meta meta' _) = do features' <- getMetaField (MetaLabel "features") meta'
                                   traverse gbFeatureMeta features'
  where
    gbFeatureMeta :: MonadError MetaError m => MetaMap -> m Feature
    gbFeatureMeta f = do range' <- getRange f
                         pdict' <- getMetaField (MetaRange range') f
                         name'  <- getMetaField MetaName pdict'
                         dict'  <- getDict $ M.delete MetaName pdict'
                         pure $ Feature name' range' dict'

    getDict :: MetaMap -> m (Map Text Text)
    getDict m = let m'c = forceJustMapKeys (M.mapKeys fromMetaKey m) >>=
                          forceJustMapVals . fmap fromValue
                in  case m'c of
                     Just m' -> pure m'
                     Nothing -> throwError $ WrongMeta (MetaLabel "dict")

    getRange :: MetaMap -> m Range
    getRange m = case M.keys m of
                   [MetaRange r] -> pure r
                   _             -> throwError $ WrongMeta (MetaLabel "range")

--------------

instance ISequenceLike GenBank where
    fromISequence                = const $ throwError MissingMeta
    toISequence                  = pure . mkISequence . fromMaybe "" . view origin
    toISequenceMeta gb  = pure $ Meta meta' sequ'
      where
        meta'     = metaGB gb
        markings' = markingFeaturesGB gb
        origin'   = fromMaybe "" (gb ^. origin)
        sequ'     = ISequence origin' markings' V.empty
    fromISequenceMeta ms = do mkGB <- gbMeta ms
                              pure $ mkGB (ms ^. mdata . sequ)
