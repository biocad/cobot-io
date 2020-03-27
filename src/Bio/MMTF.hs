{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.MMTF
  ( module Bio.MMTF.Type
  , decode
  , fetch
  ) where

import           Bio.MMTF.Decode        (l2v)
import           Bio.MMTF.MessagePack   ()
import           Bio.MMTF.Type
import           Bio.Structure

import           Control.Monad.IO.Class (MonadIO)
import           Data.Bifunctor         (Bifunctor (..))
import           Data.ByteString.Lazy   (ByteString)
import           Data.Int               (Int32)
import           Data.List              (mapAccumL, zip3, zip4)
import           Data.MessagePack       (unpack)
import           Data.Monoid            ((<>))
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import           Data.Vector            (Vector, empty, toList, (!))
import           Linear.V3              (V3 (..))
import           Network.HTTP.Simple    (getResponseBody, httpLBS)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail     (MonadFail (..))
import           Prelude                hiding (fail)
#endif

-- | Decodes a 'ByteString' to 'MMTF'
--
decode :: MonadFail m => ByteString -> m MMTF
decode = unpack

-- | Fetches MMTF structure from RSCB
fetch :: (MonadFail m, MonadIO m) => String -> m MMTF
fetch pdbid = do let url = fromString $ "https://mmtf.rcsb.org/v1.0/full/" <> pdbid
                 resp <- httpLBS url
                 decode (getResponseBody resp)

instance StructureModels MMTF where
    modelsOf m = l2v (flip Model empty . l2v <$> zipWith (zipWith Chain) chainNames chainResis)
      where
        chainsCnts = fromIntegral <$> toList (chainsPerModel (model m))
        groupsCnts = fromIntegral <$> toList (groupsPerChain (chain m))
        groupsRaws = snd $ mapAccumL getGroups (0, 0) groupsCnts
        groups     = cutter chainsCnts groupsRaws
        chainNames = cutter chainsCnts (toList $ chainNameList $ chain m)
        chainResis = fmap (fmap (l2v . fmap mkResidue)) groups

        getGroups :: (Int, Int) -> Int -> ((Int, Int), [(GroupType, SecondaryStructure, [Atom])])
        getGroups (chOffset, atOffset) sz = let chEnd        = chOffset + sz
                                                gtl          = groupTypeList (group m)
                                                gl           = groupList (group m)
                                                ssl          = secStructList (group m)
                                                chr          = [chOffset .. chEnd - 1]
                                                rgt          = (gl !) . fromIntegral . (gtl !) <$> chr
                                                rss          = (ssl !) <$> chr
                                                (atEnd, ats) = mapAccumL getAtoms atOffset rgt
                                            in  ((chEnd, atEnd), zip3 rgt rss ats)

        getAtoms :: Int -> GroupType -> (Int, [Atom])
        getAtoms offset gt = let cl  = fmap fromIntegral . toList . gtFormalChargeList $ gt
                                 nl  = toList . gtAtomNameList $ gt
                                 el  = toList . gtElementList $ gt
                                 ics = [offset .. end - 1]
                                 end = offset + length cl
                             in  (end, mkAtom <$> zip4 cl nl el ics)

        mkResidue :: (GroupType, SecondaryStructure, [Atom]) -> Residue
        -- TODO: support residue number here
        mkResidue (gt, ss, atoms') = Residue (gtGroupName gt) (-1) ' ' (l2v atoms')
                                             (mkBonds (gtBondAtomList gt) (gtBondOrderList gt))
                                              ss (gtChemCompType gt)

        mkBonds :: Vector (Int32, Int32) -> Vector Int32 -> Vector (Bond LocalID)
        mkBonds bal bol = let ball = bimap (LocalID . fromIntegral) (LocalID . fromIntegral) <$> toList bal
                              boll = fromIntegral <$> toList bol
                              res  = zipWith (\(f, t) o -> Bond f t o) ball boll
                          in  l2v res

        mkAtom :: (Int, Text, Text, Int) -> Atom
        mkAtom (fc, n, e, idx) = let i = atomIdList (atom m)
                                     x = xCoordList (atom m)
                                     y = yCoordList (atom m)
                                     z = zCoordList (atom m)
                                     o = occupancyList (atom m)
                                     b = bFactorList (atom m)
                                 in  Atom (GlobalID $ fromIntegral (i ! idx))
                                           n
                                           e
                                           (V3 (x ! idx) (y ! idx) (z ! idx))
                                           fc
                                           (b ! idx)
                                           (o ! idx)

        cutter :: [Int] -> [a] -> [[a]]
        cutter []     []    = []
        cutter (x:xs) ys    = take x ys : cutter xs (drop x ys)
        cutter []     (_:_) = error "Cutter: you cannot be here"
