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
import           Data.Vector            as V (Vector, toList, (!), null, splitAt, unfoldr)
import           Linear.V3              (V3 (..))
import           Network.HTTP.Simple    (getResponseBody, httpLBS)

-- | Decodes a 'ByteString' to 'MMTF'
--
decode :: Monad m => ByteString -> m MMTF
decode = unpack

-- | Fetches MMTF structure from RSCB
fetch :: MonadIO m => String -> m MMTF
fetch pdbid = do let url = fromString $ "https://mmtf.rcsb.org/v1.0/full/" <> pdbid
                 resp <- httpLBS url
                 decode (getResponseBody resp)

instance StructureModels MMTF where
    modelsOf m = l2v (Model . l2v <$> zipWith (zipWith Chain) chainNames chainResis)
      where
        chainsCnts = toList $ fromIntegral <$> (chainsPerModel (model m))
        groupsCnts = fromIntegral <$> (groupsPerChain (chain m))
        groupsRaws = snd $ mapAccumL getGroups (0, 0) groupsCnts
        groups     = splitPlaces chainsCnts groupsRaws
        chainNames = splitPlaces chainsCnts (chainNameList $ chain m)
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
        mkResidue (gt, ss, atoms) = Residue (gtGroupName gt) (l2v atoms)
                                            (mkBonds (gtBondAtomList gt) (gtBondOrderList gt))
                                             ss (gtChemCompType gt)

        mkBonds :: Vector (Int32, Int32) -> Vector Int32 -> Vector Bond
        mkBonds bal bol = let ball = bimap fromIntegral fromIntegral <$> toList bal
                              boll = fromIntegral <$> toList bol
                              res  = zipWith (\(f, t) o -> Bond f t o) ball boll
                          in  l2v res

        mkAtom :: (Int, Text, Text, Int) -> Atom
        mkAtom (fc, n, e, idx) = let x = xCoordList (atom m)
                                     y = yCoordList (atom m)
                                     z = zCoordList (atom m)
                                     o = occupancyList (atom m)
                                     b = bFactorList (atom m)
                                 in  Atom n e (V3 (x ! idx) (y ! idx) (z ! idx)) fc (b ! idx) (o ! idx)

        -- this code copied from [here](http://hackage.haskell.org/package/vector-split-1.0.0.2/docs/src/Data-Vector-Split.html#splitPlaces)
        splitPlaces :: [Int] -> Vector a -> [Vector a]
        splitPlaces is v = toList $ V.unfoldr go (is,v)
          where go ([],_)   = Nothing
                go (x:xs,y) | V.null y = Nothing
                            | otherwise = let (l,r) = V.splitAt x y in Just (l,(xs,r))
