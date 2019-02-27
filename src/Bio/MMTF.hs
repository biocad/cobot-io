{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.MMTF
  ( module Bio.MMTF.Type
  , decode
  , fetch
  ) where

import           Bio.MMTF.MessagePack   ()
import           Bio.MMTF.Type   hiding ( IArray )
import           Bio.MMTF.Decode        ( l2a )
import           Bio.Structure

import           Data.Array             ( Array, (!), elems )
import           Data.Int               ( Int32 )
import           Data.Bifunctor         ( Bifunctor (..) )
import           Data.List              ( mapAccumL, zip3, zip4 )
import           Data.Text              ( Text )
import           Data.ByteString.Lazy   ( ByteString )
import           Data.MessagePack       ( unpack )
import           Data.Monoid            ( (<>) )
import           Data.String            ( IsString (..) )
import           Control.Monad.IO.Class ( MonadIO )
import           Network.HTTP.Simple    ( httpLBS, getResponseBody )
import           Linear.V3              ( V3 (..) )

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
    modelsOf m = l2a (Model . l2a <$> zipWith (zipWith Chain) chainNames chainResis)
      where
        chainsCnts = fromIntegral <$> elems (chainsPerModel (model m))
        groupsCnts = fromIntegral <$> elems (groupsPerChain (chain m))
        groupsRaws = snd $ mapAccumL getGroups (0, 0) groupsCnts
        groups     = cutter chainsCnts groupsRaws
        chainNames = cutter chainsCnts (elems $ chainNameList $ chain m)
        chainResis = fmap (fmap (l2a . fmap mkResidue)) groups

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
        getAtoms offset gt = let cl  = fmap fromIntegral . elems . gtFormalChargeList $ gt
                                 nl  = elems . gtAtomNameList $ gt
                                 el  = elems . gtElementList $ gt
                                 ics = [offset .. end - 1]
                                 end = offset + length cl
                             in  (end, mkAtom <$> zip4 cl nl el ics)

        mkResidue :: (GroupType, SecondaryStructure, [Atom]) -> Residue
        mkResidue (gt, ss, atoms) = Residue (gtGroupName gt) (l2a atoms)
                                            (mkBonds (gtBondAtomList gt) (gtBondOrderList gt))
                                             ss (gtChemCompType gt)

        mkBonds :: Array Int (Int32, Int32) -> Array Int Int32 -> Array Int Bond
        mkBonds bal bol = let ball = bimap fromIntegral fromIntegral <$> elems bal
                              boll = fromIntegral <$> elems bol
                              res  = zipWith (\(f, t) o -> Bond f t o) ball boll
                          in  l2a res

        mkAtom :: (Int, Text, Text, Int) -> Atom
        mkAtom (fc, n, e, idx) = let x = xCoordList (atom m)
                                     y = yCoordList (atom m)
                                     z = zCoordList (atom m)
                                     o = occupancyList (atom m)
                                     b = bFactorList (atom m)
                                 in  Atom n e (V3 (x ! idx) (y ! idx) (z ! idx)) fc (b ! idx) (o ! idx)

        cutter :: [Int] -> [a] -> [[a]]
        cutter []     []    = []
        cutter (x:xs) ys    = take x ys : cutter xs (drop x ys)
        cutter []     (_:_) = error "Cutter: you cannot be here"
