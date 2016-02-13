{-# LANGUAGE OverloadedStrings #-}
module Data.OSMPBF.Decoder where

import qualified Data.OSMPBF.Fileformat as FF
import Data.OSMPBF.Primitives
import qualified Data.OSMPBF.Osmformat as OF
import Data.Binary.Get(Get, getByteString, runGetOrFail, getWord32be)
import Data.Conduit.Binary(sourceLbs)
import Data.Conduit.Serialization.Binary(conduitGet)
import Data.Conduit
import Data.List(zipWith4)
import Data.List.Split(splitWhen)
import Data.Int
import Data.Word
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Zlib as CZ
import qualified Data.ProtocolBuffers as PB
import qualified Data.ByteString as BS
import Control.Monad.Catch(MonadThrow)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Base (MonadBase)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS
import Data.Vector( (!) )
import qualified Data.Vector as V
import Control.Monad.Reader

data PrimitiveContext = PrimitiveContext {
  pbStringTable :: V.Vector T.Text
  , pbLatLonGranularity :: Int64
  , pbLatLonOffset :: LatLon
  , pbDateGranularity :: Int32
  }


primitiveContextFromBlock :: OF.PrimitiveBlock -> PrimitiveContext
primitiveContextFromBlock pb = let st = PB.getField $ OF.stringData $ PB.getField $ OF.stringtable pb
                                   (Just gran) = PB.getField (OF.granularity pb) <|> Just 100
                                   (Just latOff) = PB.getField (OF.latOffset pb) <|> Just 0
                                   (Just lonOff) = PB.getField (OF.latOffset pb) <|> Just 0
                                   (Just dateGran) = PB.getField (OF.dateGranularity pb) <|> Just 1000
                               in PrimitiveContext {
                                 pbStringTable = V.fromList $ map TE.decodeUtf8 st
                                 , pbLatLonGranularity = fromIntegral gran
                                 , pbLatLonOffset  = (latOff, lonOff)
                                 , pbDateGranularity = dateGran
                                 }

-- | pull n bytes from stream and decode it as protobuf message
--   throw an exception in case of decoding error or any leftovers left
decodeFixedLengthMessage :: (PB.Decode a) =>  Int -> Get a
decodeFixedLengthMessage len = do
  bs <- getByteString len
  case runGetOrFail PB.decodeMessage (LBS.fromStrict bs) of
   Right (bs', _, val)
      | LBS.null bs' -> return val
      | otherwise  -> fail $ "Unparsed bytes leftover in decodeFixedLengthMessage: "
                               ++ show (LBS.length bs')
   Left (_, _, err) -> fail err

-- | decode PBF blob header from binary stream
getBlobHeader :: Get FF.BlobHeader
getBlobHeader = getWord32be >>= decodeFixedLengthMessage . fromIntegral

-- | decode delta-encoded list of numbers [1,-1,1,1] -> [1, 0, 1, 2]
decodeDelta :: Num a => [a] -> [a]
decodeDelta [] = []
decodeDelta (x:xs) = x:go x xs
  where go _ [] = []
        go y (y':ys) = let val = y + y' in val:go val ys

-- | decode PBF block from binary stream
getBlob :: Get (FF.BlobHeader, FF.Blob)
getBlob = do
  hdr <- getBlobHeader
  bdy <- decodeFixedLengthMessage $ fromIntegral $ PB.getField $ FF.datasize hdr
  return (hdr, bdy)

-- | decode OSM HeaderBlock
getHeaderBlock :: Get OF.HeaderBlock
getHeaderBlock = PB.decodeMessage

-- | decode OSM PrimitiveBlock
getPrimitiveBlock :: Get OF.PrimitiveBlock
getPrimitiveBlock = PB.decodeMessage

-- | 
bboxFromHeader :: OF.HeaderBBox -> BBox
bboxFromHeader (OF.HeaderBBox lft rht top btm) =
  BBox (fromIntegral $ PB.getField top, fromIntegral $ PB.getField rht)
  (fromIntegral $ PB.getField btm, fromIntegral $ PB.getField lft)

-- |
headerBlockAsPrimitive :: OF.HeaderBlock -> PBFPrimitive
headerBlockAsPrimitive hb = PBFHeader $ Header
                            ( fmap bboxFromHeader (PB.getField $ OF.bbox hb) )
                            (PB.getField $ OF.requiredFeatures hb)
                            (PB.getField $ OF.optionalFeatures hb)
                            (PB.getField $ OF.writingprogram hb)
                            (PB.getField $ OF.source hb)

-- | stream primitives from given primitive group
sourcePg :: (MonadThrow m) => Conduit OF.PrimitiveGroup (ReaderT PrimitiveContext m) PBFPrimitive
sourcePg  = awaitForever $ \pg -> do
  CC.yieldMany (PB.getField $ OF.nodes pg) =$= CC.mapM ( fmap PBFNode . nodeToPrimitive )
  case PB.getField $ OF.dense pg of
    (Just a) -> do let pairs (f':s':xs) = (f', s'):pairs xs
                       pairs _ = []
                       tagsIx = map pairs $ splitWhen (== 0) (PB.getField $ OF.diKeysVals a)
                       lst = zipWith4 createNode
                          (decodeDelta $ PB.getField $ OF.diNodeId a)
                          (decodeDelta $ PB.getField $ OF.diLat a)
                          (decodeDelta $ PB.getField $ OF.diLon a)
                          (tagsIx  ++ repeat [])
                   lst' <- (lift . sequence) lst
                   CC.yieldMany lst' =$= CC.map PBFNode
    Nothing -> pure()
  CC.yieldMany (PB.getField $ OF.ways pg) =$= CC.mapM (fmap PBFWay . wayToPrimitive)
  CC.yieldMany (PB.getField $ OF.relations pg) =$= CC.mapM (fmap PBFRelation . relToPrimitive)

-- | unpack indexes based Tag array to strings 
unpackTags :: V.Vector T.Text -> [(Word32, Word32)] -> [Tag]
unpackTags st = map (\(k,v) -> (st ! fromIntegral k, st ! fromIntegral v) )

createNode :: (Monad m)
              => PB.Signed Int64
              -> PB.Signed Int64
              -> PB.Signed Int64
              -> [(Word32, Word32)]
              -> ReaderT PrimitiveContext m Node
createNode (PB.Signed id') (PB.Signed lat) (PB.Signed lon) tagsIx = do
  gran <- asks pbLatLonGranularity
  st <- asks pbStringTable
  (lat0, lon0) <- asks pbLatLonOffset
  return Node {
    nodeId = id'
    , nodeCoord = ( (lat0 + lat) * gran, (lon0 + lon) * gran)
    , nodeTags = unpackTags st tagsIx
    }

nodeToPrimitive :: (Monad m) => OF.Node -> ReaderT PrimitiveContext m Node
nodeToPrimitive n = createNode
                    (PB.getField $ OF.nodeId n)
                    (PB.getField $ OF.nodeLat n)
                    (PB.getField $ OF.nodeLon n)
                    ( zip (PB.getField $ OF.nodeKeys n) (PB.getField $ OF.nodeVals n) )

wayToPrimitive :: (Monad m) => OF.Way -> ReaderT PrimitiveContext m Way
wayToPrimitive w = do
  st <- asks pbStringTable
  return Way {
    wayId = PB.getField (OF.wayId w)
    , wayRefs = map fromIntegral $ decodeDelta $ PB.getField (OF.wayRefs w)
    , wayTags = unpackTags st $ zip (PB.getField $ OF.wayKeys w) (PB.getField $ OF.wayVals w)
    }

relToPrimitive :: (Monad m) => OF.Relation -> ReaderT PrimitiveContext m Relation
relToPrimitive r = do
  st <- asks pbStringTable
  return Relation {
    relId = PB.getField (OF.relId r)
    , relTags = unpackTags st $ zip (PB.getField $ OF.relKeys r) (PB.getField $ OF.relVals r)
    , relMembers = zipWith3 RelationMember
                   (map ( (!) st . fromIntegral ) $ PB.getField $ OF.relRolesSid r)
                   (PB.getField $ OF.relTypes r)
                   (map fromIntegral $ decodeDelta $ PB.getField $ OF.relMemIds r)
    }

sourcePrimitives :: MonadThrow m => Conduit OF.PrimitiveBlock m PBFPrimitive
sourcePrimitives = do
  pb' <- await
  case pb' of
    (Just a) -> go a
    Nothing -> pure()
  where go pb = CC.yieldMany (PB.getField $ OF.primitiveGroup pb)
                =$= transPipe (flip runReaderT $ primitiveContextFromBlock pb) sourcePg

decodeBlock :: (MonadThrow m) => T.Text -> Conduit BS.ByteString m PBFPrimitive
decodeBlock tp =
  case tp of
    "OSMHeader" -> conduitGet getHeaderBlock
                   =$= CC.map headerBlockAsPrimitive
    "OSMData"   -> conduitGet getPrimitiveBlock =$= sourcePrimitives
    _ -> fail $ "Unknown OSM block type:" ++ show tp

         
fromBlob :: (MonadThrow m, MonadBase base m, PrimMonad base)
            => Conduit (FF.BlobHeader, FF.Blob) m PBFPrimitive
fromBlob = awaitForever $ \(hdr, blb) -> do
           let tp = PB.getField $ FF.typename hdr
           case ( PB.getField . FF.zlibData ) blb of
             (Just a) -> sourceLbs (LBS.fromStrict a)
                         =$= CZ.decompress CZ.defaultWindowBits
                         =$= decodeBlock tp
             _ -> pure()
           case ( PB.getField . FF.rawData ) blb of
              (Just a) -> sourceLbs (LBS.fromStrict a) =$= decodeBlock tp
              _ -> pure()
           case ( PB.getField . FF.lzmaData ) blb of
              (Just _) -> fail "LZMA blob decompression is not supported yet"
              _ -> pure()


-- | Convert openstreetmap encoded in PBF format to stream of OSM primitives
conduitPbfToPrimitives :: (MonadThrow m, MonadBase base m, PrimMonad base)
                          => Conduit BS.ByteString m String
conduitPbfToPrimitives = conduitGet getBlob
                       =$= fromBlob
                       =$= CC.map show
