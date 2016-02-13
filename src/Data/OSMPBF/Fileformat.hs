-- | based on osmosis/osmosis-osm-binary/src/main/protobuf/fileformat.proto
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}

module Data.OSMPBF.Fileformat  where

import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Int
import GHC.Generics (Generic)
import qualified Data.ByteString as BS

data Blob = Blob {
  rawData :: Optional 1 (Value BS.ByteString)
  , rawSize :: Optional 2 (Value Int32)
  , zlibData :: Optional 3 (Value BS.ByteString)
  , lzmaData :: Optional 4 (Value BS.ByteString)
  } deriving (Generic, Show)

instance Decode Blob

data BlobHeader = BlobHeader {
  typename :: Required 1 (Value Text)
  , indexdata :: Optional 2 (Value BS.ByteString)
  , datasize :: Required 3 (Value Int32)
  } deriving (Generic)

instance Decode BlobHeader
