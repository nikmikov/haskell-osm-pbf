-- | based on osmosis/osmosis-osm-binary/src/main/protobuf/osmformat.proto
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}

module Data.OSMPBF.Osmformat where

import Data.ProtocolBuffers
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Word
import Data.Int
import Data.Typeable
import qualified Data.ByteString as B

data HeaderBlock = HeaderBlock {
      bbox :: Optional 1 (Message HeaderBBox)
    , requiredFeatures :: Repeated 4 (Value Text)
    , optionalFeatures :: Repeated 5 (Value Text)
    , writingprogram :: Optional 16 (Value Text)
    , source :: Optional 17 (Value Text)
  -- replication specific fields are skipped
  } deriving (Generic, Show)

instance Decode HeaderBlock

data HeaderBBox = HeaderBBox {
   left :: Required 1 (Value (Signed Int64))     -- ^ in nanodegrees
   , right :: Required 2 (Value (Signed Int64))
   , top :: Required 3 (Value (Signed Int64))
   , bottom :: Required 4 (Value (Signed Int64))
   } deriving (Generic, Show)

instance Decode HeaderBBox

data PrimitiveBlock = PrimitiveBlock {
  stringtable :: Required 1 (Message StringTable)
  , primitiveGroup :: Repeated 2 (Message PrimitiveGroup)
  -- Granularity, units of nanodegrees, used to store coordinates in this block
  , granularity :: Optional 17 (Value Int32) -- [default=100] defaults are not supported by protobuf
  -- Offset value between the output coordinates coordinates and the granularity grid in unites of nanodegrees.
  , latOffset :: Optional 19 (Value Int64) -- [default=0]
  , lonOffset :: Optional 20 (Value Int64) -- [default=0]
  -- Granularity of dates, normally represented in units of milliseconds since the 1970 epoch.
  , dateGranularity :: Optional 18 (Value Int32) -- [default=1000]
  -- Proposed extension:
  -- optional BBox bbox = XX;
  } deriving (Generic, Show)

instance Decode PrimitiveBlock

-- | Group of OSMPrimitives. All primitives in a group must be the same type.
data PrimitiveGroup = PrimitiveGroup {
   nodes :: Repeated 1 (Message Node)
   , dense :: Optional 2 (Message DenseNodes)
   , ways :: Repeated 3 (Message Way)
  , relations :: Repeated 4 (Message Relation)
  , changesets :: Repeated 5 (Message ChangeSet)
  } deriving (Generic, Show)

instance Decode PrimitiveGroup

-- | String table, contains the common strings in each block.
--
--   Note that we reserve index '0' as a delimiter, so the entry at that
--   index in the table is ALWAYS blank and unused.
data StringTable = StringTable {
   stringData :: Repeated 1 (Value B.ByteString)
   } deriving (Generic, Show)

instance Decode StringTable

-- | Optional metadata that may be included into each primitive.
data Info = Info {
   version :: Optional 1 (Value Int32) -- [default = -1];
   , timestamp :: Optional 2 (Value Int64)
   , changeset :: Optional 3 (Value Int64)
   , uid :: Optional 4 (Value Int32)
   , userSid :: Optional 5 (Value Word32)  -- String IDs
   -- The visible flag is used to store history information. It indicates that
   -- the current object version has been created by a delete operation on the
   -- OSM API.
   -- When a writer sets this flag, it MUST add a required_features tag with
   -- value "HistoricalInformation" to the HeaderBlock.
   -- If this flag is not available for some object it MUST be assumed to be
   -- true if the file has the required_features tag "HistoricalInformation"
   -- set.
   , visible :: Optional 6 (Value Bool)
   } deriving (Generic, Show)

instance Decode Info

-- | Optional metadata that may be included into each primitive. Special dense format used in DenseNodes.
data DenseInfo = DenseInfo {
   diVersion :: Packed 1 (Value Int32) -- [default = -1];
   , diTimestamp :: Packed 2 (Value (Signed Int64)) --  DELTA coded
   , diChangeset :: Packed 3 (Value (Signed Int64)) -- DELTA coded
   , diUid :: Packed 4 (Value (Signed Int32)) -- DELTA coded
   , diUserSid :: Packed 5 (Value (Signed Int32)) -- String IDs for usernames. DELTA coded
   -- The visible flag is used to store history information. It indicates that
   -- the current object version has been created by a delete operation on the
   -- OSM API.
   -- When a writer sets this flag, it MUST add a required_features tag with
   -- value "HistoricalInformation" to the HeaderBlock.
   -- If this flag is not available for some object it MUST be assumed to be
   -- true if the file has the required_features tag "HistoricalInformation"
   -- set.
   , diVisible :: Packed 6 (Value Bool)
   } deriving (Generic, Show)

instance Decode DenseInfo

-- THIS IS STUB DESIGN FOR CHANGESETS. NOT USED RIGHT NOW.
data ChangeSet = ChangeSet {
   changesetId :: Required 1 (Value Int64)
   } deriving (Generic, Show)

instance Decode ChangeSet

data Node = Node {
   nodeId :: Required 1 (Value (Signed Int64))
   -- Parallel arrays.
   , nodeKeys :: Packed 2 (Value Word32) -- String IDs.
   , nodeVals :: Packed 3 (Value Word32) -- String IDs.

   , nodeInfo :: Optional 4 (Message Info) -- May be omitted in omitmeta

   , nodeLat :: Required 8 (Value (Signed Int64))
   , nodeLon :: Required 9 (Value (Signed Int64))
   } deriving (Generic, Show)

instance Decode Node

-- | Used to densly represent a sequence of nodes that do not have any tags.
--
-- We represent these nodes columnwise as five columns: ID's, lats, and
-- lons, all delta coded. When metadata is not omitted,
--
-- We encode keys & vals for all nodes as a single array of integers
-- containing key-stringid and val-stringid, using a stringid of 0 as a
-- delimiter between nodes.
--
--    ( (<keyid> <valid>)* '0' )*
data DenseNodes = DenseNodes {
   diNodeId :: Packed 1 (Value (Signed Int64)) --  DELTA coded
   , diInfo :: Optional 5 (Message DenseInfo)

   , diLat :: Packed 8 (Value (Signed Int64)) -- DELTA coded
   , diLon :: Packed 9 (Value (Signed Int64)) -- DELTA coded
   -- Special packing of keys and vals into one array. May be empty if all nodes in this block are tagless.
   , diKeysVals :: Packed 10 (Value Word32)
   } deriving (Generic, Show)

instance Decode DenseNodes

data Way = Way {
   wayId :: Required 1 (Value Int64)
   -- Parallel arrays.
   , wayKeys :: Packed 2 (Value Word32)
   , wayVals :: Packed 3 (Value Word32)
   , wayInfo :: Optional 4 (Message Info)
   , wayRefs :: Packed 8 (Value (Signed Int64)) -- DELTA coded
   } deriving (Generic, Show)

instance Decode Way

data RelationMemberType
  = ZERO
  | NODE -- ^ 1
  | WAY  -- ^ 2
  | RELATION -- ^ 3
  deriving (Bounded, Enum, Eq, Typeable, Show)



data Relation = Relation {
   relId :: Required 1 (Value Int64)

   -- Parallel arrays.
   , relKeys :: Packed 2 (Value Word32)
   , relVals :: Packed 3 (Value Word32)
   , relInfo :: Optional 4 (Message Info)

   -- Parallel arrays
   , relRolesSid :: Packed 8 (Value Int32)
   , relMemIds :: Packed 9 (Value (Signed Int64)) -- DELTA encoded
   , relTypes :: Packed 10 (Enumeration RelationMemberType)
   } deriving (Generic, Show)

instance Decode Relation
