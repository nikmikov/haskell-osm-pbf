module Data.OSMPBF.Primitives where

import Data.Text (Text)
import Data.Int
import Data.OSMPBF.Osmformat(RelationMemberType)
import Data.Maybe(isJust)
import Data.Foldable(find)

-- latlon in nanoseconds
type LatLon = (Int64, Int64)

type Tag = (Text, Text)

data Node = Node {
  nodeId :: Int64
  , nodeCoord :: LatLon
  , nodeTags :: [Tag]
  } deriving (Show)

data Way = Way {
  wayId :: Int64
  , wayRefs :: [Int64]
  , wayTags :: [Tag]
  } deriving (Show)

data RelationMember = RelationMember {
  relMemRole :: Text
  , relMemType :: RelationMemberType
  , relMemId :: Int64
  } deriving (Show)

data Relation = Relation {
  relId :: Int64
  , relTags :: [Tag]
  , relMembers :: [RelationMember]
  } deriving (Show)

data BBox = BBox {
      bboxTopRight :: LatLon
    , bboxLeftBottom :: LatLon
      } deriving (Show)

data Header = Header {
      headerBbox :: Maybe BBox
    , headerRequiredFeatures :: [Text]
    , headerOptionalFeatures :: [Text]
    , headerWritingProgram :: Maybe Text
    , headerSource :: Maybe Text
      } deriving (Show)



data PBFPrimitive = PBFNode Node
                  | PBFWay Way
                  | PBFRelation Relation
                  | PBFHeader Header
                    deriving (Show)


-- | returns true if tag with given key exists
tagExists :: Text -> [Tag] -> Bool
tagExists k t = isJust $ findTag k t

-- | find tag by key
findTag :: Text -> [Tag] -> Maybe Tag
findTag key = find ( (==) key . fst )

-- | get list of tags from primitive
tagsFromPrimitive :: PBFPrimitive -> [Tag]
tagsFromPrimitive (PBFWay w) = wayTags w
tagsFromPrimitive (PBFNode n) = nodeTags n
tagsFromPrimitive (PBFRelation r) = relTags r
tagsFromPrimitive (PBFHeader _) = []

findTagPrim :: Text -> PBFPrimitive -> Maybe Tag
findTagPrim k = findTag k . tagsFromPrimitive


getNode :: PBFPrimitive -> Maybe Node
getNode (PBFNode n) = Just n
getNode _ = Nothing

getWay :: PBFPrimitive -> Maybe Way
getWay (PBFWay n) = Just n
getWay _ = Nothing
