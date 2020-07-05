module Data.HttpTypes.V000
  ( Request(..)
  , Response(..)
  , Exchange(..)
  , HttpTypesMap(..)
  , Method(..)
  , Protocol(..)
  , Query(..)
  , Header(..)
  , JSON(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Foreign (ForeignError(..), fail, isNull, readArray, readBoolean, readNumber, readString)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)

newtype HttpTypesMap b
  = HttpTypesMap (Map.Map String b)

derive instance newtypeHttpTypesMap :: Newtype (HttpTypesMap a) _

derive instance genericHttpTypesMap :: Generic (HttpTypesMap a) _

derive newtype instance eqHttpTypesMap :: (Eq a) => Eq (HttpTypesMap a)

derive newtype instance semigroupHttpTypesMap :: Semigroup (HttpTypesMap a)

derive newtype instance monoidHttpTypesMap :: Monoid (HttpTypesMap a)

instance httpTypesMapShow :: (Show a) => Show (HttpTypesMap a) where
  show = genericShow

data Method
  = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | HEAD
  | TRACE
  | OPTIONS
  | CONNECT

derive instance eqMethod :: Eq Method

derive instance genericMethod :: Generic Method _

instance showMethod :: Show Method where
  show = genericShow

instance readForeignMethod :: ReadForeign Method where
  readImpl a = do
    m <- readString a
    case m of
      "get" -> pure GET
      "post" -> pure POST
      "put" -> pure PUT
      "delete" -> pure DELETE
      "patch" -> pure PATCH
      "head" -> pure HEAD
      "trace" -> pure TRACE
      "options" -> pure OPTIONS
      "connect" -> pure CONNECT
      _ -> fail (ForeignError "Illegal protocol")

instance writeForeignMethod :: WriteForeign Method where
  writeImpl GET = writeImpl "get"
  writeImpl POST = writeImpl "post"
  writeImpl PUT = writeImpl "put"
  writeImpl DELETE = writeImpl "delete"
  writeImpl PATCH = writeImpl "patch"
  writeImpl HEAD = writeImpl "head"
  writeImpl TRACE = writeImpl "trace"
  writeImpl OPTIONS = writeImpl "options"
  writeImpl CONNECT = writeImpl "connect"

data Protocol
  = HTTP
  | HTTPS

derive instance genericProtocol :: Generic Protocol _

instance showProtocol :: Show Protocol where
  show = genericShow

derive instance eqProtocol :: Eq Protocol

instance readForeignProtocol :: ReadForeign Protocol where
  readImpl a = do
    m <- readString a
    case m of
      "http" -> pure HTTP
      "https" -> pure HTTPS
      _ -> fail (ForeignError "Illegal protocol")

instance writeForeignPrtocol :: WriteForeign Protocol where
  writeImpl HTTP = writeImpl "http"
  writeImpl HTTPS = writeImpl "https"

data Query
  = StringQuery String
  | ArrayQuery (Array String)

derive instance genericQuery :: Generic Query _

derive instance eqQuery :: Eq Query

instance showQuery :: Show Query where
  show = genericShow

instance readForeignQuery :: ReadForeign Query where
  readImpl a = (StringQuery <$> readString a) <|> (ArrayQuery <$> (readArray a >>= sequence <<< map readImpl))

instance writeForeignQuery :: WriteForeign Query where
  writeImpl (StringQuery s) = writeImpl s
  writeImpl (ArrayQuery a) = writeImpl a

data Header
  = StringHeader String
  | ArrayHeader (Array String)

derive instance genericHeader :: Generic Header _

derive instance eqHeader :: Eq Header

instance showHeader :: Show Header where
  show = genericShow

instance readForeignHeader :: ReadForeign Header where
  readImpl a = (StringHeader <$> readString a) <|> (ArrayHeader <$> (readArray a >>= sequence <<< map readImpl))

instance writeForeignHeader :: WriteForeign Header where
  writeImpl (StringHeader s) = writeImpl s
  writeImpl (ArrayHeader a) = writeImpl a

newtype Request
  = Request
  { method :: Method
  , protocol :: Maybe Protocol
  , host :: Maybe String
  , url :: Maybe String
  , path :: Maybe String
  , pathname :: Maybe String
  , query :: Maybe (HttpTypesMap Query)
  , headers :: Maybe (HttpTypesMap Header)
  , body :: Maybe String
  , timestamp :: Maybe String
  }

derive instance genericRequest :: Generic Request _

derive instance newtypeRequest :: Newtype Request _

derive instance eqRequest :: Eq Request

instance showRequest :: Show Request where
  show = genericShow

derive newtype instance readForeignRequest :: ReadForeign Request

derive newtype instance writeForeignRequest :: WriteForeign Request

newtype Response
  = Response
  { statusCode :: Int
  , body :: Maybe String
  , headers :: Maybe (HttpTypesMap Header)
  , timestamp :: Maybe String
  }

derive newtype instance readForeignResponse :: ReadForeign Response

derive newtype instance writeForeignResponse :: WriteForeign Response

derive instance genericResponse :: Generic Response _

derive instance newtypeResponse :: Newtype Response _

derive instance eqResponse :: Eq Response

instance showResponse :: Show Response where
  show = genericShow

newtype Exchange
  = Exchange
  { meta :: Maybe JSON
  , request :: Request
  , response :: Response
  }

derive newtype instance readForeignExchange :: ReadForeign Exchange

derive newtype instance writeForeignExchange :: WriteForeign Exchange

derive instance genericExchange :: Generic Exchange _

derive instance newtypeExchange :: Newtype Exchange _

derive instance eqExchange :: Eq Exchange

instance showExchange :: Show Exchange where
  show = genericShow

instance readForeignHttpTypesMap :: (ReadForeign a) => ReadForeign (HttpTypesMap a) where
  readImpl f = do
    v <- (readImpl f)
    pure (HttpTypesMap $ Map.fromFoldable ((FO.toUnfoldable $ v) :: (Array (Tuple String a))))

httpTypesMapToObject :: forall a. (WriteForeign a) => HttpTypesMap a -> FO.Object a
httpTypesMapToObject (HttpTypesMap f) = FO.fromFoldable ((Map.toUnfoldable f) :: (Array (Tuple String a)))

instance writeForeignHttpTypesMap :: (WriteForeign a) => WriteForeign (HttpTypesMap a) where
  writeImpl f = writeImpl (httpTypesMapToObject f)

data JSON
  = JObject (HttpTypesMap JSON)
  | JArray (Array JSON)
  | JString String
  | JBoolean Boolean
  | JNumber Number
  | JNull

instance readForeignJSON :: ReadForeign JSON where
  readImpl f = if (isNull f) then pure JNull else (readNumber f >>= pure <<< JNumber) <|> (readBoolean f >>= pure <<< JBoolean) <|> (readString f >>= pure <<< JString) <|> (readArray f >>= sequence <<< map readImpl >>= pure <<< JArray) <|> (readImpl f >>= pure <<< JObject)

instance writeForeignJSON :: WriteForeign JSON where
  writeImpl (JObject j) = writeImpl $ httpTypesMapToObject j
  writeImpl (JArray j) = writeImpl j
  writeImpl (JBoolean j) = writeImpl j
  writeImpl (JNumber j) = writeImpl j
  writeImpl (JString j) = writeImpl j
  writeImpl JNull = writeImpl (null :: Nullable Int) -- chose Int at random

derive instance genericJSON :: Generic JSON _

derive instance eqJSON :: Eq JSON

instance showJSON :: Show JSON where
  show a = genericShow a
