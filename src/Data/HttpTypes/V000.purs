module Data.HttpTypes.V000 (
  Request(..),
  Response(..),
  Exchange(..),
  HttpTypesMap,
  Method(..),
  Protocol(..),
  Query,
  Header,
  JSON(..)
) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Foreign (ForeignError(..), fail, isNull, readArray, readBoolean, readNumber, readString)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)

newtype HttpTypesMap b = HttpTypesMap (Map.Map String b)
derive newtype instance eqHttpTypesMap :: (Eq a) => Eq (HttpTypesMap a)

data Method = GET | POST | PUT | DELETE | PATCH | HEAD | TRACE | OPTIONS | CONNECT
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

data Protocol = HTTP | HTTPS
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

data Query = StringQuery String | ArrayQuery (Array String)
instance readForeignQuery :: ReadForeign Query where
  readImpl a = (readString a >>= pure <<< StringQuery) <|> (readArray a >>= sequence <<< map readImpl >>= pure <<< ArrayQuery)
instance writeForeignQuery :: WriteForeign Query where
  writeImpl (StringQuery s) = writeImpl s
  writeImpl (ArrayQuery a) = writeImpl a

data Header = StringHeader String | ArrayHeader (Array String)
instance readForeignHeader :: ReadForeign Header where
  readImpl a = (readString a >>= pure <<< StringHeader) <|> (readArray a >>= sequence <<< map readImpl >>= pure <<< ArrayHeader)
instance writeForeignHeader :: WriteForeign Header where
  writeImpl (StringHeader s) = writeImpl s
  writeImpl (ArrayHeader a) = writeImpl a


data JSON = JObject (HttpTypesMap JSON) | JArray (Array JSON) | JString String | JBoolean Boolean | JNumber Number | JNull

newtype Request = Request {
  method :: Method,
  protocol :: Maybe Protocol,
  host :: Maybe String,
  url :: Maybe String,
  path :: Maybe String,
  pathname :: Maybe String,
  query :: Maybe (HttpTypesMap Query),
  headers :: Maybe (HttpTypesMap Header),
  body :: Maybe String,
  timestamp :: Maybe String
}
derive newtype instance readForeignRequest :: ReadForeign Request

newtype Response = Response {
  statusCode :: Int,
  body :: Maybe String,
  headers :: Maybe (HttpTypesMap Header),
  timestamp :: Maybe String
}
derive newtype instance readForeignResponse :: ReadForeign Response

newtype Exchange = Exchange {
  meta :: Maybe JSON,
  request :: Request,
  response :: Response
}
derive newtype instance readForeignExchange :: ReadForeign Exchange


instance readForeignHttpTypesMap :: (ReadForeign a) => ReadForeign (HttpTypesMap a) where
  readImpl f = do
    v <- (readImpl f)
    pure (HttpTypesMap $ Map.fromFoldable ((FO.toUnfoldable $ v) :: (Array (Tuple String a))))


httpTypesMapToObject :: forall a. (WriteForeign a) => HttpTypesMap a -> FO.Object a
httpTypesMapToObject (HttpTypesMap f) = FO.fromFoldable ((Map.toUnfoldable f) :: (Array (Tuple String a)))

instance writeForeignHttpTypesMap :: (WriteForeign a) => WriteForeign (HttpTypesMap a) where
  writeImpl f = writeImpl (httpTypesMapToObject f)

instance readForeignJSON :: ReadForeign JSON where
  readImpl f = if (isNull f) then pure JNull else (readNumber f >>= pure <<< JNumber) <|> (readBoolean f >>= pure <<< JBoolean) <|> (readString f >>= pure <<< JString) <|> (readArray f >>= sequence <<< map readImpl >>= pure <<< JArray) <|> (readImpl f >>= pure <<< JObject)

instance writeForeignJSON :: WriteForeign JSON where
  writeImpl (JObject j) = writeImpl $ httpTypesMapToObject j
  writeImpl (JArray j) = writeImpl j
  writeImpl (JBoolean j) = writeImpl j
  writeImpl (JNumber j) = writeImpl j
  writeImpl (JString j) = writeImpl j
  writeImpl JNull = writeImpl (null :: Nullable Int) -- chose Int at random