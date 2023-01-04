{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
module GRPC.Bracket.Logging
  ( CallLog(..)
  , clientCallLog
  , serverCallLog
  ) where

import Data.Aeson qualified as J
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Proto3.Suite.JSONPB
import GRPC.Bracket.Orphans ()
import GRPC.Bracket.Types
import Network.GRPC.HighLevel.Generated as GRPC

data CallLog req resp = CallLog
  { method :: Text
  , requestMetadata :: Map.Map Text Text
  , request :: ViaPB req
  , responseMetadata :: Map.Map Text Text
  , response :: ViaPB resp
  , status :: StatusCode
  , details :: Text
  } deriving Generic
instance (FromJSONPB req, FromJSONPB resp) => J.FromJSON (CallLog req resp)
instance (ToJSONPB req, ToJSONPB resp) => J.ToJSON (CallLog req resp)

transformMetadataMap :: MetadataMap -> Map.Map Text Text
transformMetadataMap (MetadataMap m) = Map.fromList
  [ (decodeUtf8 k, decodeUtf8 v)
  | (k, vs) <- Map.toList m
  , v <- vs
  ]

serverCallLog
  :: (CallLog req resp -> IO ())
  -> Text
  -> NormalHandler req resp
  -> NormalHandler req resp
serverCallLog
  logger
  method
  handler
  req@(ServerNormalRequest serverCall (ViaPB -> request))
  = handler req >>= \r -> case r of
    ServerNormalResponse (ViaPB -> response) metadata status (StatusDetails (decodeUtf8 -> details))
      -> r <$ logger CallLog
        {responseMetadata = transformMetadataMap metadata, ..}
  where
    requestMetadata = transformMetadataMap $ metadata serverCall

clientCallLog
  :: (CallLog req resp -> IO ())
  -> Text
  -> NormalMethod req resp
  -> NormalMethod req resp
clientCallLog
  logger
  method
  handler
  req@(ClientNormalRequest (ViaPB -> request) _timeout metadataMap)
  = handler req >>= \r -> case r of
    ClientNormalResponse (ViaPB -> response) initMD trailMD status (StatusDetails (decodeUtf8 -> details))
      -> r <$ logger CallLog
        {responseMetadata = transformMetadataMap $ initMD <> trailMD, ..}
    ClientErrorResponse _ -> pure r
  where
    requestMetadata = transformMetadataMap metadataMap