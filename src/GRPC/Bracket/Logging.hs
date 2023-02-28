{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module GRPC.Bracket.Logging
  ( CallLog(..)
  , serviceLogWith
  , serviceLogJSON
  , HasCallLog(..)
  ) where

import Data.Aeson qualified as J
import Data.Functor.Identity
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Proto3.Suite.JSONPB
import GRPC.Bracket.Orphans ()
import GRPC.Bracket.Types
import GRPC.Bracket.RecordTraversable
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

class (c (RequestOf method), c (ResponseOf method)) => HasCallLog c method where
  withCallLog
    :: (CallLog (RequestOf method) (ResponseOf method) -> IO ())
    -> Text
    -> method
    -> method

instance (c req, c resp) => HasCallLog c (NormalHandler req resp) where
  withCallLog
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

instance (c req, c resp) => HasCallLog c (NormalMethod req resp) where
  withCallLog
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

serviceLogWith :: forall c service.
  RecordTraversable (HasCallLog c) service
  => (forall req resp. (c req, c resp) => CallLog req resp -> IO ())
  -> service -> service
serviceLogWith logger =
  runIdentity
  . traverseFields @(HasCallLog c)
  (\name -> pure . withCallLog @c logger name)

serviceLogJSON :: forall service.
  RecordTraversable (HasCallLog ToJSONPB) service
  => (forall req resp. (ToJSONPB req, ToJSONPB resp) => CallLog req resp -> IO ())
  -> service -> service
serviceLogJSON = serviceLogWith @ToJSONPB