{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
module GRPC.Bracket.Types where

import Data.Aeson qualified as J
import Proto3.Suite.JSONPB
import Control.Exception
import Data.Text (Text)
import Network.GRPC.HighLevel.Generated as GRPC

data GRPCStatusException = GRPCStatusException
  { code :: StatusCode
  , details :: Text
  } deriving (Eq, Show)

instance Exception GRPCStatusException

type NormalMethod req res
  = ClientRequest 'Normal req res
  -> IO (ClientResult 'Normal res)

type NormalHandler req resp
  = ServerRequest 'Normal req resp
  -> IO (ServerResponse 'Normal resp)

newtype ViaPB a = ViaPB a

instance FromJSONPB a => J.FromJSON (ViaPB a) where
  parseJSON val = ViaPB <$> parseJSONPB val

instance ToJSONPB a => J.ToJSON (ViaPB a) where
  toJSON (ViaPB a) = toJSONPB a defaultOptions
  toEncoding (ViaPB a) = toEncodingPB a defaultOptions
