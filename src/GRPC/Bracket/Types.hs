{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
module GRPC.Bracket.Types where

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
