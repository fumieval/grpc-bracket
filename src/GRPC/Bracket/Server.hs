{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module GRPC.Bracket.Server
  ( NormalHandler
  , normalHandler
  ) where

import Control.Exception
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GRPC.Bracket.Types
import Network.GRPC.HighLevel.Generated as GRPC
import Proto3.Suite
import Prelude

-- | Smart constructor for 'NormalHandler'.
-- It catches 'GRPCStatusException' and 'SomeException', setting the response status
normalHandler :: HasDefault resp => (req -> IO resp) -> NormalHandler req resp
normalHandler func (ServerNormalRequest _ req) =
  do
    resp <- func req
    pure $ ServerNormalResponse resp mempty StatusOk mempty
    `catch` handleGRPC
    `catch` handleAny
  where
    handleGRPC (GRPCStatusException status details) =
      pure
        $ ServerNormalResponse def mempty status
        $ StatusDetails
        $ encodeUtf8 details
    handleAny (SomeException exception) =
      pure $ ServerNormalResponse def mempty StatusUnknown
        $ StatusDetails
        $ encodeUtf8
        $ T.pack
        $ displayException exception
