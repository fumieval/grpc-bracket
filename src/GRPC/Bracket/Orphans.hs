{-# OPTIONS -Wno-orphans #-}

module GRPC.Bracket.Orphans () where

import Data.Aeson
import GHC.Generics
import Network.GRPC.HighLevel.Generated as GRPC

instance FromJSON StatusCode
instance ToJSON StatusCode
