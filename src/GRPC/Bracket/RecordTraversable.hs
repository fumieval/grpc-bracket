{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module GRPC.Bracket.RecordTraversable where
import Data.String
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import GHC.TypeLits

class RecordTraversable c s where
  traverseFields :: forall f. Applicative f
    => (forall a. c a
      => Text -- ^ field name
      -> a
      -> f a)
    -> s -> f s
  default traverseFields
    :: forall f. (Generic s, GRecordTraversable c (Rep s), Applicative f)
    => (forall a. c a => Text -> a -> f a) -> s -> f s
  traverseFields f = fmap to . gtraverseFields @c f . from

class GRecordTraversable c g where
  gtraverseFields :: Applicative f
    => (forall a. c a
      => Text -- ^ field name
      -> a
      -> f a)
    -> g x -> f (g x)

instance
  (KnownSymbol name, c a) =>
  GRecordTraversable c
    ( M1
        i
        ('MetaSel ('Just name) su ss ds)
        (K1 j a)
    )
  where
  gtraverseFields l (M1 (K1 a)) = M1 . K1
    <$> l (fromString $ symbolVal (Proxy :: Proxy name)) a

instance GRecordTraversable c f => GRecordTraversable c (C1 i f) where
  gtraverseFields f = fmap M1 . gtraverseFields @c f . unM1

instance GRecordTraversable c f => GRecordTraversable c (D1 i f) where
  gtraverseFields f = fmap M1 . gtraverseFields @c f . unM1

instance (GRecordTraversable c f, GRecordTraversable c g) => GRecordTraversable c (f :*: g) where
  gtraverseFields l (f :*: g) = (:*:)
    <$> gtraverseFields @c l f
    <*> gtraverseFields @c l g
