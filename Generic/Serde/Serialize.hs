{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Generic.Serde.Serialize where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder

serialize :: Serialize a => a -> ByteString
serialize = Builder.toLazyByteString . encoding

char :: Char -> Builder
char = Builder.charUtf8

parens :: Builder -> Builder
parens b = char '(' <> b <> char ')'

string :: String -> Builder
string = Builder.stringUtf8

class Serialize a where
  encoding :: a -> Builder
  default encoding :: (Generic a, GSerialize () (Rep a)) => a -> Builder
  encoding = gencoding @() . from

  encodingList :: [a] -> Builder
  encodingList [] = string "[]"
  encodingList (x:xs) = mconcat
    [ char '['
    , encoding x
    , foldMap (const (char ',') <> encoding) xs
    , char ']'
    ]

instance {-# overlappable #-} (Generic a, GSerialize () (Rep a)) => Serialize a

instance Serialize Char where
  encoding = string . show
  encodingList = string . show

instance Serialize a => Serialize [a] where
  encoding = encodingList

class GSerialize (h :: k) f where
  gencoding :: f x -> Builder

instance GSerialize () f => GSerialize () (M1 D m f) where
  gencoding (M1 a) = gencoding @() a

instance (GSerialize () f, GSerialize () g) => GSerialize () (f :+: g) where
  gencoding (L1 a) = gencoding @() a
  gencoding (R1 a) = gencoding @() a

instance (GSerialize False f, KnownSymbol n) => GSerialize () (M1 C (MetaCons n PrefixI False) f) where
  gencoding (M1 a) = parens $ string (symbolVal @n Proxy) <> gencoding @False a

instance (GSerialize True f, KnownSymbol n) => GSerialize () (M1 C (MetaCons n PrefixI True) f) where
  gencoding (M1 a) = mconcat
    [ string (symbolVal @n Proxy)
    , char '{'
    , gencoding @True a
    , char '}'
    ]

instance (GSerialize () f) => GSerialize False (M1 S (MetaSel Nothing su ss ds) f) where
  gencoding (M1 a) = char ' ' <> gencoding @() a

instance (GSerialize () f, KnownSymbol s) => GSerialize True (M1 S (MetaSel (Just s) su ss ds) f) where
  gencoding (M1 a) = mconcat
    [ string (symbolVal @s Proxy)
    , char '='
    , gencoding @() a
    ]

instance (GSerialize False f, GSerialize False g) => GSerialize False (f :*: g) where
  gencoding (a :*: b) = gencoding @False a <> gencoding @False b

instance (GSerialize True f, GSerialize True g) => GSerialize True (f :*: g) where
  gencoding (a :*: b) = gencoding @True a <> char ',' <> gencoding @True b

instance GSerialize h U1 where
  gencoding _ = mempty

instance GSerialize () V1 where
  gencoding v = case v of {}

instance Serialize a => GSerialize () (K1 i a) where
  gencoding (K1 a) = encoding a