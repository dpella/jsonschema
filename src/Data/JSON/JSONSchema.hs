{- |
Module:      Data.JSON.JSONSchema
Copyright:   (c) DPella AB 2025
License:     LicenseRef-AllRightsReserved
Maintainer:  <matti@dpella.io>, <lobo@dpella.io>

JSON Schema generation for Haskell types.

This module provides a type class and generic implementation for
automatically deriving JSON Schema descriptions from Haskell data types.
The generated schemas follow the JSON Schema 2020-12 specification.

= Usage

Define instances using the default generic implementation:

@
data Person = Person
  { name :: Text
  , age :: Int
  } deriving (Generic)

instance ToJSONSchema Person
@

Or provide custom instances for more control:

@
instance ToJSONSchema UUID where
  toJSONSchema _ = object
    [ "type" .= ("string" :: Text)
    , "minLength" .= 36
    , "maxLength" .= 36
    ]
@
-}
module Data.JSON.JSONSchema (
    ToJSONSchema (..),
    Proxy (..),
    validateJSONSchema,
) where

import Data.JSON.ToJSONSchema
import JSONSchema.Validation
