{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module:      JSONSchema
-- Copyright:   (c) DPella AB 2025
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
--
-- JSON Schema generation for Haskell types.
--
-- This module provides a type class and generic implementation for
-- automatically deriving JSON Schema descriptions from Haskell data types.
-- The generated schemas follow the JSON Schema Draft 7 specification.
--
-- = Usage
--
-- Define instances using the default generic implementation:
--
-- @
-- data Person = Person
--   { name :: Text
--   , age :: Int
--   } deriving (Generic)
--
-- instance ToJSONSchema Person
-- @
--
-- Or provide custom instances for more control:
--
-- @
-- instance ToJSONSchema UUID where
--   toJSONSchema _ = object
--     [ "type" .= ("string" :: Text)
--     , "minLength" .= 36
--     , "maxLength" .= 36
--     ]
-- @
module JSONSchema.ToJSONSchema (
  ToJSONSchema (..),
  Proxy (..),
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (isJust)
import Data.Proxy
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector qualified as V
import GHC.Generics
import GHC.TypeLits

-- | Type class for converting Haskell types to JSON Schema.
--
-- The class provides a default implementation using GHC generics,
-- which works for most algebraic data types. Custom instances can
-- be defined for types requiring special schema representations.
class ToJSONSchema a where
  -- | Generate a JSON Schema for the given type.
  --
  -- The Proxy argument carries the type information without
  -- requiring an actual value of that type.
  --
  -- >>> toJSONSchema (Proxy :: Proxy Bool)
  -- {"type": "boolean"}
  toJSONSchema :: Proxy a -> Value
  default toJSONSchema
    :: ( Generic a
       , GToJSONSchema (Rep a)
       , Typeable a
       )
    => Proxy a
    -> Value
  -- We start with no root name; the D1 instance will set the root name
  -- and wrap the result with $defs and a top-level $ref. This allows
  -- recursive types to refer to themselves using $ref without infinite recursion.
  toJSONSchema _ = gToJSONSchema @(Rep a) False Nothing (Proxy :: Proxy (Rep a a))

-- | String instance with overlapping to handle String as a special case, and not as [Char]
instance {-# OVERLAPPING #-} ToJSONSchema String where
  toJSONSchema _ = object ["type" .= ("string" :: Text)]

-- | Text instance.
instance ToJSONSchema Text where
  toJSONSchema _ = object ["type" .= ("string" :: Text)]

-- | Boolean schema instance.
instance ToJSONSchema Bool where
  toJSONSchema _ = object ["type" .= ("boolean" :: Text)]

-- | Machine integer schema instance.
instance ToJSONSchema Int where
  toJSONSchema _ = object ["type" .= ("integer" :: Text)]

-- | Arbitrary precision integer schema instance.
instance ToJSONSchema Integer where
  toJSONSchema _ = object ["type" .= ("integer" :: Text)]

-- | Single precision floating point schema instance.
instance ToJSONSchema Float where
  toJSONSchema _ = object ["type" .= ("number" :: Text)]

-- | Double precision floating point schema instance.
instance ToJSONSchema Double where
  toJSONSchema _ = object ["type" .= ("number" :: Text)]

-- | List schema instance for homogeneous arrays.
instance (ToJSONSchema a) => ToJSONSchema [a] where
  toJSONSchema _ =
    object
      [ "type" .= ("array" :: Text)
      , "items" .= toJSONSchema (Proxy :: Proxy a)
      ]

-- | Either schema instance for tagged unions.
--
-- Encodes as Aeson's default representation with Left/Right tags:
-- @
-- Left x  -> {\"Left\": x}
-- Right y -> {\"Right\": y}
-- @
instance (ToJSONSchema a, ToJSONSchema b) => ToJSONSchema (Either a b) where
  toJSONSchema _ =
    object
      [ "anyOf"
          .= [ object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "Left" .= toJSONSchema (Proxy :: Proxy a)
                      ]
                ]
             , object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "Right" .= toJSONSchema (Proxy :: Proxy b)
                      ]
                ]
             ]
      ]

-- | Maybe schema instance allowing null values.
--
-- A Maybe value can be either the wrapped type or null:
-- @
-- Just x  -> x
-- Nothing -> null
-- @
instance (ToJSONSchema a) => ToJSONSchema (Maybe a) where
  toJSONSchema _ =
    object
      [ "anyOf"
          .= [ toJSONSchema (Proxy :: Proxy a)
             , object ["type" .= ("null" :: Text)]
             ]
      ]

-- | Generic type class for deriving JSON schemas.
--
-- This class handles the generic representation of data types
-- and converts them to appropriate JSON Schema structures.
--
-- The Bool parameter indicates whether we're inside a sum type
-- that needs tagging for proper deserialization.
class GToJSONSchema f where
  -- | Generate schema from generic representation.
  --
  -- The Bool parameter controls tagged union representation:
  -- * True: Add "tag" field for sum type constructors
  -- * False: No tagging needed
  -- The Maybe Text carries the root datatype name, if any. When present,
  -- occurrences of the same datatype in fields will be rendered as
  -- {"$ref": "#/$defs/<root>"} to avoid infinite recursion.
  gToJSONSchema :: (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (f a) -> Value

-- | Instance for empty types (no constructors).
--
-- Empty types are represented as JSON null since they
-- can never have a value.
instance GToJSONSchema V1 where
  gToJSONSchema _ _ _ = Null

-- | Instance for unit types (constructors with no fields).
--
-- Unit constructors are represented as null when untagged,
-- or as objects with just a tag field when tagged.
instance GToJSONSchema U1 where
  gToJSONSchema :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (U1 a) -> Value
  gToJSONSchema _ _ _ = Null

-- | Instance for sum types (multiple constructors).
--
-- Sum types are encoded using JSON Schema's "anyOf" keyword,
-- allowing the value to match any of the constructor schemas.
--
-- Example:
-- @
-- data Color = Red | Green | Blue
-- -- Generates: {"anyOf": [{...Red schema}, {...Green schema}, {...Blue schema}]}
-- @
instance (GToJSONSchema f1, GToJSONSchema f2) => GToJSONSchema (f1 :+: f2) where
  gToJSONSchema :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy ((:+:) f1 f2 a) -> Value
  gToJSONSchema _ root_name _ =
    let v1 = flattenKeys "anyOf" $ gToJSONSchema True root_name (Proxy :: Proxy (f1 a))
        v2 = flattenKeys "anyOf" $ gToJSONSchema True root_name (Proxy :: Proxy (f2 a))
    in  case (v1, v2) of
          (Object km1, Object km2)
            | Just (Array vec1) <- km1 KM.!? "anyOf"
            , Just (Array vec2) <- km2 KM.!? "anyOf" ->
                object ["anyOf" .= Array (vec1 <> vec2)]
          (Object km1, Object km2)
            | Just (Array vec) <- km1 KM.!? "anyOf"
            , Nothing <- km2 KM.!? "anyOf" ->
                object ["anyOf" .= Array (vec `V.snoc` v2)]
          (Object _, Object km2)
            | Just (Array vec) <- km2 KM.!? "anyOf" ->
                object ["anyOf" .= Array (v1 `V.cons` vec)]
          (_, _) -> object ["anyOf" .= [v1, v2]]

-- | Instance for product types (multiple fields in a constructor).
--
-- Non-record products are encoded as fixed-length arrays where
-- each position has a specific type. The "items": false ensures
-- no additional array elements are allowed.
--
-- Example:
-- @
-- data Point = Point Double Double
-- -- Generates: {"type": "array", "prefixItems": [{"type": "number"}, {"type": "number"}], "items": false}
-- @
instance (GToJSONSchema f1, GToJSONSchema f2) => GToJSONSchema (f1 :*: f2) where
  gToJSONSchema :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy ((:*:) f1 f2 a) -> Value
  gToJSONSchema _ root_name _ =
    object
      [ "type" .= ("array" :: Text)
      , "prefixItems"
          .= [ gToJSONSchema False root_name (Proxy :: Proxy (f1 a))
             , gToJSONSchema False root_name (Proxy :: Proxy (f2 a))
             ]
      , "items" .= False
      ]

-- | Helper to flatten nested array structures in schemas.
--
-- When building schemas for nested product types, we may get
-- structures like prefixItems: [a, {prefixItems: [b, c]}].
-- This function flattens them to prefixItems: [a, b, c] for
-- consistency with how Aeson represents such types.
flattenKeys :: Key -> Value -> Value
flattenKeys key (Object km)
  | Just (Array vec) <- km KM.!? key
  , length vec == 2
  , vf <- V.head vec
  , Object vlkm <- flattenKeys key (V.last vec)
  , Just (Array vec') <- vlkm KM.!? key =
      Object
        ( KM.singleton
            key
            (Array (V.cons vf vec'))
            `KM.union` km
        )
flattenKeys _ o = o

-- | Instance for datatype metadata.
-- If this is the root, we output a $defs section with the type name and schema,
-- and refer to that in $ref.
instance (KnownSymbol dtn, GToJSONSchema f) => GToJSONSchema (D1 (MetaData dtn m p nt) f) where
  gToJSONSchema
    :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (D1 (MetaData dtn m p nt) f a) -> Value
  gToJSONSchema _ root_name _ =
    let dt_name = symbolVal (Proxy :: Proxy dtn)
        this_name = pack dt_name
        body = gToJSONSchema False (Just this_name) (Proxy :: Proxy (f a))
    in  case root_name of
          -- Top-level: wrap with $defs and $ref to support recursion
          Nothing ->
            object
              [ "$defs" .= object [fromString dt_name .= body]
              , "$ref" .= ("#/$defs/" <> this_name)
              ]
          -- Nested: just return the documented body
          Just _ -> body

-- | Instance for type constants (actual field types).
--
-- This delegates to the ToJSONSchema instance of the field type,
-- allowing custom schemas for specific types.
instance (ToJSONSchema c, Typeable c) => GToJSONSchema (K1 i c) where
  gToJSONSchema :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (K1 i c a) -> Value
  gToJSONSchema _ root_name _ =
    case root_name of
      -- If we know the root type name and the field type equals the root
      -- type, emit a $ref to the root definition to avoid infinite recursion.
      Just nm ->
        if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy c)
          then object ["$ref" .= ("#/$defs/" <> nm)]
          else toJSONSchema (Proxy :: Proxy c)
      Nothing -> toJSONSchema (Proxy :: Proxy c)

-- | Instance for record constructors.
--
-- Record types are encoded as objects with named properties.
-- When in a tagged sum type, adds a "tag" field with the
-- constructor name for discrimination.
--
-- Example:
-- @
-- data Person = Person {name :: Text, age :: Int}
-- -- Generates: {
-- --   "type": "object",
-- --   "properties": {
-- --     "name": {"type": "string"},
-- --     "age": {"type": "integer"}
-- --   },
-- --   "additionalProperties": false
-- -- }
-- @
instance (KnownSymbol name, GToJSONSchema f) => GToJSONSchema (C1 (MetaCons name fixity True) f) where
  gToJSONSchema
    :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (C1 (MetaCons name fixity True) f a) -> Value
  gToJSONSchema tagged root_name _ =
    let props_val = extractProperties $ gToJSONSchema tagged root_name (Proxy :: Proxy (f a))
    in  object
          [ "type" .= ("object" :: Text)
          , "properties" .= if tagged then addTag props_val else props_val
          , "additionalProperties" .= False
          ]
    where
      tag = object ["const" .= cn]
      addTag (Object km) = Object $ KM.singleton (fromString "tag") tag `KM.union` km
      addTag o = o
      xP (Object p) | Just (Object r) <- p KM.!? "properties" = Just r
      xP _ = Nothing
      cn = symbolVal (Proxy :: Proxy name)
      extractProperties :: Value -> Value
      extractProperties o@(Object _)
        | Object km <- flattenKeys "prefixItems" o
        , Just (Array vec) <- km KM.!? "prefixItems"
        , V.all (isJust . xP) vec =
            Object $ V.foldl KM.union KM.empty $ V.mapMaybe xP vec
      extractProperties (Object km) | Just p <- km KM.!? "properties" = p
      extractProperties o = o

-- | Instance for non-record constructors.
--
-- Non-record constructors with multiple fields are encoded as arrays.
-- When in a tagged sum type, wraps in an object with "tag" and "contents".
--
-- Examples:
-- @
-- data Point = Point Double Double
-- -- Untagged: {"type": "array", "prefixItems": [...], "items": false}
--
-- data Shape = Circle Double | Rectangle Double Double
-- -- Tagged: {
-- --   "type": "object",
-- --   "properties": {
-- --     "tag": {"const": "Circle"},
-- --     "contents": {"type": "number"}
-- --   }
-- -- }
-- @
instance (KnownSymbol name, GToJSONSchema f) => GToJSONSchema (C1 (MetaCons name fixity False) f) where
  gToJSONSchema
    :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (C1 (MetaCons name fixity False) f a) -> Value
  gToJSONSchema tagged root_name _ =
    let c_val = flattenKeys "prefixItems" $ gToJSONSchema False root_name (Proxy :: Proxy (f a))
        c_name = symbolVal (Proxy :: Proxy name)
        tag = object ["const" .= c_name]
    in  case c_val of
          o@(Object km) ->
            let obj = case km KM.!? "prefixItems" of
                  Just pfi@(Array _) -> object ["type" .= ("array" :: Text), "prefixItems" .= pfi, "items" .= False]
                  _ -> o
            in  if tagged
                  then
                    object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object ["tag" .= tag, "contents" .= obj]
                      , "additionalProperties" .= False
                      ]
                  else obj
          Null ->
            object
              [ "type" .= ("object" :: Text)
              , "properties" .= object ["tag" .= tag]
              , "additionalProperties" .= False
              ]
          x -> x

-- | Instance for unnamed fields (positional constructor arguments).
--
-- Simply delegates to the field type's schema without wrapping.
instance (GToJSONSchema f) => GToJSONSchema (S1 (MetaSel Nothing su ss ds) f) where
  gToJSONSchema
    :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (S1 (MetaSel Nothing su ss ds) f a) -> Value
  gToJSONSchema _ root_name _ = gToJSONSchema False root_name (Proxy :: Proxy (f a))

-- | Instance for named record fields.
--
-- Creates an object schema with a single property named after
-- the field. These are combined by the record constructor instance.
--
-- Example:
-- @
-- data Person = Person { name :: Text }
-- -- For the 'name' field generates:
-- -- {
-- --   "type": "object",
-- --   "properties": {
-- --     "name": {"type": "string"}
-- --   }
-- -- }
-- @
instance (KnownSymbol name, GToJSONSchema f) => GToJSONSchema (S1 (MetaSel (Just name) su ss ds) f) where
  gToJSONSchema
    :: forall a. (ToJSONSchema a, Typeable a) => Bool -> Maybe Text -> Proxy (S1 (MetaSel (Just name) su ss ds) f a) -> Value
  gToJSONSchema _ root_name _ =
    object
      [ "type" .= ("object" :: Text)
      , "properties"
          .= object
            [ fromString (symbolVal (Proxy :: Proxy name)) .= gToJSONSchema False root_name (Proxy :: Proxy (f a))
            ]
      ]
