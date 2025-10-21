{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}

-- |
-- Module:      JSONSchema.Validation
-- Copyright:   (c) DPella AB 2025
-- License:     LicenseRef-AllRightsReserved
-- Maintainer:  <matti@dpella.io>, <lobo@dpella.io>
--
-- JSON Schema validation according to JSON Schema 2020-12.
--
-- This module provides validation functions that check if a JSON value
-- conforms to a given JSON Schema. It supports the core 2020-12
-- semantics for validation and applicator keywords ("type", "properties", "patternProperties",
-- "additionalProperties", "items", "prefixItems", numeric and string
-- constraints, combinators, and conditionals). Local $ref resolution to JSON Pointers
-- within the same schema document (including paths under "$defs") is supported.
-- The "unevaluatedProperties" and "unevaluatedItems" keywords are implemented
-- with pragmatic semantics at the current instance location (covering properties/items
-- handled by properties/patternProperties/prefixItems/items/contains). The full
-- annotation-merging behavior across nested applicators is not implemented.
-- The Format-Assertion vocabulary is not implemented; the "format" and "content*"
-- keywords are treated as annotations only, per the 2020-12 specification.
--
-- = Usage
--
-- @
-- let schema = toJSONSchema (Proxy :: Proxy Person)
-- let value = object ["name" .= "Alice", "age" .= 30]
-- validateJSONSchema schema value  -- Returns True if valid
-- @
module JSONSchema.Validation (
  validateJSONSchema,
  ValidationError (..),
  validate,
  validateWithErrors,
) where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ratio (denominator)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

-- | Validation error with context about what failed
data ValidationError = ValidationError
  { error_path :: [Text]
  -- ^ Path to the failing value (e.g. ["users", "0", "name"])
  , error_message :: Text
  -- ^ Description of the validation failure
  }
  deriving (Show, Eq)

-- | Simple validation that returns True if the value matches the schema
validateJSONSchema :: Value -> Value -> Bool
validateJSONSchema schema value = null $ validateWithErrors schema value

-- | Validate with error collection
validate :: Value -> Value -> Either [ValidationError] ()
validate schema value =
  case validateWithErrors schema value of
    [] -> Right ()
    errs -> Left errs

-- | Validate and return all errors found for an instance against a schema.
-- This walks the instance and applies the validation vocabulary for 2020-12,
-- returning every violation discovered (no short-circuit).
validateWithErrors :: Value -> Value -> [ValidationError]
validateWithErrors schema value = validateValue [] schema schema value maxRefDepth

-- | Max recursion depth for $ref to avoid infinite loops
maxRefDepth :: Int
maxRefDepth = 256

-- | Internal validator with context.
--
-- Parameters:
--  - path: JSON Pointer-like path to the current instance location
--  - root: the root schema document (for resolving local $ref)
--  - schema: the current subschema to apply
--  - value: the current instance value
--  - fuel: remaining recursion depth for $ref resolution
validateValue :: [Text] -> Value -> Value -> Value -> Int -> [ValidationError]
validateValue path root schema value fuel =
  case schema of
    Bool True -> [] -- true schema always validates
    Bool False -> [ValidationError path "Schema is false (always fails)"]
    Object km ->
      -- \$ref short-circuit: if present, ignore other keywords (per spec)
      case km KM.!? "$ref" of
        Just (String ref_path) ->
          if fuel <= 0
            then [ValidationError path "Exceeded $ref resolution depth"]
            else case resolveRef root ref_path of
              Just ref_schema -> validateValue path root ref_schema value (fuel - 1)
              Nothing -> [ValidationError path ("Unresolved $ref: " <> ref_path)]
        _ -> validateObject path root km value fuel
    _ -> [ValidationError path "Invalid schema: must be a boolean or object"]

-- | Apply object schema keywords and dispatch to other validators.
--
-- Covers:
--  - type/const/enum
--  - Object keywords: properties, patternProperties, additionalProperties,
--    propertyNames, required, dependentSchemas, dependentRequired
--  - Array keywords present at this location: prefixItems, items (when instance is array)
--  - String/Number/Object count constraints
--  - Combinators: anyOf, oneOf, allOf, not
--  - Conditionals: if/then/else
--  - unevaluatedProperties / unevaluatedItems (local, pragmatic semantics)
validateObject :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateObject path root km value fuel =
  concat
    [ validateType path km value
    , validateConst path km value
    , validateEnum path km value
    , validateProperties path root km value fuel
    , validateItems path root km value fuel
    , validatePrefixItems path root km value fuel
    , validateArrayConstraints path root km value fuel
    , validateStringConstraints path km value
    , validateNumberConstraints path km value
    , validateObjectConstraints path km value
    , validateCombinators path root km value fuel
    , validateConditional path root km value fuel
    , validateUnevaluatedProperties path root km value fuel
    , validateUnevaluatedItems path root km value fuel
    ]

-- | Validate the "type" keyword. Supports a single string or an array of types.
-- Recognized types: null, boolean, string, number, integer, array, object.
validateType :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateType path km value =
  case km KM.!? "type" of
    Nothing -> []
    Just (String type_str) ->
      [ ValidationError path $ "Expected type " <> type_str <> " but got " <> describeType value
      | not (checkType type_str value)
      ]
    Just (Array types) ->
      let type_strs = [t | String t <- V.toList types]
      in  [ ValidationError path $ "Expected one of types " <> T.intercalate ", " type_strs <> " but got " <> describeType value
          | not (any (`checkType` value) type_strs)
          ]
    Just _ -> [ValidationError path "Invalid 'type' field in schema"]

-- | Check whether a JSON value is of the given JSON Schema type.
checkType :: Text -> Value -> Bool
checkType "null" Null = True
checkType "boolean" (Bool _) = True
checkType "string" (String _) = True
checkType "number" (Number _) = True
-- Per JSON Schema, "integer" means the instance is a number with an integral value,
-- not limited by machine-sized Int bounds.
checkType "integer" (Number n) = Sci.isInteger n
checkType "array" (Array _) = True
checkType "object" (Object _) = True
checkType _ _ = False

-- | Derive the schema type name corresponding to a value (e.g., "integer").
describeType :: Value -> Text
describeType Null = "null"
describeType (Bool _) = "boolean"
describeType (String _) = "string"
describeType (Number n)
  | Sci.isInteger n = "integer"
  | otherwise = "number"
describeType (Array _) = "array"
describeType (Object _) = "object"

-- | Validate the "const" keyword (instance must be exactly equal to the given value).
validateConst :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateConst path km value =
  case km KM.!? "const" of
    Nothing -> []
    Just const_val ->
      [ ValidationError path $ "Expected constant value " <> showJSON const_val <> " but got " <> showJSON value
      | value /= const_val
      ]

-- | Validate the "enum" keyword (instance must be a member of the given array).
validateEnum :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateEnum path km value =
  case km KM.!? "enum" of
    Nothing -> []
    Just (Array values) ->
      [ValidationError path $ "Value not in enum: " <> showJSON value | not (value `V.elem` values)]
    Just _ -> [ValidationError path "Invalid 'enum' field in schema"]

-- | Validate object-related keywords at this location.
--
-- Implements:
--  - properties: validate each defined property if present
--  - patternProperties: validate properties whose names match regex patterns
--  - additionalProperties: validate and/or forbid properties not matched above
--  - propertyNames: validate each property name as a string instance
--  - required: ensure listed properties exist
--  - dependentSchemas: when a property exists, apply another schema to the whole object
--  - dependentRequired: when a property exists, require additional properties
validateProperties :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateProperties path root km value fuel =
  case value of
    Object obj ->
      let props_schema = km KM.!? "properties"
          pattern_props_schema = km KM.!? "patternProperties"
          additional_props_schema = km KM.!? "additionalProperties"
          property_names_schema = km KM.!? "propertyNames"
          required_props = case km KM.!? "required" of
            Just (Array arr) -> [t | String t <- V.toList arr]
            _ -> []

          -- Validate properties defined in 'properties'
          prop_errors = case props_schema of
            Just (Object schema_obj) ->
              concat
                [ case obj KM.!? key of
                  Just prop_value ->
                    validateValue (path <> [K.toText key]) root schema prop_value fuel
                  Nothing -> []
                | (key, schema) <- KM.toList schema_obj
                ]
            _ -> []

          -- Validate properties matching patterns in 'patternProperties'
          pattern_prop_errors = case pattern_props_schema of
            Just (Object patterns) ->
              concat
                [ concat
                  [ if K.toText obj_key =~ K.toText pattern_key
                    then validateValue (path <> [K.toText obj_key]) root pattern_schema obj_value fuel
                    else []
                  | (pattern_key, pattern_schema) <- KM.toList patterns
                  ]
                | (obj_key, obj_value) <- KM.toList obj
                ]
            _ -> []

          -- Determine which properties are handled by properties or patternProperties
          handled_by_props = case props_schema of
            Just (Object schema_obj) -> KM.keys schema_obj
            _ -> []
          handled_by_patterns = case pattern_props_schema of
            Just (Object patterns) ->
              [ obj_key
              | obj_key <- KM.keys obj
              , any (\pattern_key -> K.toText obj_key =~ K.toText pattern_key) (KM.keys patterns)
              ]
            _ -> []
          unhandled_keys = filter (\k -> k `notElem` handled_by_props && k `notElem` handled_by_patterns) (KM.keys obj)

          -- Validate additional properties
          additional_errors = case additional_props_schema of
            Just (Bool False) ->
              [ ValidationError path $
                "Additional property not allowed: " <> K.toText key
              | key <- unhandled_keys
              ]
            Just schema ->
              concat
                [ case obj KM.!? key of
                  Just prop_value ->
                    validateValue (path <> [K.toText key]) root schema prop_value fuel
                  Nothing -> []
                | key <- unhandled_keys
                ]
            Nothing -> []

          -- Validate property names
          property_name_errors = case property_names_schema of
            Just schema ->
              concat
                [ let key_value = String (K.toText key)
                  in  validateValue (path <> ["propertyName:" <> K.toText key]) root schema key_value fuel
                | key <- KM.keys obj
                ]
            _ -> []

          -- Validate required properties
          required_errors =
            [ ValidationError path $
              "Missing required property: " <> prop
            | prop <- required_props
            , isNothing (obj KM.!? K.fromText prop)
            ]

          -- Handle dependent schemas and required
          dependent_errors = validateDependencies path root km obj fuel
      in  concat [prop_errors, pattern_prop_errors, additional_errors, property_name_errors, required_errors, dependent_errors]
    _ -> []

-- | Validate dependentSchemas and dependentRequired.
--
-- - dependentSchemas: if key K present, validate the entire object against a schema S
-- - dependentRequired: if key K present, require listed properties to also be present
validateDependencies :: [Text] -> Value -> KM.KeyMap Value -> KM.KeyMap Value -> Int -> [ValidationError]
validateDependencies path root km obj fuel =
  let dep_schemas_errors = case km KM.!? "dependentSchemas" of
        Just (Object dep_schemas) ->
          concat
            [ if isJust (obj KM.!? dep_key)
              then validateValue path root dep_schema (Object obj) fuel
              else []
            | (dep_key, dep_schema) <- KM.toList dep_schemas
            ]
        _ -> []

      dep_required_errors = case km KM.!? "dependentRequired" of
        Just (Object dep_required) ->
          concat
            [ if isJust (obj KM.!? dep_key)
              then
                ( case dep_value of
                    Array required_arr ->
                      [ ValidationError path $
                        "Property '" <> K.toText dep_key <> "' requires property: " <> req_prop
                      | String req_prop <- V.toList required_arr
                      , isNothing (obj KM.!? K.fromText req_prop)
                      ]
                    _ -> []
                )
              else []
            | (dep_key, dep_value) <- KM.toList dep_required
            ]
        _ -> []
  in  dep_schemas_errors <> dep_required_errors

-- | Validate array items
-- Implements 2020-12 array applicators semantics:
--  - "prefixItems": array of schemas applied positionally to the first N items
--  - "items": schema applied to all items with index >= N (N = prefixItems length, or 0 if absent)
--  - "items": false forbids any items beyond N; if N == 0, array must be empty
-- | Validate the "items" applicator for arrays in 2020-12.
--
-- Semantics:
--  - prefixItems: schemas applied positionally to first N items
--  - items: schema for items at index >= N; items=false forbids any items beyond N
validateItems :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateItems path root km value fuel =
  case value of
    Array arr ->
      let prefix_len = case km KM.!? "prefixItems" of
            Just (Array prefixes) -> Just (V.length prefixes)
            _ -> Just 0
          n = fromMaybe 0 prefix_len
          arr_len = V.length arr
          rest_indices = [n .. arr_len - 1]
      in  case km KM.!? "items" of
            Just (Bool False) ->
              [ ValidationError path $
                "Array has "
                  <> T.pack (show arr_len)
                  <> " items but only "
                  <> T.pack (show n)
                  <> " allowed"
              | arr_len > n
              ]
            Just item_schema ->
              concat
                [ validateValue (path <> [T.pack $ show i]) root item_schema (arr V.! i) fuel
                | i <- rest_indices
                ]
            Nothing -> []
    _ -> []

-- | Validate the "prefixItems" applicator for arrays (positionally for the first N items).
validatePrefixItems :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validatePrefixItems path root km value fuel =
  case value of
    Array arr ->
      case km KM.!? "prefixItems" of
        Just (Array prefixes) ->
          let limit = min (V.length prefixes) (V.length arr)
          in  concat
                [ validateValue (path <> [T.pack $ show i]) root (prefixes V.! i) (arr V.! i) fuel
                | i <- [0 .. limit - 1]
                ]
        _ -> []
    _ -> []

-- | Validate string constraints: minLength, maxLength, pattern.
-- Note: "format" is treated as an annotation and not asserted.
validateStringConstraints :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateStringConstraints path km value =
  case value of
    String str ->
      let l = unicodeLength str
      in  concat
            [ case km KM.!? "minLength" of
                Just (Number n)
                  | Just min_len <- Sci.toBoundedInteger n ->
                      [ ValidationError path $
                        "String length "
                          <> T.pack (show l)
                          <> " is less than minimum "
                          <> T.pack (show min_len)
                      | l < (min_len :: Int)
                      ]
                _ -> []
            , case km KM.!? "maxLength" of
                Just (Number n)
                  | Just max_len <- Sci.toBoundedInteger n ->
                      ( [ ValidationError path $
                          "String length "
                            <> T.pack (show l)
                            <> " exceeds maximum "
                            <> T.pack (show max_len)
                        | l > (max_len :: Int)
                        ]
                      )
                _ -> []
            , case km KM.!? "pattern" of
                Just (String pattern) ->
                  [ValidationError path $ "String does not match pattern: " <> pattern | not (str =~ pattern)]
                _ -> []
                -- format validation is optional and can be added later
            ]
    _ -> []

-- | Validate numeric constraints: minimum, maximum, exclusiveMinimum, exclusiveMaximum, multipleOf.
-- multipleOf is checked exactly using rationals to avoid floating point error.
validateNumberConstraints :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateNumberConstraints path km value =
  case value of
    Number num ->
      let fmt :: Scientific -> Text
          fmt = T.pack . Sci.formatScientific Sci.Generic Nothing
      in  concat
            [ case km KM.!? "minimum" of
                Just (Number min_val) ->
                  [ ValidationError path $
                    "Value " <> fmt num <> " is less than minimum " <> fmt min_val
                  | num < min_val
                  ]
                _ -> []
            , case km KM.!? "exclusiveMinimum" of
                Just (Number min_val) ->
                  [ ValidationError path $
                    "Value " <> fmt num <> " is not greater than exclusiveMinimum " <> fmt min_val
                  | num <= min_val
                  ]
                _ -> []
            , case km KM.!? "maximum" of
                Just (Number max_val) ->
                  [ ValidationError path $
                    "Value " <> fmt num <> " exceeds maximum " <> fmt max_val
                  | num > max_val
                  ]
                _ -> []
            , case km KM.!? "exclusiveMaximum" of
                Just (Number max_val) ->
                  [ ValidationError path $
                    "Value " <> fmt num <> " is not less than exclusiveMaximum " <> fmt max_val
                  | num >= max_val
                  ]
                _ -> []
            , case km KM.!? "multipleOf" of
                Just (Number divisor) ->
                  ( [ ValidationError path $
                      "Value " <> fmt num <> " is not a multiple of " <> fmt divisor
                    | not (isMultipleOf num divisor)
                    ]
                  )
                _ -> []
            ]
    _ -> []

-- | Validate logical combinators: anyOf, oneOf, allOf, not.
validateCombinators :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateCombinators path root km value fuel =
  concat
    [ validateAnyOf path root km value fuel
    , validateOneOf path root km value fuel
    , validateAllOf path root km value fuel
    , validateNot path root km value fuel
    ]

-- | anyOf: valid if at least one subschema validates (merges annotations of matching subschemas).
validateAnyOf :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateAnyOf path root km value fuel =
  case km KM.!? "anyOf" of
    Just (Array schemas) ->
      let results = [validateValue path root schema value fuel | schema <- V.toList schemas]
      in  [ValidationError path "Value does not match any of the schemas in 'anyOf'" | not (any null results)]
    _ -> []

-- | oneOf: valid if exactly one subschema validates.
validateOneOf :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateOneOf path root km value fuel =
  case km KM.!? "oneOf" of
    Just (Array schemas) ->
      let results = [validateValue path root schema value fuel | schema <- V.toList schemas]
          valid_count = length $ filter null results
      in  case valid_count of
            0 -> [ValidationError path "Value does not match any schema in 'oneOf'"]
            1 -> []
            _ ->
              [ ValidationError path $ "Value matches " <> T.pack (show valid_count) <> " schemas in 'oneOf' but must match exactly one"
              ]
    _ -> []

-- | allOf: valid only if all subschemas validate; accumulates all errors.
validateAllOf :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateAllOf path root km value fuel =
  case km KM.!? "allOf" of
    Just (Array schemas) ->
      concat [validateValue path root schema value fuel | schema <- V.toList schemas]
    _ -> []

-- | not: valid only if the subschema does not validate.
validateNot :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateNot path root km value fuel =
  case km KM.!? "not" of
    Just not_schema ->
      [ValidationError path "Value matches schema in 'not'" | null (validateValue path root not_schema value fuel)]
    _ -> []

-- | Validate array count/membership constraints: minItems, maxItems, uniqueItems, contains.
validateArrayConstraints :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateArrayConstraints path root km value fuel =
  case value of
    Array arr ->
      concat
        [ case km KM.!? "minItems" of
            Just (Number n)
              | Just min_items <- Sci.toBoundedInteger n ->
                  [ ValidationError path $
                    "Array has "
                      <> T.pack (show $ V.length arr)
                      <> " items but minimum is "
                      <> T.pack (show min_items)
                  | V.length arr < min_items
                  ]
            _ -> []
        , case km KM.!? "maxItems" of
            Just (Number n)
              | Just max_items <- Sci.toBoundedInteger n ->
                  [ ValidationError path $
                    "Array has "
                      <> T.pack (show $ V.length arr)
                      <> " items but maximum is "
                      <> T.pack (show max_items)
                  | V.length arr > max_items
                  ]
            _ -> []
        , case km KM.!? "uniqueItems" of
            Just (Bool True) ->
              let items = V.toList arr
              in  [ValidationError path "Array items are not unique" | length items /= length (nub items)]
            _ -> []
        , validateContains path root km arr fuel
        ]
    _ -> []

-- | Validate contains constraints
-- | Validate contains/minContains/maxContains: counts items matching the subschema.
-- Note: when "contains" is false, minContains defaults to 1, making any array invalid.
validateContains :: [Text] -> Value -> KM.KeyMap Value -> V.Vector Value -> Int -> [ValidationError]
validateContains path root km arr fuel =
  case km KM.!? "contains" of
    Nothing -> []
    Just contains_schema ->
      let matches = V.filter (\item -> null $ validateValue path root contains_schema item fuel) arr
          match_count = V.length matches
          min_contains = case km KM.!? "minContains" of
            Just (Number n) -> fromMaybe 1 (Sci.toBoundedInteger n)
            _ -> 1
          max_contains = case km KM.!? "maxContains" of
            Just (Number n) -> Sci.toBoundedInteger n
            _ -> Nothing
      in  concat
            [ [ ValidationError path $
                "Array has "
                  <> T.pack (show match_count)
                  <> " matching items but minContains is "
                  <> T.pack (show min_contains)
              | match_count < min_contains
              ]
            , case max_contains of
                Just max_cont ->
                  [ ValidationError path $
                    "Array has "
                      <> T.pack (show match_count)
                      <> " matching items but maxContains is "
                      <> T.pack (show max_cont)
                  | match_count > max_cont
                  ]
                Nothing -> []
            ]

-- | Validate object constraints (minProperties, maxProperties)
-- | Validate object property count constraints: minProperties and maxProperties.
validateObjectConstraints :: [Text] -> KM.KeyMap Value -> Value -> [ValidationError]
validateObjectConstraints path km value =
  case value of
    Object obj ->
      let prop_count = KM.size obj
      in  concat
            [ case km KM.!? "minProperties" of
                Just (Number n)
                  | Just min_props <- Sci.toBoundedInteger n ->
                      [ ValidationError path $
                        "Object has "
                          <> T.pack (show prop_count)
                          <> " properties but minimum is "
                          <> T.pack (show min_props)
                      | prop_count < min_props
                      ]
                _ -> []
            , case km KM.!? "maxProperties" of
                Just (Number n)
                  | Just max_props <- Sci.toBoundedInteger n ->
                      [ ValidationError path $
                        "Object has "
                          <> T.pack (show prop_count)
                          <> " properties but maximum is "
                          <> T.pack (show max_props)
                      | prop_count > max_props
                      ]
                _ -> []
            ]
    _ -> []

-- | Validate conditional logic (if/then/else)
-- | Validate conditional keywords: if/then/else.
validateConditional :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateConditional path root km value fuel =
  case km KM.!? "if" of
    Nothing -> []
    Just if_schema ->
      let if_matches = null $ validateValue path root if_schema value fuel
      in  if if_matches
            then case km KM.!? "then" of
              Just then_schema -> validateValue path root then_schema value fuel
              Nothing -> []
            else case km KM.!? "else" of
              Just else_schema -> validateValue path root else_schema value fuel
              Nothing -> []

-- | Check if a number is a multiple of another
-- Exact multipleOf using rationals (avoids floating point error).
-- | Exact multipleOf check using rationals (denominator must be 1).
isMultipleOf :: Scientific -> Scientific -> Bool
isMultipleOf value divisor =
  let q = toRational value / toRational divisor
  in  denominator q == 1

-- Count Unicode code points (not UTF-16 code units)

-- | Count Unicode code points (not UTF-16 code units) for string length.
unicodeLength :: Text -> Int
unicodeLength = T.foldl' (\n _ -> n + 1) 0

-- | Helper to show JSON values as text
showJSON :: Value -> Text
showJSON = T.take 100 . T.pack . show

-- | Resolve a local $ref against the root schema using JSON Pointer
-- Supports only fragment starting with '#'. External URIs and anchors are not resolved.
-- | Resolve a local $ref against the root schema using JSON Pointer.
-- Supports only fragments starting with '#'; external URIs/anchors are not resolved.
resolveRef :: Value -> Text -> Maybe Value
resolveRef root ref_path =
  case T.uncons ref_path of
    Just ('#', rest) ->
      -- empty fragment or pointer
      if T.null rest
        then Just root
        else
          if T.head rest == '/'
            then jsonPointer root (T.tail rest)
            else Nothing -- unsupported non-pointer fragment
    _ -> Nothing -- unsupported non-fragment refs

-- | Evaluate a JSON Pointer (RFC 6901) against a JSON value.
jsonPointer :: Value -> Text -> Maybe Value
jsonPointer v ptr =
  let tokens = map unescapePointer $ T.splitOn "/" ptr
  in  go v tokens
  where
    go :: Value -> [Text] -> Maybe Value
    go cur [] = Just cur
    go (Object km) (t : ts) =
      case KM.lookup (K.fromText t) km of
        Just nxt -> go nxt ts
        Nothing -> Nothing
    go (Array arr) (t : ts)
      | T.all isDigit t && not (T.null t) =
          let i = read (T.unpack t) :: Int
          in  if i >= 0 && i < V.length arr then go (arr V.! i) ts else Nothing
      | otherwise = Nothing
    go _ _ = Nothing

    unescapePointer :: Text -> Text
    unescapePointer = T.replace "~1" "/" . T.replace "~0" "~"

-- | Validate "unevaluatedProperties":
-- Forbids or constrains properties that were not covered by properties,
-- patternProperties, or additionalProperties at the current schema location.
-- This implementation approximates evaluated sets locally and does not
-- perform full annotation merging across nested applicators.
validateUnevaluatedProperties :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateUnevaluatedProperties path root km value fuel =
  case (value, km KM.!? "unevaluatedProperties") of
    (Object obj, Just uneval_schema) ->
      let props_schema = km KM.!? "properties"
          pattern_props_schema = km KM.!? "patternProperties"

          props_handled = case props_schema of
            Just (Object schema_obj) -> [k | (k, _) <- KM.toList schema_obj, KM.member k obj]
            _ -> []
          patterns_handled = case pattern_props_schema of
            Just (Object patterns) ->
              [ obj_key
              | obj_key <- KM.keys obj
              , any (\pattern_key -> K.toText obj_key =~ K.toText pattern_key) (KM.keys patterns)
              ]
            _ -> []
          extras = [k | k <- KM.keys obj, k `notElem` props_handled, k `notElem` patterns_handled]

          -- If additionalProperties is present, consider extras handled by it
          extras_handled_by_additional = case km KM.!? "additionalProperties" of
            Just _ -> extras
            Nothing -> []

          unevaluated_keys = [k | k <- extras, k `notElem` extras_handled_by_additional]

          applyUnevaluated (Bool False) =
            [ ValidationError path $
              "Unevaluated property not allowed: " <> K.toText k
            | k <- unevaluated_keys
            ]
          applyUnevaluated sch =
            concat
              [ case obj KM.!? k of
                Just v -> validateValue (path <> [K.toText k]) root sch v fuel
                Nothing -> []
              | k <- unevaluated_keys
              ]
      in  applyUnevaluated uneval_schema
    _ -> []

-- | Validate "unevaluatedItems":
-- Applies to array indices not covered by prefixItems, items, or contains When false, any such index is prohibited; when a schema, each such item
-- is validated against it. This uses local evaluated-set approximation.
validateUnevaluatedItems :: [Text] -> Value -> KM.KeyMap Value -> Value -> Int -> [ValidationError]
validateUnevaluatedItems path root km value fuel =
  case (value, km KM.!? "unevaluatedItems") of
    (Array arr, Just uneval_schema) ->
      let arr_len = V.length arr
          -- prefix items coverage
          prefix_count = case km KM.!? "prefixItems" of
            Just (Array prefixes) -> min (V.length prefixes) arr_len
            _ -> 0
          prefix_idx = [0 .. prefix_count - 1]
          -- items coverage for rest if present
          rest_idx = case km KM.!? "items" of
            Just _ -> [prefix_count .. arr_len - 1]
            Nothing -> []
          -- contains matched indices (optional)
          contains_idx = case km KM.!? "contains" of
            Nothing -> []
            Just sch ->
              [ i
              | i <- [0 .. arr_len - 1]
              , null $ validateValue path root sch (arr V.! i) fuel
              ]
          evaluated = nub (prefix_idx <> rest_idx <> contains_idx)
          unevaluated = [i | i <- [0 .. arr_len - 1], i `notElem` evaluated]
          applyItems (Bool False) =
            [ ValidationError path $
              "Unevaluated item not allowed at index " <> T.pack (show i)
            | i <- unevaluated
            ]
          applyItems sch =
            concat
              [ validateValue (path <> [T.pack (show i)]) root sch (arr V.! i) fuel
              | i <- unevaluated
              ]
      in  applyItems uneval_schema
    _ -> []
