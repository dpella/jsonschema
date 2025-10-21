{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONSchema.Spec (tests) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.String (fromString)
import Data.Text (Text)
import Data.Vector (fromList)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Data.JSON.JSONSchema
import Test.Tasty
import Test.Tasty.HUnit

-- Simple product type (record)
data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Person

instance ToJSONSchema Person

-- Sum type with non-record and product constructors
data Shape
  = Circle Double
  | Rectangle Double Double
  deriving (Show, Eq, Generic)

instance ToJSON Shape

instance ToJSONSchema Shape

-- Sum type with record constructors (tests tagged record encoding)
data Animal
  = Dog {dogName :: Text, dogAge :: Int}
  | Cat {catName :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Animal

instance ToJSONSchema Animal

-- Non-record product with 3 fields (tests prefixItems flattening)
data Triple = Triple Int Text Bool
  deriving (Show, Eq, Generic)

instance ToJSON Triple

instance ToJSONSchema Triple

-- Recursive non-record sum type for ToJSON encoding test
data RTree
  = Leaf Int
  | Node RTree RTree
  deriving (Show, Eq, Generic)

instance ToJSON RTree

instance ToJSONSchema RTree

tests :: TestTree
tests =
  testGroup
    "JSON Schema"
    [ validationWorks
    , validationSpecCoverage
    , deriveWorks
    , schemaRecursiveWorks
    ]

validationWorks :: TestTree
validationWorks =
  testGroup
    "Validation Works"
    [ testCase "Valid object with required properties" $ do
        let schema =
              object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "name" .= object ["type" .= ("string" :: Text)]
                      , "age" .= object ["type" .= ("integer" :: Text)]
                      ]
                , "required" .= (["name", "age"] :: [Text])
                , "additionalProperties" .= False
                ]
            value = object ["name" .= ("Alice" :: Text), "age" .= (30 :: Int)]
        assertBool "Expected valid JSON for schema" (validateJSONSchema schema value)
    , testCase "Missing required property fails" $ do
        let schema =
              object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "name" .= object ["type" .= ("string" :: Text)]
                      , "age" .= object ["type" .= ("integer" :: Text)]
                      ]
                , "required" .= (["name", "age"] :: [Text])
                , "additionalProperties" .= False
                ]
            value = object ["name" .= ("Alice" :: Text)]
        assertBool "Expected invalid JSON for schema" (not $ validateJSONSchema schema value)
    , testCase "Wrong type fails" $ do
        let schema =
              object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "name" .= object ["type" .= ("string" :: Text)]
                      , "age" .= object ["type" .= ("integer" :: Text)]
                      ]
                , "required" .= (["name", "age"] :: [Text])
                , "additionalProperties" .= False
                ]
            value = object ["name" .= ("Alice" :: Text), "age" .= ("30" :: Text)]
        assertBool "Expected invalid JSON for schema" (not $ validateJSONSchema schema value)
    , testCase "Additional property rejected when additionalProperties=false" $ do
        let schema =
              object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= object
                      [ "name" .= object ["type" .= ("string" :: Text)]
                      , "age" .= object ["type" .= ("integer" :: Text)]
                      ]
                , "required" .= (["name", "age"] :: [Text])
                , "additionalProperties" .= False
                ]
            value = object ["name" .= ("Alice" :: Text), "age" .= (30 :: Int), "extra" .= (True :: Bool)]
        assertBool "Expected invalid JSON for schema" (not $ validateJSONSchema schema value)
    , testCase "items=false with no prefixItems forbids any items" $ do
        let schema = object ["type" .= ("array" :: Text), "items" .= False]
        assertBool "[] should validate" (validateJSONSchema schema (Array mempty))
        assertBool "[1] should not validate" (not $ validateJSONSchema schema (toJSON ([1 :: Int] :: [Int])))
    , testCase "items applies only to items after prefixItems" $ do
        let schema =
              object
                [ "type" .= ("array" :: Text)
                , "prefixItems" .= [object ["type" .= ("integer" :: Text)]]
                , "items" .= object ["type" .= ("string" :: Text)]
                ]
            j1 = toJSON ([1 :: Int] :: [Int])
            j2 = Array (fromList [Number 1, String "x"])
            j3 = toJSON ([1 :: Int, 2] :: [Int])
        assertBool "[1] valid (no rest)" (validateJSONSchema schema j1)
        assertBool "[1, \"x\"] valid (rest string)" (validateJSONSchema schema j2)
        assertBool "[1, 2] invalid (rest not string)" (not $ validateJSONSchema schema j3)
    ]

-- A comprehensive set of tests covering the 2020-12 validation vocabulary
validationSpecCoverage :: TestTree
validationSpecCoverage =
  testGroup
    "Validation Spec Coverage"
    [ -- type
      testCase "type: integer vs number" $ do
        let sInt = object ["type" .= ("integer" :: Text)]
        assertBool "3 integer" (validateJSONSchema sInt (Number 3))
        assertBool "3.5 not integer" (not $ validateJSONSchema sInt (Number 3.5))
    , testCase "type: array of types" $ do
        let s = object ["type" .= (["string", "null"] :: [Text])]
        assertBool "string ok" (validateJSONSchema s (String "x"))
        assertBool "null ok" (validateJSONSchema s Null)
        assertBool "bool not ok" (not $ validateJSONSchema s (Bool True))
    , -- const, enum
      testCase "const exact match" $ do
        let s = object ["const" .= object ["a" .= (1 :: Int)]]
        assertBool "match" (validateJSONSchema s (object ["a" .= (1 :: Int)]))
        assertBool "no match" (not $ validateJSONSchema s (object ["a" .= (2 :: Int)]))
    , testCase "enum membership" $ do
        let s = object ["enum" .= ([String "a", String "b"] :: [Value])]
        assertBool "a ok" (validateJSONSchema s (String "a"))
        assertBool "c not ok" (not $ validateJSONSchema s (String "c"))
    , -- string: minLength, maxLength, pattern
      testCase "string length counts code points" $ do
        let s = object ["type" .= ("string" :: Text), "minLength" .= (1 :: Int), "maxLength" .= (1 :: Int)]
        assertBool "emoji single code point ok" (validateJSONSchema s (String "😊"))
    , testCase "string pattern" $ do
        let s = object ["type" .= ("string" :: Text), "pattern" .= ("^[a-z]+$" :: Text)]
        assertBool "lowercase ok" (validateJSONSchema s (String "abc"))
        assertBool "uppercase not ok" (not $ validateJSONSchema s (String "Abc"))
    , -- number: minimum/maximum/exclusive*/multipleOf
      testCase "number bounds inclusive/exclusive" $ do
        let s =
              object
                [ "type" .= ("number" :: Text)
                , "minimum" .= (1 :: Int)
                , "exclusiveMaximum" .= (3 :: Int)
                ]
        assertBool "1 ok" (validateJSONSchema s (Number 1))
        assertBool "2.9 ok" (validateJSONSchema s (Number 2.9))
        assertBool "3 not ok" (not $ validateJSONSchema s (Number 3))
    , testCase "minimum respects large integers" $ do
        let s =
              object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= Number 9007199254740993
                ]
        assertBool "just below fails" (not $ validateJSONSchema s (Number 9007199254740992))
        assertBool "threshold passes" (validateJSONSchema s (Number 9007199254740993))
    , testCase "exclusiveMaximum handles high precision decimals" $ do
        let bound = Number 0.12345678901234567890
            s =
              object
                [ "type" .= ("number" :: Text)
                , "exclusiveMaximum" .= bound
                ]
        assertBool "slightly smaller ok" (validateJSONSchema s (Number 0.12345678901234567889))
        assertBool "equal fails" (not $ validateJSONSchema s bound)
        assertBool "larger fails" (not $ validateJSONSchema s (Number 0.12345678901234567891))
    , testCase "multipleOf exact with rational" $ do
        let s = object ["type" .= ("number" :: Text), "multipleOf" .= (0.1 :: Double)]
        assertBool "0.3 ok" (validateJSONSchema s (Number 0.3))
        assertBool "0.31 not ok" (not $ validateJSONSchema s (Number 0.31))
    , -- array: minItems/maxItems/uniqueItems/contains/minContains/maxContains
      testCase "array min/max items" $ do
        let s = object ["type" .= ("array" :: Text), "minItems" .= (1 :: Int), "maxItems" .= (2 :: Int)]
        assertBool "[1] ok" (validateJSONSchema s (toJSON ([1 :: Int] :: [Int])))
        assertBool "[] not ok" (not $ validateJSONSchema s (toJSON ([] :: [Int])))
        assertBool "[1,2,3] not ok" (not $ validateJSONSchema s (toJSON ([1 :: Int, 2, 3] :: [Int])))
    , testCase "uniqueItems deep equality" $ do
        let s = object ["type" .= ("array" :: Text), "uniqueItems" .= True]
        assertBool
          "unique ok"
          (validateJSONSchema s (toJSON ([object ["a" .= (1 :: Int)], object ["a" .= (2 :: Int)]] :: [Value])))
        assertBool
          "duplicate not ok"
          (not $ validateJSONSchema s (toJSON ([object ["a" .= (1 :: Int)], object ["a" .= (1 :: Int)]] :: [Value])))
    , testCase "contains with min/maxContains" $ do
        let s =
              object
                [ "type" .= ("array" :: Text)
                , "contains" .= object ["type" .= ("integer" :: Text)]
                , "minContains" .= (2 :: Int)
                , "maxContains" .= (3 :: Int)
                ]
        assertBool "two ints ok" (validateJSONSchema s (toJSON ([Number 1, String "x", Number 2] :: [Value])))
        assertBool "one int not ok" (not $ validateJSONSchema s (toJSON ([Number 1, String "x"] :: [Value])))
        assertBool "four ints not ok" (not $ validateJSONSchema s (toJSON ([1 :: Int, 2, 3, 4] :: [Int])))
    , testCase "contains=false forbids any matches by minContains default" $ do
        let s = object ["type" .= ("array" :: Text), "contains" .= False]
        assertBool "[] not ok (minContains defaults to 1)" (not $ validateJSONSchema s (toJSON ([] :: [Int])))
        -- default minContains is 1 when contains is present, so any match is impossible
        assertBool "[1] not ok" (not $ validateJSONSchema s (toJSON ([1 :: Int] :: [Int])))
    , testCase "unevaluatedItems=false forbids trailing items not covered by prefix/items" $ do
        let s =
              object
                [ "type" .= ("array" :: Text)
                , "prefixItems" .= toJSON ([object ["type" .= ("integer" :: Text)]] :: [Value])
                , "unevaluatedItems" .= False
                ]
        assertBool "[1] ok" (validateJSONSchema s (toJSON ([1 :: Int] :: [Int])))
        assertBool "[1, \"x\"] not ok" (not $ validateJSONSchema s (toJSON ([Number 1, String "x"] :: [Value])))
    , -- object: required/properties/patternProperties/additionalProperties/propertyNames/min/max props
      testCase "required properties enforced" $ do
        let s =
              object
                [ "type" .= ("object" :: Text)
                , "properties" .= object ["a" .= object ["type" .= ("integer" :: Text)]]
                , "required" .= (["a"] :: [Text])
                ]
        assertBool "present ok" (validateJSONSchema s (object ["a" .= (1 :: Int)]))
        assertBool "missing not ok" (not $ validateJSONSchema s (object []))
    , testCase "patternProperties match" $ do
        let s =
              object
                [ "type" .= ("object" :: Text)
                , "patternProperties" .= object ["^x-" .= object ["type" .= ("string" :: Text)]]
                ]
        assertBool "x-foo ok" (validateJSONSchema s (object ["x-foo" .= ("bar" :: Text)]))
        assertBool "x-foo wrong type" (not $ validateJSONSchema s (object ["x-foo" .= (1 :: Int)]))
    , testCase "additionalProperties schema applies to extras" $ do
        let s =
              object
                [ "type" .= ("object" :: Text)
                , "properties" .= object ["a" .= object ["type" .= ("integer" :: Text)]]
                , "additionalProperties" .= object ["type" .= ("string" :: Text)]
                ]
        assertBool "extra string ok" (validateJSONSchema s (object ["a" .= (1 :: Int), "b" .= ("x" :: Text)]))
        assertBool "extra int not ok" (not $ validateJSONSchema s (object ["a" .= (1 :: Int), "b" .= (2 :: Int)]))
    , testCase "unevaluatedProperties=false forbids extras not covered by props/patterns" $ do
        let s =
              object
                [ "type" .= ("object" :: Text)
                , "properties" .= object ["a" .= object []]
                , "unevaluatedProperties" .= False
                ]
        assertBool "only a ok" (validateJSONSchema s (object ["a" .= (1 :: Int)]))
        assertBool "extra b not ok" (not $ validateJSONSchema s (object ["a" .= (1 :: Int), "b" .= (2 :: Int)]))
    , testCase "local $ref into $defs resolves and validates" $ do
        let s =
              object
                [ "$defs" .= object ["posInt" .= object ["type" .= ("integer" :: Text), "minimum" .= (0 :: Int)]]
                , "type" .= ("object" :: Text)
                , "properties" .= object ["age" .= object ["$ref" .= ("#/$defs/posInt" :: Text)]]
                , "required" .= (["age"] :: [Text])
                ]
        assertBool "age=5 ok" (validateJSONSchema s (object ["age" .= (5 :: Int)]))
        assertBool "age=-1 not ok" (not $ validateJSONSchema s (object ["age" .= (-1 :: Int)]))
    , testCase "unevaluatedItems with contains marks matches as evaluated; remainder must satisfy schema" $ do
        let s =
              object
                [ "type" .= ("array" :: Text)
                , "contains" .= object ["type" .= ("integer" :: Text)]
                , "minContains" .= (1 :: Int)
                , "unevaluatedItems" .= object ["type" .= ("string" :: Text)]
                ]
        assertBool "[1, \"x\", \"y\"] ok" (validateJSONSchema s (toJSON ([Number 1, String "x", String "y"] :: [Value])))
        assertBool
          "[1, false] not ok (false not string)"
          (not $ validateJSONSchema s (toJSON ([Number 1, Bool False] :: [Value])))
    , testCase "propertyNames constraint" $ do
        let s = object ["type" .= ("object" :: Text), "propertyNames" .= object ["pattern" .= ("^[a-z]+$" :: Text)]]
        assertBool "lowercase key ok" (validateJSONSchema s (object ["abc" .= (1 :: Int)]))
        assertBool "uppercase key not ok" (not $ validateJSONSchema s (object ["Abc" .= (1 :: Int)]))
    , testCase "min/max properties" $ do
        let s = object ["type" .= ("object" :: Text), "minProperties" .= (1 :: Int), "maxProperties" .= (2 :: Int)]
        assertBool "one ok" (validateJSONSchema s (object ["a" .= (1 :: Int)]))
        assertBool "none not ok" (not $ validateJSONSchema s (object []))
        assertBool
          "three not ok"
          (not $ validateJSONSchema s (object ["a" .= (1 :: Int), "b" .= (2 :: Int), "c" .= (3 :: Int)]))
    , -- dependentRequired / dependentSchemas
      testCase "dependentRequired enforces required when key present" $ do
        let s =
              object
                [ "type" .= ("object" :: Text)
                , "dependentRequired" .= object ["credit_card" .= (["billing_address"] :: [Text])]
                ]
        assertBool "missing credit_card ok" (validateJSONSchema s (object ["name" .= ("Bob" :: Text)]))
        assertBool "present without dependency not ok" (not $ validateJSONSchema s (object ["credit_card" .= ("123" :: Text)]))
        assertBool
          "present with dependency ok"
          (validateJSONSchema s (object ["credit_card" .= ("123" :: Text), "billing_address" .= ("A" :: Text)]))
    , testCase "dependentSchemas applies object schema when key present" $ do
        let dep = object ["properties" .= object ["a" .= object ["const" .= (1 :: Int)]], "required" .= (["a"] :: [Text])]
            s = object ["type" .= ("object" :: Text), "dependentSchemas" .= object ["flag" .= dep]]
        assertBool "no flag ok" (validateJSONSchema s (object ["a" .= (2 :: Int)]))
        assertBool "flag requires a=1" (validateJSONSchema s (object ["flag" .= True, "a" .= (1 :: Int)]))
        assertBool "flag with wrong a not ok" (not $ validateJSONSchema s (object ["flag" .= True, "a" .= (2 :: Int)]))
    , -- combinators
      testCase "anyOf passes if any schema matches" $ do
        let s = object ["anyOf" .= ([object ["type" .= ("string" :: Text)], object ["type" .= ("integer" :: Text)]] :: [Value])]
        assertBool "int ok" (validateJSONSchema s (Number 3))
    , testCase "oneOf exactly one" $ do
        let s = object ["oneOf" .= ([object ["type" .= ("number" :: Text)], object ["minimum" .= (0 :: Int)]] :: [Value])]
        -- number 3 matches both type=number and minimum=0; should fail
        assertBool "matches both not ok" (not $ validateJSONSchema s (Number 3))
    , testCase "allOf accumulates constraints" $ do
        let s = object ["allOf" .= ([object ["type" .= ("number" :: Text)], object ["maximum" .= (5 :: Int)]] :: [Value])]
        assertBool "6 not ok" (not $ validateJSONSchema s (Number 6))
    , testCase "not inverts" $ do
        let s = object ["not" .= object ["type" .= ("null" :: Text)]]
        assertBool "null not ok" (not $ validateJSONSchema s Null)
    , -- conditionals if/then/else
      testCase "if/then/else branching" $ do
        let s =
              object
                [ "if" .= object ["properties" .= object ["a" .= object ["const" .= True]]]
                , "then" .= object ["required" .= (["b"] :: [Text])]
                , "else" .= object ["required" .= (["c"] :: [Text])]
                ]
        assertBool "a=true requires b" (not $ validateJSONSchema s (object ["a" .= True]))
        assertBool "a=false requires c" (not $ validateJSONSchema s (object ["a" .= False]))
    , -- format and content keywords are annotations in 2020-12: do not assert by default
      testCase "format does not assert by default" $ do
        let s = object ["type" .= ("string" :: Text), "format" .= ("email" :: Text)]
        assertBool "non-email still valid (annotation only)" (validateJSONSchema s (String "not-an-email"))
    , testCase "content keywords are annotations only" $ do
        let s =
              object
                [ "type" .= ("string" :: Text)
                , "contentEncoding" .= ("base64" :: Text)
                , "contentMediaType" .= ("image/png" :: Text)
                ]
        -- we do not auto-decode/validate: annotation only
        assertBool "arbitrary string still valid" (validateJSONSchema s (String "@@not-base64@@"))
    ]

deriveWorks :: TestTree
deriveWorks =
  testGroup
    "Derive Works (Aeson JSON validates against derived schema)"
    [ testCase "Person record encodes and validates" $ do
        let schema = toJSONSchema (Proxy :: Proxy Person)
            jPerson = toJSON (Person "Bob" 42)
        assertBool "Person JSON should validate against derived schema" (validateJSONSchema schema jPerson)
        assertBool "Person schema should reject missing fields" (not $ validateJSONSchema schema (object []))
    , testCase "Shape sum (non-record) constructors validate" $ do
        let schema = toJSONSchema (Proxy :: Proxy Shape)
            j1 = toJSON (Circle 1.5)
            j2 = toJSON (Rectangle 2.3 4.5)
        assertBool "Circle JSON should validate" (validateJSONSchema schema j1)
        assertBool "Rectangle JSON should validate" (validateJSONSchema schema j2)
    , testCase "Animal sum (record) constructors validate" $ do
        let schema = toJSONSchema (Proxy :: Proxy Animal)
            j1 = toJSON (Dog "Fido" 5)
            j2 = toJSON (Cat "Whiskers")
        assertBool "Dog JSON should validate" (validateJSONSchema schema j1)
        assertBool "Cat JSON should validate" (validateJSONSchema schema j2)
        assertBool "Dog schema should reject missing required fields"
          (not $
            validateJSONSchema schema
              (object ["tag" .= ("Dog" :: Text)])
          )
        assertBool "Dog schema should reject missing tag"
          (not $
            validateJSONSchema schema
              (object ["dogName" .= ("Fido" :: Text), "dogAge" .= (5 :: Int)])
          )
    , testCase "Triple non-record product flattens and validates" $ do
        let schema = toJSONSchema (Proxy :: Proxy Triple)
            jTriple = toJSON (Triple 1 "a" True)
        assertBool "Triple JSON should validate" (validateJSONSchema schema jTriple)
    , testCase "Maybe and list instances validate via derived schema" $ do
        let maybeSchema = toJSONSchema (Proxy :: Proxy (Maybe Int))
            listSchema = toJSONSchema (Proxy :: Proxy [Text])
        assertBool "Just Int validates" (validateJSONSchema maybeSchema (toJSON (Just (3 :: Int))))
        assertBool "Nothing validates" (validateJSONSchema maybeSchema Null)
        assertBool "[Text] validates" (validateJSONSchema listSchema (toJSON (["x", "y"] :: [Text])))
    , testCase "Either schema enforces discriminator semantics" $ do
        let schema = toJSONSchema (Proxy :: Proxy (Either Int Text))
            leftVal = toJSON (Left (3 :: Int) :: Either Int Text)
            rightVal = toJSON (Right ("hi" :: Text) :: Either Int Text)
            emptyObj = object []
            bothObj =
              object
                [ "Left" .= (1 :: Int)
                , "Right" .= ("oops" :: Text)
                ]
        assertBool "Left JSON should validate" (validateJSONSchema schema leftVal)
        assertBool "Right JSON should validate" (validateJSONSchema schema rightVal)
        assertBool "Empty object should be rejected" (not $ validateJSONSchema schema emptyObj)
        assertBool "Object with both constructors should be rejected" (not $ validateJSONSchema schema bothObj)
    ]

schemaRecursiveWorks :: TestTree
schemaRecursiveWorks =
  testGroup
    "ToJSONSchema Works (recursive type emits $ref)"
    [ testCase "Derived schema uses $defs and $ref for recursion" $ do
        let s = toJSONSchema (Proxy :: Proxy RTree)
        -- Top-level must be an object with $defs (and ideally $ref)
        case s of
          Object km -> do
            -- \$defs contains RTree definition, and its Node constructor contains $ref back
            case km KM.!? "$defs" of
              Just (Object defs) -> case defs KM.!? fromString "RTree" of
                Just (Object rtreeDef) -> do
                  -- look for 'anyOf' containing Node/Leaf. In Node path, ensure children $ref
                  case rtreeDef KM.!? fromString "anyOf" of
                    Just (Array alts) -> do
                      -- At least one alternative should be the tagged Node object with contents array
                      let hasRefBack =
                            V.any
                              ( \v -> case v of
                                  Object km2 -> case km2 KM.!? fromString "properties" of
                                    Just (Object props) -> case props KM.!? fromString "contents" of
                                      Just (Object cont) -> case cont KM.!? fromString "prefixItems" of
                                        Just (Array pfi) ->
                                          V.all
                                            ( \piV -> case piV of
                                                Object o -> case o KM.!? fromString "$ref" of
                                                  Just (String r) -> r == "#/$defs/RTree"
                                                  _ -> False
                                                _ -> False
                                            )
                                            pfi
                                        _ -> False
                                      _ -> False
                                    _ -> False
                                  _ -> False
                              )
                              alts
                      assertBool "Node contents references back to RTree via $ref" hasRefBack
                    _ -> assertFailure "RTree def missing anyOf"
                _ -> assertFailure "$defs.RTree missing"
              _ -> assertFailure "$defs missing"
          _ -> assertFailure "Top-level schema should be object"
    , testCase "Recursive value validates against derived schema" $ do
        let s = toJSONSchema (Proxy :: Proxy RTree)
            j = toJSON (Node (Leaf 1) (Leaf 2))
        assertBool "recursive value should validate" (validateJSONSchema s j)
    ]
