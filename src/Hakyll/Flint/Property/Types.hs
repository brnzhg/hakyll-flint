-- | Core vocabulary for user-declared metadata properties.
--
-- These are deliberately small newtypes rather than bare 'String's so that a
-- signature like @'PropertyName' -> 'PropertyValue' -> ...@ says what each
-- argument /is/, and the compiler stops you from swapping them.
module Hakyll.Flint.Property.Types
    ( -- * Declaring properties
      Property (..)
    , PropertyName (..)
    , Cardinality (..)
    , singleValued
    , manyValued

      -- * Values
    , PropertyValue (..)
    , ValueSlug (..)
    , slugify
    , valueSlug
    , valueString
    , slugString

      -- * Template keys
    , ContextKey (..)

      -- * Errors
    , PropertyError (..)
    , renderPropertyError
    ) where

import Data.Char (isAlphaNum, toLower)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Hakyll.Core.Identifier (Identifier)

--------------------------------------------------------------------------------
-- Declaring properties

-- | The metadata (front matter) field a property is read from, e.g.
-- @ingredient@ for a page with @ingredient: [chicken, leek]@.
newtype PropertyName = PropertyName {unPropertyName :: Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord, IsString)

-- | How many values a page may give a property.
data Cardinality
    = -- | At most one value. A list in the front matter is an error.
      SingleValued
    | -- | Any number of values, written as a YAML list or a comma-separated
      -- string (the same convention as Hakyll's @tags@).
      ManyValued
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A user-declared property. Pages that don't mention the property simply
-- have no values for it.
data Property = Property
    { propertyName :: PropertyName
    , propertyCardinality :: Cardinality
    }
    deriving stock (Show, Eq, Ord)

-- | @singleValued "cuisine"@
singleValued :: PropertyName -> Property
singleValued name = Property name SingleValued

-- | @manyValued "ingredient"@
manyValued :: PropertyName -> Property
manyValued name = Property name ManyValued

--------------------------------------------------------------------------------
-- Values

-- | A property value as written by the author (whitespace-trimmed), e.g.
-- @Chicken Thigh@. This is what gets displayed.
newtype PropertyValue = PropertyValue {unPropertyValue :: Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord, IsString)

-- | The URL-safe identity of a value, e.g. @chicken-thigh@. Two values with
-- the same slug (@Chicken@ and @chicken@) are treated as the same value.
newtype ValueSlug = ValueSlug {unValueSlug :: Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord, IsString)

-- | Lower-case, keep alphanumerics (including non-ASCII letters), and
-- collapse every other run of characters into a single @-@.
--
-- >>> slugify "Chicken Thigh (bone-in)"
-- "chicken-thigh-bone-in"
slugify :: Text -> Text
slugify =
    T.intercalate "-"
        . filter (not . T.null)
        . T.split (not . isAlphaNum)
        . T.map toLower

valueSlug :: PropertyValue -> ValueSlug
valueSlug = ValueSlug . slugify . unPropertyValue

-- | For Hakyll, which wants 'String's.
valueString :: PropertyValue -> String
valueString = T.unpack . unPropertyValue

slugString :: ValueSlug -> String
slugString = T.unpack . unValueSlug

--------------------------------------------------------------------------------
-- Template keys

-- | The name a field is bound to in a Hakyll template, i.e. the @foo@ in
-- @$foo$@ or @$for(foo)$@.
newtype ContextKey = ContextKey {unContextKey :: String}
    deriving stock (Show)
    deriving newtype (Eq, Ord, IsString)

--------------------------------------------------------------------------------
-- Errors

data PropertyError
    = -- | A 'SingleValued' property was given several values.
      TooManyValues Property Identifier [PropertyValue]
    | -- | The front matter held something we can't read as values (e.g. a
      -- nested mapping).
      UnreadableValue Property Identifier Text
    | -- | A value that has no alphanumeric characters, so no usable slug.
      EmptySlug Property Identifier PropertyValue
    deriving stock (Show, Eq)

renderPropertyError :: PropertyError -> String
renderPropertyError = \case
    TooManyValues p ident vs ->
        prefix p ident
            <> "is single-valued but has "
            <> show (length vs)
            <> " values: "
            <> commaSeparated vs
    UnreadableValue p ident why ->
        prefix p ident <> "could not be read: " <> T.unpack why
    EmptySlug p ident (PropertyValue v) ->
        prefix p ident
            <> "has value "
            <> show v
            <> " which contains no letters or digits"
  where
    commaSeparated = T.unpack . T.intercalate ", " . map unPropertyValue

    prefix p ident =
        "hakyll-flint: property '"
            <> T.unpack (unPropertyName $ propertyName p)
            <> "' of "
            <> show ident
            <> " "
