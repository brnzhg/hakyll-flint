-- | User-declared metadata properties: a generalisation of Hakyll's tags.
--
-- Declare a property, index it over a set of pages, then
--
--   * generate one page per value (@ingredient/chicken.html@) with
--     'valuePageRules',
--   * show a page's own values with 'propertyLinksField' or
--     'propertyValuesField',
--   * list the pages having some value from /any/ page with
--     'pagesWithValueField', or from inside a template or Markdown body with
--     'propertyQueryField': @$for(pagesWhere("ingredient", "chicken"))$@.
--
-- = Why this has no dependency cycles
--
-- Page listings are built from the index (computed from front matter while
-- the 'Rules' run) and render each listed page as an @'Item' ()@ that only
-- reads that page's /metadata/ and /route/. Nothing is 'load'ed, so a recipe
-- page may list every chicken recipe, including itself, without Hakyll
-- seeing a cycle. If you need listed pages' compiled /bodies/ (teasers, say),
-- compile them a second time under a 'Hakyll.Core.Identifier.setVersion' and
-- load that version instead; see the README.
module Hakyll.Flint.Property
    ( module Hakyll.Flint.Property.Types

      -- * Reading values from front matter
    , readPropertyValues
    , getPropertyValues

      -- * Indexing
    , PropertyIndex
    , ValueEntry (..)
    , buildPropertyIndex
    , valuePagesAt
    , indexProperty
    , indexPattern
    , indexDependency
    , indexEntries
    , lookupValue
    , pagesWithValue
    , valuesOfPage

      -- * One page per value
    , valuePageRules

      -- * Template contexts
    , PageListing (..)
    , defaultListing
    , pageRefContext
    , pageRefs
    , valueEntryContext
    , valueEntryContextWith
    , propertyLinksField
    , propertyValuesField
    , allValuesField
    , pagesWithValueField
    , valuePageContext
    , entryPagesField
    , propertyQueryField
    ) where

import Control.Monad (forM, forM_, unless, (<=<))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hakyll.Core.Compiler (Compiler, getRoute, noResult)
import Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import Hakyll.Core.Dependencies
    ( Dependency
    , DependencySelector (PatternDependency)
    , metadataDependency
    )
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Identifier.Pattern (Pattern, fromCapture)
import Hakyll.Core.Item (Item (..))
import Hakyll.Core.Metadata (Metadata, MonadMetadata (..))
import Hakyll.Core.Rules (Rules, create, rulesExtraDependencies)
import Hakyll.Flint.Property.Types
import Hakyll.Web.Html (toUrl)
import Hakyll.Web.Template.Context
import Text.Blaze.Html qualified as H
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 qualified as H5
import Text.Blaze.Html5.Attributes qualified as A

--------------------------------------------------------------------------------
-- Reading values

-- | Read a property's values out of one page's metadata. Absent or @null@
-- means no values.
readPropertyValues
    :: Property -> Identifier -> Metadata -> Either PropertyError [PropertyValue]
readPropertyValues prop ident meta =
    case KeyMap.lookup key meta of
        Nothing -> Right []
        Just raw -> do
            values <- filter (not . T.null . unPropertyValue) <$> fromJson raw
            forM_ values $ \v ->
                unless (validSlug v) $ Left (EmptySlug prop ident v)
            case (propertyCardinality prop, values) of
                (SingleValued, _ : _ : _) -> Left (TooManyValues prop ident values)
                _ -> Right values
  where
    key = Key.fromText . unPropertyName $ propertyName prop
    validSlug = not . T.null . unValueSlug . valueSlug

    fromJson :: Aeson.Value -> Either PropertyError [PropertyValue]
    fromJson = \case
        Aeson.Null -> Right []
        Aeson.String s -> Right $ case propertyCardinality prop of
            SingleValued -> [trimmed s]
            ManyValued -> map trimmed (T.splitOn "," s)
        Aeson.Array xs -> traverse scalar (toList xs)
        other -> (: []) <$> scalar other

    scalar :: Aeson.Value -> Either PropertyError PropertyValue
    scalar = \case
        Aeson.String s -> Right (trimmed s)
        Aeson.Number n -> Right . PropertyValue $
            either (T.pack . show @Double) (T.pack . show @Integer) (floatingOrInteger n)
        Aeson.Bool b -> Right . PropertyValue $ if b then "true" else "false"
        Aeson.Null -> Left (unreadable "null inside a list")
        Aeson.Array _ -> Left (unreadable "nested list")
        Aeson.Object _ -> Left (unreadable "a mapping is not a value")

    trimmed = PropertyValue . T.strip
    unreadable = UnreadableValue prop ident

-- | 'readPropertyValues' for a page, failing the build on malformed values.
getPropertyValues
    :: (MonadMetadata m, MonadFail m) => Property -> Identifier -> m [PropertyValue]
getPropertyValues prop ident =
    either (fail . renderPropertyError) pure . readPropertyValues prop ident
        =<< getMetadata ident

--------------------------------------------------------------------------------
-- Indexing

-- | Everything known about one value of a property.
data ValueEntry = ValueEntry
    { entryValue :: PropertyValue
    -- ^ Display form: the first spelling encountered (pages are visited in
    -- identifier order).
    , entrySlug :: ValueSlug
    , entryPage :: Identifier
    -- ^ The identifier of this value's own page (see 'valuePageRules').
    , entryMembers :: [Identifier]
    -- ^ Pages having this value, in identifier order.
    }
    deriving stock (Show)

-- | A property's values across a set of pages, in both directions.
data PropertyIndex = PropertyIndex
    { indexProperty :: Property
    , indexPattern :: Pattern
    -- ^ The pages that were scanned.
    , indexDependency :: Dependency
    -- ^ Depends on the metadata of every scanned page. Anything rendered
    -- from the index must be rebuilt when this changes; the fields in this
    -- module register it for you.
    , indexBySlug :: Map ValueSlug ValueEntry
    , indexByPage :: Map Identifier [PropertyValue]
    }

-- | Scan the front matter of every page matching the pattern.
--
-- The function names each value's own page; 'valuePagesAt' covers the usual
-- case.
buildPropertyIndex
    :: (MonadMetadata m, MonadFail m)
    => Property
    -> Pattern
    -- ^ Pages to scan.
    -> (ValueSlug -> Identifier)
    -- ^ Identifier of each value's own page.
    -> m PropertyIndex
buildPropertyIndex prop pages valuePage = do
    idents <- getMatches pages
    byPage <- fmap Map.fromList . forM idents $ \ident ->
        (ident,) <$> getPropertyValues prop ident
    let bySlug =
            Map.fromListWith
                (flip merge)
                [ (entrySlug entry, entry)
                | (ident, vs) <- Map.toAscList byPage
                , entry <- map (entryFor ident) vs
                ]
    pure
        PropertyIndex
            { indexProperty = prop
            , indexPattern = pages
            , indexDependency =
                metadataDependency . PatternDependency pages $ Set.fromList idents
            , indexBySlug = Map.map dedupe bySlug
            , indexByPage = byPage
            }
  where
    entryFor ident v = ValueEntry v slug (valuePage slug) [ident]
      where
        slug = valueSlug v

    merge older newer =
        older {entryMembers = entryMembers older <> entryMembers newer}

    -- A page listing both "Chicken" and "chicken" is still one member.
    dedupe e = e {entryMembers = nubSorted $ entryMembers e}

nubSorted :: (Ord a) => [a] -> [a]
nubSorted = Set.toAscList . Set.fromList

-- | Put value pages at a simple glob, filling the @*@ with the slug:
--
-- > valuePagesAt "ingredient/*.html"   -- ingredient/chicken-thigh.html
valuePagesAt :: Pattern -> ValueSlug -> Identifier
valuePagesAt pat = fromCapture pat . slugString

-- | Every value, ordered by slug.
indexEntries :: PropertyIndex -> [ValueEntry]
indexEntries = Map.elems . indexBySlug

-- | Values are matched by slug, so @lookupValue idx "Chicken"@ finds
-- @chicken@.
lookupValue :: PropertyIndex -> PropertyValue -> Maybe ValueEntry
lookupValue idx v = Map.lookup (valueSlug v) $ indexBySlug idx

pagesWithValue :: PropertyIndex -> PropertyValue -> [Identifier]
pagesWithValue idx = maybe [] entryMembers . lookupValue idx

-- | A page's values as written; empty for pages outside the index.
valuesOfPage :: PropertyIndex -> Identifier -> [PropertyValue]
valuesOfPage idx ident = Map.findWithDefault [] ident $ indexByPage idx

-- | How many pages have this value.
entryCount :: ValueEntry -> Int
entryCount = length . entryMembers

indexedName :: PropertyIndex -> PropertyName
indexedName = propertyName . indexProperty

--------------------------------------------------------------------------------
-- Value pages

-- | 'create' each value's page and run the given rules for it (typically a
-- 'Hakyll.Core.Rules.route' and a 'Hakyll.Core.Rules.compile'). The page is
-- rebuilt whenever any scanned page's metadata changes.
--
-- Inside, 'entryPagesField' lists the value's pages.
valuePageRules :: PropertyIndex -> (ValueEntry -> Rules ()) -> Rules ()
valuePageRules idx rules =
    forM_ (indexEntries idx) $ \entry ->
        rulesExtraDependencies [indexDependency idx] $
            create [entryPage entry] (rules entry)

--------------------------------------------------------------------------------
-- Contexts

-- | How to render a list of pages.
data PageListing = PageListing
    { listingContext :: Context ()
    -- ^ Fields available for each listed page.
    , listingOrder :: [Item ()] -> Compiler [Item ()]
    -- ^ Sort or filter, e.g. 'Hakyll.Web.Template.List.recentFirst'.
    }

-- | 'pageRefContext', in identifier order.
defaultListing :: PageListing
defaultListing = PageListing pageRefContext pure

-- | For a listed page: @$url$@, @$path$@, @$title$@ (falling back to the
-- file name) and every front-matter field. Deliberately no @$body$@: that
-- would need the page's compiled output.
pageRefContext :: Context ()
pageRefContext =
    urlField "url" <> pathField "path" <> metadataField <> titleField "title"

-- | Metadata-only items for the given pages.
pageRefs :: [Identifier] -> [Item ()]
pageRefs = map (`Item` ())

-- | The given pages, as the listing renders them.
listPages :: PageListing -> [Identifier] -> Compiler [Item ()]
listPages listing = listingOrder listing . pageRefs

-- | Fields for one value: @$value$@, @$slug$@, @$url$@ (of the value's page),
-- @$count$@ and the list @$pages$@.
valueEntryContext :: Context ValueEntry
valueEntryContext = valueEntryContextWith defaultListing

valueEntryContextWith :: PageListing -> Context ValueEntry
valueEntryContextWith listing =
    mconcat
        [ entryField "value" (pure . valueString . entryValue)
        , entryField "slug" (pure . slugString . entrySlug)
        , entryField "count" (pure . show . entryCount)
        , entryField "url" entryUrl
        , listFieldWith "pages" (listingContext listing) $
            listPages listing . entryMembers . itemBody
        ]
  where
    entryField name f = field name (f . itemBody)
    entryUrl = maybe (noResult "value page has no route") (pure . toUrl) <=< getRoute . entryPage

entryItem :: ValueEntry -> Item ValueEntry
entryItem e = Item (entryPage e) e

-- | Register that the current compilation reads the index.
dependOn :: PropertyIndex -> Compiler ()
dependOn idx = compilerTellDependencies [indexDependency idx]

-- | The current page's values as comma-separated HTML links to their value
-- pages, like Hakyll's @tagsField@. Values without a routed page are shown
-- as plain text.
propertyLinksField :: ContextKey -> PropertyIndex -> Context a
propertyLinksField (ContextKey key) idx = field key $ \item -> do
    let values = valuesOfPage idx (itemIdentifier item)
    links <- forM values $ \v -> do
        route <- maybe (pure Nothing) (getRoute . entryPage) (lookupValue idx v)
        pure (renderLink v route)
    if null links
        then noResult $ "page has no " <> show (indexedName idx)
        else pure . renderHtml . mconcat . intersperse ", " $ links
  where
    renderLink (PropertyValue v) = \case
        Nothing -> H.toHtml v
        Just path ->
            H5.a
                H.! A.href (H.toValue $ toUrl path)
                H.! A.rel "tag"
                $ H.toHtml v

-- | The current page's values as a list, each with 'valueEntryContext'.
-- Empty pages produce no field, so @$if(key)$@ works.
propertyValuesField :: ContextKey -> PropertyIndex -> Context a
propertyValuesField (ContextKey key) idx =
    Context $ \k _ item ->
        if k /= key
            then noResult ("not " <> key)
            else do
                let entries = mapMaybe (lookupValue idx) . valuesOfPage idx $ itemIdentifier item
                if null entries
                    then noResult "page has no values"
                    else pure . ListField valueEntryContext $ map entryItem entries

-- | Every value of the property (e.g. for an index or a tag cloud).
allValuesField :: ContextKey -> PropertyIndex -> Context a
allValuesField (ContextKey key) idx =
    listField key valueEntryContext $ do
        dependOn idx
        pure . map entryItem $ indexEntries idx

-- | The pages having a fixed value, from any page.
pagesWithValueField
    :: ContextKey -> PageListing -> PropertyIndex -> PropertyValue -> Context a
pagesWithValueField (ContextKey key) listing idx value =
    listField key (listingContext listing) $ do
        dependOn idx
        listPages listing $ pagesWithValue idx value

-- | Everything a value's own page needs, for use inside 'valuePageRules':
-- @$title$@ and @$value$@ (the value), @$slug$@, @$count$@, and the list
-- named by the key (its pages).
valuePageContext :: ContextKey -> PageListing -> ValueEntry -> Context a
valuePageContext pagesKey listing entry =
    mconcat
        [ constField "title" value
        , constField "value" value
        , constField "slug" . slugString $ entrySlug entry
        , constField "count" . show $ entryCount entry
        , entryPagesField pagesKey listing entry
        ]
  where
    value = valueString $ entryValue entry

-- | The pages of a value page's own value; use inside 'valuePageRules'.
entryPagesField :: ContextKey -> PageListing -> ValueEntry -> Context a
entryPagesField (ContextKey key) listing entry =
    listField key (listingContext listing) . listPages listing $ entryMembers entry

-- | A template-callable query. With key @pagesWhere@:
--
-- > $for(pagesWhere("ingredient", "chicken", "cuisine", "thai"))$
-- >   <a href="$url$">$title$</a>
-- > $endfor$
--
-- Arguments are property/value pairs; a page must match all of them. Each
-- property must be one of the given indexes, and only pages present in every
-- named index can match.
--
-- This also works from a page's own Markdown if you run it through
-- 'Hakyll.Web.Template.applyAsTemplate'.
propertyQueryField :: ContextKey -> PageListing -> [PropertyIndex] -> Context a
propertyQueryField (ContextKey key) listing indexes =
    Context $ \k args _ ->
        if k /= key
            then noResult ("not " <> key)
            else do
                conditions <- pairs args
                matches <- forM conditions $ \(name, value) -> do
                    idx <- maybe (unknown name) pure (findIndex name)
                    dependOn idx
                    pure . Set.fromList $ pagesWithValue idx value
                let hits = case matches of
                        [] -> []
                        m : ms -> Set.toAscList $ foldr Set.intersection m ms
                ListField (listingContext listing) <$> listPages listing hits
  where
    byName :: Map PropertyName PropertyIndex
    byName = Map.fromList [(indexedName i, i) | i <- indexes]

    findIndex name = Map.lookup name byName

    pairs :: [String] -> Compiler [(PropertyName, PropertyValue)]
    pairs = \case
        [] -> pure []
        n : v : rest -> (condition n v :) <$> pairs rest
        [_] -> fail $ key <> ": expected property/value pairs, got an odd number of arguments"

    condition n v = (PropertyName $ T.pack n, PropertyValue . T.strip $ T.pack v)

    unknown (PropertyName n) =
        fail $
            key
                <> ": unknown property "
                <> show n
                <> "; known: "
                <> show (map (unPropertyName . indexedName) indexes)

