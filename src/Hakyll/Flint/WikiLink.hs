-- | Obsidian-style @[[wiki links]]@ and backlinks.
--
-- A link names its target by path, relative to the site's source directory
-- (Hakyll's 'Identifier'), so there is never any ambiguity:
--
-- > [[recipes/green-curry.md]]                 -- text: the target's title
-- > [[recipes/green-curry.md|that curry]]      -- text: "that curry"
-- > [[recipes/green-curry.md#method]]          -- links to a heading
--
-- = How backlinks avoid cycles
--
-- 'wikiLinkRules' compiles every page a second time, under the version
-- 'linkVersion', into just the list of pages it links to. A page's backlinks
-- are found by loading /those/ items, never the other pages' real output.
-- Link-extraction never loads anything, so there is no cycle, and Hakyll's
-- ordinary dependency tracking rebuilds backlinks when any page changes.
module Hakyll.Flint.WikiLink
    ( -- * Link targets
      LinkTarget (..)
    , Anchor (..)
    , parseLinkTarget

      -- * Reading and rendering
    , wikiLinkReaderOptions
    , outgoingLinks
    , MissingTarget (..)
    , resolveWikiLinks
    , renderWikiPandoc
    , wikiPandocCompiler

      -- * Backlinks
    , linkVersion
    , wikiLinkRules
    , LinkGraph
    , loadLinkGraph
    , linksFrom
    , linksTo
    , backlinksField
    , outlinksField
    ) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hakyll.Core.Compiler
    ( Compiler
    , getResourceBody
    , getRoute
    , getUnderlying
    , loadAll
    , makeItem
    , noResult
    )
import Hakyll.Core.Identifier
    ( Identifier
    , fromFilePath
    , setVersion
    , toFilePath
    )
import Hakyll.Core.Identifier.Pattern (Pattern, hasVersion, (.&&.))
import Hakyll.Core.Item (Item (..))
import Hakyll.Core.Metadata (getMetadataField)
import Hakyll.Core.Rules (Rules, compile, match, version)
import Hakyll.Core.Writable (Writable (..))
import Hakyll.Flint.Property (PageListing (..), pageRefs)
import Hakyll.Flint.Property.Types (ContextKey (..))
import Hakyll.Web.Html (toUrl)
import Hakyll.Web.Pandoc
    ( defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    , readPandocWith
    , renderPandocWithTransformM
    )
import Hakyll.Web.Template.Context (Context (..), ContextField (ListField))
import Text.Pandoc.Definition (Inline (..), Pandoc)
import Text.Pandoc.Options
    ( Extension (Ext_wikilinks_title_after_pipe)
    , ReaderOptions (..)
    , enableExtension
    )
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query, walkM)

--------------------------------------------------------------------------------
-- Link targets

-- | A heading id within a page, the part after @#@.
newtype Anchor = Anchor {unAnchor :: Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

-- | Where a wiki link points.
data LinkTarget = LinkTarget
    { targetPage :: Identifier
    , targetAnchor :: Maybe Anchor
    }
    deriving stock (Show, Eq, Ord)

-- | Parse the inside of @[[...]]@ (without any @|alias@). A leading @/@ is
-- allowed and ignored. 'Nothing' for an empty path.
parseLinkTarget :: Text -> Maybe LinkTarget
parseLinkTarget raw
    | T.null path = Nothing
    | otherwise = Just $ LinkTarget (fromFilePath $ T.unpack path) anchor
  where
    (pathPart, anchorPart) = T.break (== '#') (T.strip raw)
    path = T.dropWhile (== '/') pathPart
    anchor = case T.drop 1 anchorPart of
        "" -> Nothing
        a -> Just (Anchor a)

--------------------------------------------------------------------------------
-- Reading and rendering

-- | Turn on pandoc's @wikilinks_title_after_pipe@ extension, so
-- @[[target|alias]]@ parses as a link.
wikiLinkReaderOptions :: ReaderOptions -> ReaderOptions
wikiLinkReaderOptions ropts =
    ropts
        { readerExtensions =
            enableExtension Ext_wikilinks_title_after_pipe (readerExtensions ropts)
        }

isWikiLink :: (Text, [Text], [(Text, Text)]) -> Bool
isWikiLink (_, classes, _) = "wikilink" `elem` classes

-- | Every wiki link in a document, in order (duplicates kept). Links in code
-- are not links, and are not found. Embeds (@![[...]]@) are ignored.
outgoingLinks :: Pandoc -> [LinkTarget]
outgoingLinks = mapMaybe parseLinkTarget . query collect
  where
    collect = \case
        Link attr _ (url, _) | isWikiLink attr -> [url]
        _ -> []

-- | What to do with a link to a page that has no route.
data MissingTarget
    = -- | Fail the build, naming the link.
      FailOnMissing
    | -- | Keep the text, as @\<span class="wikilink missing"\>@.
      RenderMissingAsText
    deriving stock (Show, Eq)

-- | Point every wiki link at its target's URL. Links without an alias get the
-- target's @title@ metadata (or its path) as their text.
resolveWikiLinks :: MissingTarget -> Pandoc -> Compiler Pandoc
resolveWikiLinks onMissing = walkM resolve
  where
    resolve :: Inline -> Compiler Inline
    resolve = \case
        Link attr inlines (url, title)
            | isWikiLink attr
            , Just target <- parseLinkTarget url ->
                resolveOne attr inlines url title target
        other -> pure other

    resolveOne attr inlines url title target = do
        route <- getRoute (targetPage target)
        -- pandoc copies the target into the text when there is no alias.
        text <-
            if stringify inlines == url
                then pure . Str . T.pack <$> titleOf (targetPage target)
                else pure inlines
        case route of
            Just path -> pure $ Link attr text (href path, title)
            Nothing -> case onMissing of
                RenderMissingAsText -> pure $ Span missingAttr text
                FailOnMissing -> do
                    here <- getUnderlying
                    fail $
                        "hakyll-flint: "
                            <> toFilePath here
                            <> " links to [["
                            <> T.unpack url
                            <> "]], which has no route"
      where
        missingAttr = ("", ["wikilink", "missing"], [])
        href path = T.pack (toUrl path) <> anchorSuffix
        anchorSuffix = maybe "" (("#" <>) . unAnchor) $ targetAnchor target

    titleOf ident =
        fromMaybe (toFilePath ident) <$> getMetadataField ident "title"

-- | 'Hakyll.Web.Pandoc.renderPandoc' with wiki links enabled and resolved.
renderWikiPandoc :: MissingTarget -> Item String -> Compiler (Item String)
renderWikiPandoc onMissing =
    renderPandocWithTransformM
        (wikiLinkReaderOptions defaultHakyllReaderOptions)
        defaultHakyllWriterOptions
        (resolveWikiLinks onMissing)

-- | 'Hakyll.Web.Pandoc.pandocCompiler' with wiki links enabled and resolved.
wikiPandocCompiler :: MissingTarget -> Compiler (Item String)
wikiPandocCompiler onMissing = getResourceBody >>= renderWikiPandoc onMissing

--------------------------------------------------------------------------------
-- Backlinks

-- | The version under which 'wikiLinkRules' stores each page's outgoing
-- links. Don't give it a route.
linkVersion :: String
linkVersion = "flint-links"

-- | For every page matching the pattern, record its outgoing links (under
-- 'linkVersion'). Call once, for the same pattern you later pass to
-- 'loadLinkGraph' or 'backlinksField'.
wikiLinkRules :: Pattern -> Rules ()
wikiLinkRules pages =
    match pages . version linkVersion . compile $ do
        doc <- getResourceBody >>= readPandocWith (wikiLinkReaderOptions defaultHakyllReaderOptions)
        makeItem . OutgoingLinks . map targetPage . outgoingLinks $ itemBody doc

-- | What 'wikiLinkRules' stores for each page: the pages it links to. It has
-- no route and is never written out.
newtype OutgoingLinks = OutgoingLinks [Identifier]
    deriving newtype (Binary)

instance Writable OutgoingLinks where
    write _ _ = pure ()

-- | Who links to whom, among a set of pages.
data LinkGraph = LinkGraph
    { graphOut :: Map Identifier [Identifier]
    , graphIn :: Map Identifier [Identifier]
    }

-- | Load the links recorded by 'wikiLinkRules' for these pages.
loadLinkGraph :: Pattern -> Compiler LinkGraph
loadLinkGraph pages = do
    items <- loadAll (pages .&&. hasVersion linkVersion)
    let edges =
            [ (setVersion Nothing ident, targets)
            | Item ident (OutgoingLinks targets) <- items
            ]
    let out = Map.fromList [(from, dedupe tos) | (from, tos) <- edges]
        into =
            Map.map dedupe $
                Map.fromListWith
                    (<>)
                    [(to, [from]) | (from, tos) <- edges, to <- tos, to /= from]
    pure (LinkGraph out into)
  where
    dedupe = Set.toAscList . Set.fromList

-- | Pages this page links to (only pages within the graph's pattern are
-- looked at as sources; targets can be anything).
linksFrom :: LinkGraph -> Identifier -> [Identifier]
linksFrom g ident = Map.findWithDefault [] ident (graphOut g)

-- | Pages linking to this page, excluding itself.
linksTo :: LinkGraph -> Identifier -> [Identifier]
linksTo g ident = Map.findWithDefault [] ident (graphIn g)

-- | The pages (within the pattern) that link to the current page. No links
-- means no field, so @$if(key)$@ works.
backlinksField :: ContextKey -> PageListing -> Pattern -> Context a
backlinksField = graphField linksTo

-- | The pages the current page links to.
outlinksField :: ContextKey -> PageListing -> Pattern -> Context a
outlinksField = graphField linksFrom

graphField
    :: (LinkGraph -> Identifier -> [Identifier])
    -> ContextKey
    -> PageListing
    -> Pattern
    -> Context a
graphField select (ContextKey key) listing pages =
    Context $ \k _ item ->
        if k /= key
            then noResult ("not " <> key)
            else do
                graph <- loadLinkGraph pages
                case select graph . setVersion Nothing $ itemIdentifier item of
                    [] -> noResult "no links"
                    idents ->
                        ListField (listingContext listing)
                            <$> listingOrder listing (pageRefs idents)
