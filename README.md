# hakyll-flint

Obsidian-flavoured extensions for [Hakyll](https://jaspervdj.be/hakyll/).

- **Custom properties** (done): declare any front-matter field, such as
  `ingredient:`, as a property. Index it, give each value its own page, and
  list the pages with a given value from any page.
- **Wiki links and backlinks**: `[[recipes/green-curry.md]]` links, plus
  each page's backlinks. A link names its target by path, so links always
  resolve unambiguously.
- **Bases-style views** (planned): queries over properties. Basic AND queries
  already work through `propertyQueryField`.

## Development

The toolchain is whatever ghcup marks as *recommended*. The compiler is pinned
in `cabal.project`.

| tool  | version  |
|-------|----------|
| GHC   | 9.10.3   |
| cabal | 3.16.1.0 |
| HLS   | 2.14.0.0 |

```sh
ghcup install ghc 9.10.3 && ghcup install cabal recommended && ghcup install hls recommended
cabal build all
cabal test
cd example/site && cabal run flint-example -- watch   # http://localhost:8000
```

The code is formatted with fourmolu (`fourmolu.yaml`), which HLS uses
automatically.

### Style

Hakyll passes `String`s for nearly everything. This project uses small
newtypes instead, so a signature tells you what each argument is:
`PropertyName`, `PropertyValue`, `ValueSlug` and `ContextKey` (the name a
template refers to). All of them are `IsString`, so call sites still use
plain literals.

## Custom properties

```haskell
import Hakyll
import Hakyll.Flint

ingredient, cuisine :: Property
ingredient = manyValued "ingredient"   -- [a, b] or "a, b"
cuisine    = singleValued "cuisine"    -- a list is a build error

main = hakyll $ do
  ingredients <- buildPropertyIndex ingredient "recipes/*.md" (valuePagesAt "ingredient/*.html")

  -- ingredient/chicken.html, ingredient/coconut-milk.html, ...
  valuePageRules ingredients $ \entry -> do
    route idRoute
    compile $ makeItem "" >>= loadAndApplyTemplate "templates/value.html"
      (valuePageContext "pages" defaultListing entry <> defaultContext)

  let ctx = propertyLinksField "ingredientLinks" ingredients             -- like tagsField
         <> propertyQueryField "pagesWhere" defaultListing [ingredients] -- callable from templates
         <> defaultContext
  ...
```

You can call a query from a template, or from a Markdown body that goes
through `applyAsTemplate`:

```
$for(pagesWhere("ingredient", "chicken", "cuisine", "thai"))$
- [$title$]($url$)
$endfor$
```

| field | gives |
|---|---|
| `propertyLinksField` | the current page's values as HTML links to their value pages |
| `propertyValuesField` | the current page's values as a list (`$value$ $slug$ $url$ $count$ $pages$`) |
| `allValuesField` | every value (for an index page or a tag cloud) |
| `pagesWithValueField` | the pages with one fixed value |
| `valuePageContext` | a value page's `$title$ $value$ $slug$ $count$` and its pages |
| `propertyQueryField` | a function field that ANDs property/value pairs |

Values are matched by slug, so `Chicken` and `chicken` count as the same
value. The slug is also the file name of the value's page. A `PageListing`
controls what each listed page exposes and how the list is ordered (for
example `PageListing pageRefContext recentFirst`).

### Avoiding dependency cycles

The index is built from front matter while the `Rules` run. Listed pages are
`Item ()`s that only read their metadata and route, and are never `load`ed.
So any page can list any set of pages, including itself, and Hakyll sees no
cycle. Each field registers a metadata dependency on the indexed pages. If
you add or edit a page, the listings that mention it are rebuilt on the next
incremental build. The integration test checks this.

If you need the listed pages' compiled bodies (for teasers), compile them a
second time under a version and load that version:

```haskell
match "recipes/*.md" $ version "teaser" $ compile $
  pandocCompiler >>= saveSnapshot "teaser"

-- inside a listing:
loadAllSnapshots (fromList (map (setVersion (Just "teaser")) ids)) "teaser"
```

The teaser version doesn't run property queries, so loading it never forms a
cycle. This is the approach from
[hakyll#590](https://github.com/jaspervdj/hakyll/issues/590).

## Wiki links and backlinks

Link targets are paths relative to the site's source directory, the same
form as a Hakyll `Identifier`:

```
[[recipes/green-curry.md]]              text: the target's title
[[recipes/green-curry.md|that curry]]   text: "that curry"
[[/recipes/green-curry.md#method]]      leading / is fine; #anchor is kept
```

```haskell
let notes = "recipes/*.md" .||. "index.md"
wikiLinkRules notes      -- records each page's outgoing links (see below)

match notes $ do
  route (setExtension "html")
  compile $ wikiPandocCompiler FailOnMissing   -- or RenderMissingAsText
    >>= loadAndApplyTemplate "templates/page.html"
          (backlinksField "backlinks" defaultListing notes <> defaultContext)
```

Links are found by pandoc's own `wikilinks_title_after_pipe` reader
extension, so `[[...]]` inside code isn't treated as a link. If you already
run bodies through `applyAsTemplate`, use `renderWikiPandoc` in place of
`renderPandoc`. `outlinksField` lists the pages the current page links to.

**Avoiding cycles.** `wikiLinkRules` compiles every page a second time, under
the version `"flint-links"`, into just the list of paths it links to.
`backlinksField` loads those small items and never the other pages' real
output. Extracting links never loads anything, so there is no cycle, and
Hakyll's normal dependency tracking rebuilds backlinks when a page changes.
