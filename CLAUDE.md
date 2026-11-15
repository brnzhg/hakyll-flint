# hakyll-flint

A Haskell library extending Hakyll: custom metadata properties
(`Hakyll.Flint.Property`) and path-based wiki links with backlinks
(`Hakyll.Flint.WikiLink`). The README is the user-facing documentation.

## Commands

```sh
cabal build all
cabal test                                   # unit tests + builds example/site in a temp dir
cd example/site && cabal run flint-example -- watch   # must run from example/site
```

The toolchain is ghcup's recommended GHC 9.10.3, cabal 3.16 and HLS 2.14.
The compiler is pinned in `cabal.project`. Format with fourmolu
(`fourmolu.yaml`).

## Conventions

- Don't pass bare `String`s whose meaning isn't clear from the type. Use
  newtypes like `PropertyName`, `PropertyValue`, `ValueSlug`, `ContextKey`,
  `LinkTarget` and `Anchor`, and add new ones in the same style. Make a
  newtype `IsString` when callers write literals.
- Code must compile without warnings under the flags in the cabal `common`
  stanza.
- `example/Site.hs` is shared by the example executable and the
  integration test. When adding a feature, add it to the example site and
  assert on the generated HTML in `test/Main.hs`.

## Design constraint: no dependency cycles

Anything that lists other pages must not `load` their compiled output.

- Property listings use the index built from front matter during `Rules`.
  They render `Item ()`s that read only the listed pages' metadata and
  routes.
- Backlinks load the small `"flint-links"` versions that `wikiLinkRules`
  produces, never the pages themselves.
- Fields that read an index must register its dependency (`dependOn`), or
  incremental builds go stale. The rebuild assertions in the test cover
  this.

## Gotchas

- `cabal.project` pins `setup.Cabal`. Without it, `cabal build` and HLS
  resolve different Cabal versions for setup scripts and each compiles
  pandoc separately, which takes about 15 minutes.
- Executables need `-threaded`, or Hakyll's `watch` crashes.
- HLS's first load of the project compiles pandoc. Let it finish instead of
  restarting it.
