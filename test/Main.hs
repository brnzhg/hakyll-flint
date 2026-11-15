module Main (main) where

import Control.Monad (forM_, void)
import Data.Aeson qualified as Aeson
import Data.List (isPrefixOf)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Hakyll
    ( Configuration (..)
    , defaultConfiguration
    , fromFilePath
    )
import Hakyll.Core.Logger qualified as Logger
import Hakyll.Core.Runtime (RunMode (..), run)
import Hakyll.Flint
import Site (siteRules)
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , listDirectory
    , doesDirectoryExist
    )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "hakyll-flint" [slugTests, readTests, targetTests, siteTests]

--------------------------------------------------------------------------------

slugTests :: TestTree
slugTests =
    testGroup
        "slugify"
        [ testCase "spaces and case" $ slugify "Chicken Thigh" @?= "chicken-thigh"
        , testCase "punctuation runs" $ slugify "  Thigh (bone-in)! " @?= "thigh-bone-in"
        , testCase "non-ASCII letters kept" $ slugify "Crème Fraîche" @?= "crème-fraîche"
        , testCase "nothing usable" $ slugify "--!" @?= ""
        ]

--------------------------------------------------------------------------------

readTests :: TestTree
readTests =
    testGroup
        "readPropertyValues"
        [ testCase "absent" $ many [] @?= Right []
        , testCase "null" $ many [("ingredient", Aeson.Null)] @?= Right []
        , testCase "YAML list" $
            many [("ingredient", list ["chicken", " leek "])] @?= Right ["chicken", "leek"]
        , testCase "comma string" $
            many [("ingredient", Aeson.String "chicken, leek,")] @?= Right ["chicken", "leek"]
        , testCase "numbers and bools" $
            many [("ingredient", Aeson.toJSON [Aeson.Number 4, Aeson.Number 1.5, Aeson.Bool True])]
                @?= Right ["4", "1.5", "true"]
        , testCase "single-valued keeps commas" $
            single [("cuisine", Aeson.String "Thai, Northern")] @?= Right ["Thai, Northern"]
        , testCase "single-valued rejects a list" $
            isLeft' (single [("cuisine", list ["Thai", "Lao"])])
        , testCase "single-valued accepts a one-element list" $
            single [("cuisine", list ["Thai"])] @?= Right ["Thai"]
        , testCase "mapping is unreadable" $
            isLeft' (many [("ingredient", Aeson.Object mempty)])
        , testCase "punctuation-only value is rejected" $
            isLeft' (many [("ingredient", list ["???"])])
        ]
  where
    ident = fromFilePath "recipes/x.md"
    many kvs = readPropertyValues (manyValued "ingredient") ident (KeyMap.fromList kvs)
    single kvs = readPropertyValues (singleValued "cuisine") ident (KeyMap.fromList kvs)
    list = Aeson.toJSON . map Aeson.String
    isLeft' = \case
        Left _ -> pure ()
        Right v -> assertFailure ("expected an error, got " <> show v)

--------------------------------------------------------------------------------

targetTests :: TestTree
targetTests =
    testGroup
        "parseLinkTarget"
        [ testCase "plain path" $
            parseLinkTarget "recipes/a.md" @?= Just (LinkTarget (fromFilePath "recipes/a.md") Nothing)
        , testCase "leading slash and anchor" $
            parseLinkTarget " /recipes/a.md#method "
                @?= Just (LinkTarget (fromFilePath "recipes/a.md") (Just (Anchor "method")))
        , testCase "empty" $ parseLinkTarget "#only-anchor" @?= Nothing
        ]

--------------------------------------------------------------------------------

-- | Build the example site in a scratch directory and inspect the output.
siteTests :: TestTree
siteTests =
    testCase "example site" $ withSystemTempDirectory "flint" $ \tmp -> do
        let src = tmp </> "site"
            out = tmp </> "_site"
            config =
                defaultConfiguration
                    { providerDirectory = src
                    , destinationDirectory = out
                    , storeDirectory = tmp </> "_cache"
                    , tmpDirectory = tmp </> "_cache/tmp"
                    }
            buildExpecting expected = do
                logger <- Logger.new Logger.Error
                (code, _) <- run RunModeNormal config logger siteRules
                code @?= expected
            build = buildExpecting ExitSuccess
            page p = readFile (out </> p)
            has p needles = do
                body <- page p
                forM_ needles $ \n ->
                    assertBool (p <> " should mention " <> show n) (n `T.isInfixOf` T.pack body)
            lacks p needles = do
                body <- page p
                forM_ needles $ \n ->
                    assertBool (p <> " should not mention " <> show n) (not (n `T.isInfixOf` T.pack body))

        copyTree "example/site" src
        build

        -- One page per value, slugged, listing its recipes.
        has "ingredient/chicken.html" ["<h1>Chicken</h1>", "Green Curry", "Roast Chicken", "Pad Krapow"]
        lacks "ingredient/chicken.html" ["Lentil Soup"]
        has "ingredient/lemon.html" ["Roast Chicken", "Lentil Soup"]
        exists (out </> "ingredient/thai-basil.html")
        has "cuisine/thai.html" ["Green Curry", "Pad Krapow"]
        lacks "cuisine/thai.html" ["Roast Chicken"]

        -- A page's own values as links.
        has "recipes/green-curry.html" ["href=\"/ingredient/coconut-milk.html\"", "href=\"/cuisine/thai.html\""]
        lacks "recipes/lentil-soup.html" ["Cuisine:"]

        -- A recipe listing chicken recipes includes itself: no cycle.
        has "recipes/green-curry.html" ["/recipes/green-curry.html", "/recipes/pad-krapow.html", "/recipes/roast-chicken.html"]

        -- AND query from Markdown, matching values by slug ("thai" vs "Thai").
        has "index.html" ["/recipes/green-curry.html", "/recipes/pad-krapow.html", "lentils"]
        lacks "index.html" ["/recipes/roast-chicken.html"]

        -- Wiki links: resolved to routes, untitled ones get the target's title,
        -- anchors kept, code left alone.
        has "recipes/green-curry.html"
            [ "<a href=\"/recipes/pad-krapow.html\" class=\"wikilink\">Pad Krapow</a>"
            , "<a href=\"/recipes/roast-chicken.html\" class=\"wikilink\">the roast</a>"
            , "<code>[[recipes/lentil-soup.md]]</code>"
            ]
        has "index.html" ["href=\"/recipes/lentil-soup.html#method\""]

        -- Backlinks, without self-links or links from code.
        has "recipes/pad-krapow.html" ["Linked from", "/recipes/green-curry.html"]
        has "recipes/lentil-soup.html" ["Linked from", "/index.html"]
        lacks "recipes/lentil-soup.html" ["Green Curry"]
        -- Green Curry links to it, but nothing else does yet.
        has "recipes/roast-chicken.html" ["Linked from"]
        lacks "recipes/roast-chicken.html" ["/recipes/lentil-soup.html"]

        -- Incremental rebuild: give lentil soup chicken and a cuisine; listings
        -- elsewhere must notice.
        writeFile (src </> "recipes/lentil-soup.md") $
            unlines
                [ "---"
                , "title: Lentil Soup"
                , "cuisine: Thai"
                , "ingredient: [lentils, lemon, chicken]"
                , "---"
                , "Now with chicken, like [[recipes/roast-chicken.md]]."
                ]
        build
        has "recipes/roast-chicken.html" ["Linked from", "/recipes/lentil-soup.html"]
        has "ingredient/chicken.html" ["Lentil Soup"]
        has "index.html" ["/recipes/lentil-soup.html"]
        has "recipes/green-curry.html" ["/recipes/lentil-soup.html"]

        -- A link to a page that doesn't exist fails the build.
        writeFile (src </> "recipes/broken.md") "---\ntitle: Broken\n---\nSee [[recipes/nope.md]].\n"
        buildExpecting (ExitFailure 1)
  where
    exists p = doesFileExist p >>= assertBool (p <> " should exist")

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
    createDirectoryIfMissing True to
    -- Skip build output (_site, _cache) from any manual run of the example.
    entries <- filter (not . isPrefixOf "_") <$> listDirectory from
    forM_ entries $ \e -> do
        isDir <- doesDirectoryExist (from </> e)
        if isDir
            then copyTree (from </> e) (to </> e)
            else void (copyFile (from </> e) (to </> e))
