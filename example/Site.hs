-- | Rules for the example recipe site. Shared by the @flint-example@
-- executable and the integration tests.
module Site (siteRules) where

import Hakyll
import Hakyll.Flint

ingredient, cuisine :: Property
ingredient = manyValued "ingredient"
cuisine = singleValued "cuisine"

recipes :: Pattern
recipes = "recipes/*.md"

-- | Pages whose [[wiki links]] count towards backlinks.
notes :: Pattern
notes = recipes .||. "index.md"

siteRules :: Rules ()
siteRules = do
    ingredients <- buildPropertyIndex ingredient recipes (valuePagesAt "ingredient/*.html")
    cuisines <- buildPropertyIndex cuisine recipes (valuePagesAt "cuisine/*.html")

    wikiLinkRules notes

    let pageCtx =
            propertyLinksField "ingredientLinks" ingredients
                <> propertyLinksField "cuisineLinks" cuisines
                <> propertyQueryField "pagesWhere" defaultListing [ingredients, cuisines]
                <> allValuesField "allIngredients" ingredients
                <> backlinksField "backlinks" defaultListing notes
                <> defaultContext

    match "templates/*" $ compile templateBodyCompiler

    -- Recipes may list other recipes (even themselves) by property.
    match recipes $ do
        route (setExtension "html")
        compile $
            getResourceBody
                >>= applyAsTemplate pageCtx
                >>= renderWikiPandoc FailOnMissing
                >>= loadAndApplyTemplate "templates/recipe.html" pageCtx
                >>= loadAndApplyTemplate "templates/default.html" pageCtx

    -- index.md queries the indexes from its own Markdown body.
    match "index.md" $ do
        route (setExtension "html")
        compile $
            getResourceBody
                >>= applyAsTemplate pageCtx
                >>= renderWikiPandoc FailOnMissing
                >>= loadAndApplyTemplate "templates/default.html" pageCtx

    let valuePages idx = valuePageRules idx $ \entry -> do
            route idRoute
            compile $ do
                let ctx = valuePageContext "pages" defaultListing entry <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/value.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
    valuePages ingredients
    valuePages cuisines
