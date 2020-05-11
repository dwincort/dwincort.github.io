{-# LANGUAGE OverloadedStrings #-}

import           Data.List (sortBy, isSuffixOf)
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import           Hakyll
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do
    match ("robots.txt"
      .||. "images/*"
      .||. "data/*"
      .||. "CNAME") $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "catcam.html" $ do
      route cleanRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/encrypted.html" defaultContext'
        >>= relativizeUrls

    match (fromList ["index.md", "404.md"]) $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= cleanIndexUrls

    match topLevelPages $ do
      route cleanRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= cleanIndexUrls

    match "research.md" $ do
      route cleanRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/bib.html" defaultContext'
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= cleanIndexUrls

    match "posts/*" $ do
      route cleanRoute -- $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

    create ["archive.html"] $ do
      route cleanRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          archiveCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Archives"            <>
            defaultContext'

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        singlePages <- loadAll (topLevelPages .||. "research.md")
        let pages = posts <> singlePages
            sitemapCtx =
              constField "root" root <>
              listField "pages" postCtx (return pages)
        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
          >>= cleanIndexUrls


    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) <>
    --                 defaultContext'
    --
    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
topLevelPages :: Pattern
topLevelPages = fromList
  [ "projects.md"
  , "teaching.md"
  , "personal.md"
  , "contact.md"]

root :: String
root = "https://danwc.com"

defaultContext' :: Context String
defaultContext' = constField "root" root <> defaultContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext'
  -- dateField "date" "%Y-%m-%d" <>


--------------------------------------------------------------------------------
-- from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url
