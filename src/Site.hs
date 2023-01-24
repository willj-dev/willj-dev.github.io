{-# LANGUAGE OverloadedStrings #-}

import Hakyll

import Site.Pandoc (pageCompiler)

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeFileName, splitDirectories)
import System.FilePath.Posix (joinPath) -- always use '/' to make site paths
import Data.Foldable (foldl')
import Site.Config (makeItemWith)

hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration
    { destinationDirectory = "docs",
      providerDirectory = "content"
    }

main :: IO ()
main = hakyllWith hakyllConfig $ do
  loadTemplates
  compileNotFound
  loadSiteConfig
  copyFiles
  compileSass
  compileIndex

loadTemplates, compileNotFound, loadSiteConfig, copyFiles, compileSass, compileIndex :: Rules ()

loadTemplates = match "templates/*" $ compile templateBodyCompiler

compileNotFound = match "404.html" $ do
  route idRoute
  compile $
    getResourceBody
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= loadAndApplyTemplate "templates/404.html" defaultContext
      >>= relativizeUrls

loadSiteConfig = match "config.yaml" $ compile getResourceStrictByteString
  where
    bsLazyToStrict = SBS.pack . LBS.unpack
    getResourceStrictByteString = fmap bsLazyToStrict <$> getResourceLBS

copyFiles = match copiedFiles $ route idRoute >> compile copyFileCompiler
  where
    copiedFiles = fromList ["CNAME", "js/colorMode.js"]

compileSass = match "css/main.scss" $
  withSassIncludes $ do
    route $ setExtension "css"
    compile sassCompiler

compileIndex = match "index.rst" $ do
  route $ setExtension "html"
  compile $
    pageCompiler
      >>= loadAndApplyTemplate "templates/index.html" defaultContext
      >>= loadAndApplyTemplate "templates/base.html" (pageDirsContext <> defaultContext)
      >>= relativizeUrls

filenameOnlyRoute, htmlExtensionRoute :: Routes
filenameOnlyRoute = customRoute (takeFileName . toFilePath)
htmlExtensionRoute = setExtension "html"

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody callSass
  where
    callSass = unixFilter "sass" ["--stdin", "-s", "compressed", "-I", "./content/css"]

withSassIncludes :: Rules () -> Rules ()
withSassIncludes sassRules = do
  deps <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [deps] sassRules

pageDirsContext :: forall a. Context a
pageDirsContext = listField "page-dirs" pageDirContext pageDirsCompiler
  where
    pageDirContext = mconcat [cumulativeDirField, currentDirField]
    cumulativeDirField = field "cumulative-dir" (return . fst . itemBody)
    currentDirField = field "current-dir" (return . snd . itemBody)

    pageDirsCompiler = foldPageDirs . toFilePath <$> getUnderlying
    foldPageDirs itemPath = foldl' foldFn [] (init $ splitDirectories itemPath)

    foldFn :: [Item (String, String)] -> FilePath -> [Item (String, String)]
    foldFn [] rootDir = [makeItemWith pageDirIdentifier (rootDir, rootDir)]
    foldFn dirs nextDir = dirs ++ [
      makeItemWith pageDirIdentifier (joinPath [fst . itemBody $ last dirs, nextDir], nextDir)
      ]

    pageDirIdentifier (cd, _) = fromFilePath $ "__page_dir_" ++ cd

