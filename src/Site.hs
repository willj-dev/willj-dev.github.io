{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Site.Pandoc (pageCompiler)

import System.FilePath (takeFileName)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as SBS


hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { destinationDirectory = "docs"
    , providerDirectory = "content"
    }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    loadTemplates
    loadSiteConfig
    copyFiles
    compileSass
    compileIndex

loadTemplates, loadSiteConfig, copyFiles, compileSass, compileIndex :: Rules ()

loadTemplates = match "templates/*" $ compile templateBodyCompiler

loadSiteConfig = match "config.yaml" $ compile getResourceStrictByteString
    where
        bsLazyToStrict = SBS.pack . LBS.unpack
        getResourceStrictByteString = fmap bsLazyToStrict <$> getResourceLBS

copyFiles = match copiedFiles $ route idRoute >> compile copyFileCompiler
    where
        copiedFiles = fromList ["CNAME"]

compileSass = match "css/main.scss" $ withSassIncludes $ do
    route $ setExtension "css"
    compile sassCompiler

compileIndex = match "index.rst" $ do
    route $ setExtension "html"
    compile $ pageCompiler
        >>= loadAndApplyTemplate "templates/index.html" defaultContext
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
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
