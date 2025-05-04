module Site (site) where

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS
import Hakyll

import Site.ConceptualFP
import Site.GeometricUniverse
import Site.Index

site :: Rules ()
site = do
  loadTemplates
  compileNotFound
  loadSiteConfig
  copyFiles
  compileSass

  sequence [conceptualFPRules, guRules] >>= compileIndex

loadTemplates, compileNotFound, loadSiteConfig, copyFiles, compileSass :: Rules ()

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

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody callSass
  where
    callSass = unixFilter "sass" ["--stdin", "-s", "compressed", "-I", "./content/css"]

withSassIncludes :: Rules () -> Rules ()
withSassIncludes sassRules = do
  deps <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [deps] sassRules
