{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Site.Common
    ( makeItemWith, ProjectMetadata(..), applyIndexTemplatesWith )
import Site.ConceptualFP (conceptualFPRules)
import Site.Utopia (utopiaRules)

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS
import Hakyll.Core.Configuration (Configuration(..), defaultConfiguration)
import Hakyll.Core.Compiler (Compiler, getResourceBody, getResourceLBS, getResourceString)
import Hakyll.Core.File (copyFileCompiler)
import Hakyll.Core.Identifier.Pattern (fromList)
import Hakyll.Core.Item (Item (itemBody), withItemBody)
import Hakyll.Core.Metadata (makePatternDependency)
import Hakyll.Core.Routes (idRoute, setExtension)
import Hakyll.Core.Rules (Rules, match, compile, route, rulesExtraDependencies)
import Hakyll.Core.UnixFilter (unixFilter)
import Hakyll.Main (hakyllWith)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)
import Hakyll.Web.Template (templateBodyCompiler, loadAndApplyTemplate, applyAsTemplate)
import Hakyll.Web.Template.Context (defaultContext, listField, field)
import Hakyll.Core.Identifier (fromFilePath)

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

  sequence [utopiaRules, conceptualFPRules] >>= compileIndex

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

compileIndex :: [ProjectMetadata] -> Rules ()
compileIndex projs = match "index.html" $ do
  route $ setExtension "html"
  compile $ getResourceBody >>= applyAsTemplate projectsContext >>= applyIndexTemplatesWith defaultContext
  where
    projectsContext = listField "projects" projectContext projectItems

    projectContext = mconcat [titleField, idField, blurbField]
      where
        titleField = field "title" (return . proj_title . itemBody)
        idField = field "project-id" (return . proj_id . itemBody)
        blurbField = field "blurb" (return . proj_blurb . itemBody)

    projectItems = return $ makeProjectItem <$> projs
    makeProjectItem = makeItemWith (\(ProjectMetadata _ pid _) -> fromFilePath $ "__proj_" ++ pid)

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody callSass
  where
    callSass = unixFilter "sass" ["--stdin", "-s", "compressed", "-I", "./content/css"]

withSassIncludes :: Rules () -> Rules ()
withSassIncludes sassRules = do
  deps <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [deps] sassRules
