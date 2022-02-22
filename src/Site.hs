module Site (site) where

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS
import Hakyll

import Site.Common
import Site.ConceptualFP
import Site.Project
import Site.Utopia

site :: Rules ()
site = do
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
compileIndex projs = match "pages/home.rst" $ do
  route $ constRoute "index.html"
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/index.html" (projectsContext <> defaultContext)
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls
  where
    projectsContext = listField "projects" projectContext projectItems

    projectContext = mconcat [projTitleField, projIdField, projBlurbField]
    projTitleField = field "project-title" (return . proj_title . itemBody)
    projIdField = field "project-id" (return . proj_id . itemBody)
    projBlurbField = field "blurb" (return . proj_blurb . itemBody)
    
    projectItems = return $ makeProjectItem <$> projs
    makeProjectItem = makeItemWith (\(ProjectMetadata _ pid _) -> fromFilePath $ "__page_" ++ pid)

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString >>= withItemBody callSass
  where
    callSass = unixFilter "sass" ["--stdin", "-s", "compressed", "-I", "./content/css"]

withSassIncludes :: Rules () -> Rules ()
withSassIncludes sassRules = do
  deps <- makePatternDependency "css/_*.scss"
  rulesExtraDependencies [deps] sassRules
