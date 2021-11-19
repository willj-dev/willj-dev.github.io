{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Site.Common where

import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Identifier (toFilePath, Identifier, fromFilePath)
import Hakyll.Core.Identifier.Pattern (fromList)
import Hakyll.Core.Item (Item (Item))
import Hakyll.Core.Metadata (getMetadataField')
import Hakyll.Core.Routes (Routes, customRoute, setExtension)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)
import Hakyll.Web.Template.Context (Context, defaultContext)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Core.Rules (Rules, match)
import System.FilePath (takeFileName, splitPath)
import System.FilePath.Posix (joinPath) -- always use '/' to make site paths

applyIndexTemplates :: Item String -> Compiler (Item String)
applyIndexTemplates = applyIndexTemplatesWith defaultContext

applyIndexTemplatesWith :: Context String -> Item String -> Compiler (Item String)
applyIndexTemplatesWith indexContext indexHTML =
  loadAndApplyTemplate "templates/index.html" indexContext indexHTML
  >>= loadAndApplyTemplate "templates/base.html" defaultContext
  >>= relativizeUrls

filenameOnlyRoute, htmlExtensionRoute, tailRoute :: Routes
filenameOnlyRoute = customRoute (takeFileName . toFilePath)
htmlExtensionRoute = setExtension "html"
tailRoute = customRoute (fpTail . toFilePath)
  where
    fpTail = joinPath . tail . splitPath

makeItemWith :: (a -> Identifier) -> a -> Item a
makeItemWith makeId body = Item (makeId body) body

makeSubItemWith :: (a -> String) -> (a -> b) -> Item a -> Item b
makeSubItemWith mapIdSuffix mapBody (Item parentId parentBody) = Item newId newBody
  where
    newId = addIdSuffix parentId (mapIdSuffix parentBody)
    newBody = mapBody parentBody

addIdSuffix :: Identifier -> String -> Identifier
addIdSuffix parentId suffix = fromFilePath (toFilePath parentId ++ "_" ++ suffix)

data ProjectMetadata = ProjectMetadata
  { proj_title  :: String
  , proj_id     :: String
  , proj_blurb  :: String
  }

projectMetadata :: Identifier -> Rules ProjectMetadata
projectMetadata projectIndex = do
  title     <- getMetadataField' projectIndex "title"
  projectId <- getMetadataField' projectIndex "project-id"
  blurb     <- getMetadataField' projectIndex "blurb"
  return (ProjectMetadata title projectId blurb)

matchOnly :: Identifier -> Rules () -> Rules ()
matchOnly ident = match (fromList [ident])
