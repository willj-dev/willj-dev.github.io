{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Site.Common where

import Hakyll.Core.Compiler (Compiler, getUnderlying)
import Hakyll.Core.Identifier (toFilePath, Identifier, fromFilePath)
import Hakyll.Core.Identifier.Pattern (fromList, fromGlob, complement, (.&&.))
import Hakyll.Core.Item (Item (Item))
import Hakyll.Core.Metadata (getMetadataField')
import Hakyll.Core.Routes (Routes, customRoute, setExtension, composeRoutes)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)
import Hakyll.Web.Template.Context (Context, defaultContext, field)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Core.Rules (Rules, match)
import System.FilePath (takeFileName, splitPath, splitDirectories)
import System.FilePath.Posix (joinPath) -- always use '/' to make site paths

projectIdContext, headerTitleContext :: Context String
projectIdContext = field "project-id" (const $ getUnderlying >>= projectIdFromIdentifier)
headerTitleContext = field "header-title" (const $ getUnderlying >>= (`getMetadataField'` "title"))

applyIndexTemplates :: Bool -> Item String -> Compiler (Item String)
applyIndexTemplates = applyIndexTemplatesWith defaultContext

applyIndexTemplatesWith :: Context String -> Bool -> Item String -> Compiler (Item String)
applyIndexTemplatesWith indexContext isProjectIndex indexHTML =
  loadAndApplyTemplate "templates/index.html" indexContext indexHTML
  >>= loadAndApplyTemplate "templates/base.html" baseContext
  >>= relativizeUrls
  where
    baseContext = if isProjectIndex
      then projectIdContext <> defaultContext
      else defaultContext

filenameOnlyRoute, htmlExtensionRoute, tailRoute, tailHTMLRoute :: Routes
filenameOnlyRoute = customRoute (takeFileName . toFilePath)
htmlExtensionRoute = setExtension "html"
tailRoute = customRoute (joinPath . tail . splitPath . toFilePath)
tailHTMLRoute = composeRoutes tailRoute htmlExtensionRoute

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
  projectId <- projectIdFromIdentifier projectIndex
  blurb     <- getMetadataField' projectIndex "blurb"
  return (ProjectMetadata title projectId blurb)

matchOnly :: Identifier -> Rules () -> Rules ()
matchOnly ident = match (fromList [ident])

matchGlobExceptIndex :: String -> Rules () -> Rules ()
matchGlobExceptIndex globString = match $ fromGlob globString .&&. complement (fromGlob "**/index.*")

-- 'blah/project-id/page.foo' -> 'project-id'
projectIdFromIdentifier :: MonadFail m => Identifier -> m String
projectIdFromIdentifier itemId = case splitDirectories (toFilePath itemId) of
    [_, projectId, _] -> return projectId
    _                 -> fail $ "could not determine project id for " ++ show itemId
