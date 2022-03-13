
-- | Functions and types about `Project`s, which are collections of `Page`s with independent internal links and their own index
module Site.Project where

import Hakyll

import Site.Common
import Site.Page

data ProjectMetadata = ProjectMetadata
  { proj_id     :: ProjectId
  , proj_title  :: String
  , proj_blurb  :: String
  } deriving (Show)

-- | The project id for the file we're currently working on
currentProjectId :: Compiler ProjectId
currentProjectId = getUnderlying >>= projectId

-- | A template context for the project-id of the file currently being compiled
projectIdContext :: Context a
projectIdContext = field "project-id" (const currentProjectId)

-- | A matcher for 'pages/project-id/index.*'
matchProjectIndex :: ProjectId -> Rules () -> Rules ()
matchProjectIndex pid = match (fromGlob $ "pages/" ++ pid ++ "/index.*")

-- | A matcher for everything in 'pages/project-id/', except for the index
matchProjectPages :: ProjectId -> Rules () -> Rules ()
matchProjectPages pid = match (projectPagesGlob .&&. notProjectIndex)
  where
    projectPagesGlob = fromGlob $ "pages/" ++ pid ++ "/*"
    notProjectIndex = complement . fromGlob $ "pages/" ++ pid ++ "/index.*"

-- | Apply templates for a project's index page
applyProjectIndexTemplates :: [PageId] -> String -> Item String -> Compiler (Item String)
applyProjectIndexTemplates pgIds ext pageBodyHTMLItem = do
  pcc <- projectContentsContext <$> currentProjectId
  loadAndApplyTemplate "templates/index.html" (pcc <> defaultContext) pageBodyHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
  where
    projectContentsContext pid = listField "project-contents" pageMetadataContext $
      sequence (makePageMetadata . pageIdentifier ext pid <$> pgIds)
    baseContext  = projectIdContext <> defaultContext

projectMetadata :: (MonadFail m, MonadMetadata m) => ProjectId -> String -> m ProjectMetadata
projectMetadata pid ext = do
  title <- getMetadataField' projIndexIdentifier "title"
  blurb <- getMetadataField' projIndexIdentifier "blurb"
  return $ ProjectMetadata pid title blurb
  where
    projIndexIdentifier = pageIdentifier ext pid "index"
