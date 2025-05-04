{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Index (compileIndex) where

import Hakyll

import Site.Project
import Site.Common
import Site.Pandoc
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

-- writing projects come from the site code and are passed in here; programming projects are found
-- in the metadata for the home page.
compileIndex :: [ProjectMetadata] -> Rules ()
compileIndex writingProjects = match "pages/home.md" $ do
  route $ constRoute "index.html"
  compile $ dbg writingProjects >> compilePandocMarkdown' >>= compileHTMLPandoc >>= makeItem
    >>= loadAndApplyTemplate "templates/home.html" (programmingProjectsContext <> writingProjectsContext <> defaultContext)
    >>= loadAndApplyTemplate "templates/index.html" defaultContext
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls
  where
    writingProjectsContext = listField "writing-projects" projectContext writingProjectItems
    writingProjectItems = return $ makeProjectItem <$> writingProjects

instance FromJSON ProjectMetadata where
  parseJSON = withObject "ProjectMetadata" $ \v -> ProjectMetadata
    <$> v .: "github"
    <*> v .: "name"
    <*> v .: "blurb"

programmingProjectsContext :: Context String
programmingProjectsContext = listField "programming-projects" projectContext projectItems
  where
    projectItems = fmap makeProjectItem <$> projectsMetadata

    projectsMetadata = do
      allMetadata <- getMetadata "pages/home.md"
      case KM.lookup "programming-projects" allMetadata of
        Nothing -> dbg "programming-projects metadata field empty" >> return []
        Just v -> case fromJSON v of
          Error e -> dbg ("failed to parse programming-projects metadata: " ++ e) >> return []
          Success pms -> return pms
