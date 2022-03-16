
-- | Functions and types about `Page`s, which have links to and from other `Page`s in the same `Project`
--   Something is a `Page` iff its Identifier looks like `pages/project-id/page-name.md`

module Site.Page where

import Hakyll
import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine

import Site.Common hiding (CompiledPage(..))

pageIdentifier :: ProjectId -> PageId -> Identifier
pageIdentifier projId pgId = fromFilePath $ FPL.joinPath ["pages", projId, pgId ++ ".md"]

pageId :: Identifier -> PageId
pageId = FPL.takeBaseName . toFilePath

data PageMetadata = PageMetadata
  { page_id     :: PageId
  , page_title  :: String
  , page_blurb  :: Maybe String
  }

makePageMetadata :: (MonadFail m, MonadMetadata m) => Identifier -> m (Item PageMetadata)
makePageMetadata pg = do
  title     <- getMetadataField' pg "title"
  blurb     <- getMetadataField pg "blurb"
  return $ Item (addIdSuffix pg "_md") (PageMetadata (pageId pg) title blurb)

pageMetadataContext :: Context PageMetadata
pageMetadataContext =
  field "page-id" (return . page_id . itemBody)
  <> field "page-title" (return . page_title . itemBody)
  <> maybeField "blurb" (return . page_blurb . itemBody)
