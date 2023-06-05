
-- | Functions and types about `Page`s, which have links to and from other `Page`s in the same `Project`
--   Something is a `Page` iff its Identifier looks like `pages/project-id/page-name.md`

module Site.Page where

import Hakyll
import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!?), (!))

import Site.Common hiding (CompiledPage(..))

pageIdentifier :: ProjectId -> PageId -> Identifier
pageIdentifier projId pgId = fromFilePath $ FPL.joinPath ["pages", projId, pgId ++ ".md"]

projectPagesGlob :: ProjectId -> Pattern
projectPagesGlob projId = fromGlob $ FPL.joinPath ["pages", projId, "*.md"]

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

getUnderlyingBaseName :: Compiler String
getUnderlyingBaseName = identifierBaseName <$> getUnderlying

identifierBaseName :: Identifier -> String
identifierBaseName = FPL.takeBaseName . toFilePath

compilePageTitles :: ProjectId -> Compiler (Map String String)
compilePageTitles projId = getAllMetadata (projectPagesGlob projId) >>= pageTitlesMap
  where
    pageTitlesMap :: [(Identifier, Metadata)] -> Compiler (Map String String)
    pageTitlesMap = extractPageTitles . M.mapKeys identifierBaseName . M.fromList

    extractPageTitles :: Map String Metadata -> Compiler (Map String String)
    extractPageTitles mds = sequence (M.mapWithKey getPageTitle mds)

    getPageTitle :: (MonadFail m) => String -> Metadata -> m String
    getPageTitle name md = maybe err return $ lookupString "title" md
      where
        err = fail $ "ConceptualFP.compilePageTitles.getPageTitle: no title metadata for " ++ name

compilePrevNextContext :: [PageId] -> Map String String -> Compiler (Context String)
compilePrevNextContext pageOrder pgTitles = do
  bn <- getUnderlyingBaseName
  let
    pn = pagePrevNexts pageOrder !? bn
    pfield = maybe mempty (\p -> constField "prev-page" p <> constField "prev-page-title" (pgTitles ! p)) $ pn >>= prev
    nfield = maybe mempty (\n -> constField "next-page" n <> constField "next-page-title" (pgTitles ! n)) $ pn >>= next
  return $ pfield <> nfield

data PrevNext
  = NoPrevNext
  | Prev String
  | Next String
  | PrevNext String String
  deriving (Show)

prev :: PrevNext -> Maybe String
prev NoPrevNext = Nothing
prev (Prev n) = Just n
prev (Next _) = Nothing
prev (PrevNext n _) = Just n

next :: PrevNext -> Maybe String
next NoPrevNext = Nothing
next (Prev _) = Nothing
next (Next n) = Just n
next (PrevNext _ n) = Just n

prevnext :: Maybe String -> Maybe String -> PrevNext
prevnext Nothing  Nothing  = NoPrevNext
prevnext (Just p) Nothing  = Prev p
prevnext Nothing  (Just n) = Next n
prevnext (Just p) (Just n) = PrevNext p n

-- don't make too many chapters, or else the lazy foldl will become inefficient! :)
pagePrevNexts :: [PageId] -> Map String PrevNext
pagePrevNexts pageOrder = foldl go M.empty $ pnTriples pageOrder
  where
    go pns (mp, n, mn) = M.insert n (prevnext mp mn) pns

pnTriples :: [a] -> [(Maybe a, a, Maybe a)]
pnTriples [] = []
pnTriples [x] = [(Nothing, x, Nothing)]
pnTriples (x1:x2:xs) = (Nothing, x1, Just x2) : pnTriples' x1 x2 xs
  where
    pnTriples' p x [] = [(Just p, x, Nothing)]
    pnTriples' p x (n:ns) = (Just p, x, Just n) : pnTriples' x n ns
