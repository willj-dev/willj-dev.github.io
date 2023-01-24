
module Site.Common where

import Hakyll

import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine
import qualified System.FilePath.Posix as FPP -- always use '/' to make site routes

-- | kebab-case id for a project (e.g. "conceptual-fp").
type ProjectId = String

-- | kebab-case id for a page (e.g. "a-millennial-utopia"); the identifier for the file
--   containing a page's content is always at 'pages/$project-id$/$page-id$.rst'
type PageId = String

-- | kebab-case "canonical" section name, which should always exist and be targetable
--   by URL fragments, e.g. '/millennial-utopia/a-millennial-utopia.html#government-transparency'
type SectionId = String

-- | Arbitrary text that can serve as an RST hyperlink reference, e.g. `Government Transparency`_
--   (There may be multiple ways to refer to the same section, depending on context)
type LinkText = String

data CompiledPage = CompiledPage
  { page_title        :: String,
    page_name         :: PageId,
    page_blurb        :: Maybe String,
    page_html         :: Item String,
    page_htmlTOC      :: String
  }

-- 'blah/project-id/page.foo' -> 'project-id'
projectId :: MonadFail m => Identifier -> m ProjectId
projectId itemId = case FPL.splitDirectories (toFilePath itemId) of
    [_, pid, _] -> return pid
    _           -> fail $ "could not determine project id for " ++ show itemId

identifierExtension :: MonadFail m => Identifier -> m String
identifierExtension itemId = case FPL.takeExtension (toFilePath itemId) of
  "tex" -> return "tex"
  "rst" -> return "rst"
  _ -> fail $ "unknown file extension: " ++ show itemId

filenameOnlyRoute, htmlExtensionRoute, tailRoute, tailHTMLRoute :: Routes
filenameOnlyRoute = customRoute (FPL.takeFileName . toFilePath)
htmlExtensionRoute = setExtension "html"
tailRoute = customRoute (FPP.joinPath . tail . FPL.splitPath . toFilePath)
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

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key mkVal = Context $ \k _ i ->
  if k == key
    then mkVal i >>= maybe (noResult $ "Nothing value for " ++ key) (return . StringField)
    else noResult $ "Tried maybeField " ++ key

dbg :: Show a => a -> Compiler a
dbg x = debugCompiler (show x) >> return x
