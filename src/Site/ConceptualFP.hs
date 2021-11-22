module Site.ConceptualFP (conceptualFPRules) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!), (!?))
import Hakyll.Core.Compiler (getResourceBody, Compiler, getUnderlying)
import Hakyll.Core.Identifier (Identifier, toFilePath)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Rules (Rules, route, compile, match)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)
import Hakyll.Web.Template (templateBodyCompiler, loadAndApplyTemplate)
import Hakyll.Web.Template.Context (Context, defaultContext, constField)
import System.FilePath (takeBaseName)

import Site.Common
import Site.ConceptualFP.PseudoML (loadPseudoMLSyntax, compilePandocWithPseudoML)
import Site.Config (config_cfp, configCompiler, cfp_rst, ConceptualFPConfig)
import Site.Pandoc (rstBodyCompiler, rstContext)
import Hakyll.Core.Metadata (getAllMetadata, Metadata, lookupString)

indexId :: Identifier
indexId = "pages/conceptual-fp/index.rst"

pageOrder :: [String]
pageOrder =
  [ "introduction"
  , "basic-concepts"
  , "intermediate-typeclasses"
  , "monad-field-guide"
  , "recursion-schemes"
  , "advanced-typeclasses"
  , "appendix-lang"
  , "appendix-foldr"
  ]

conceptualFPRules :: Rules ProjectMetadata
conceptualFPRules = do
  loadTemplates
  loadPseudoMLSyntax
  compilePages
  compileIndex
  projectMetadata indexId

loadTemplates, compilePages, compileIndex :: Rules ()
loadTemplates = match "templates/conceptual-fp/*" $ compile templateBodyCompiler

compilePages = matchGlobExceptIndex "pages/conceptual-fp/*.rst" $ do
  route tailHTMLRoute
  compile $ do
    config <- config_cfp <$> configCompiler
    rstItem <- getResourceBody
    (toc, body) <- compilePandocWithPseudoML rstItem
    pn <- compilePageTitles >>= compilePrevNextContext
    applyCFPTemplates config (toc <> pn) body

-- todo: toc in correct page order
compileIndex = matchOnly indexId $ do
  route tailHTMLRoute
  compile $ do
    config <- cfp_rst . config_cfp <$> configCompiler
    rstBodyCompiler config >>= applyIndexTemplates True

applyCFPTemplates :: ConceptualFPConfig -> Context String -> Item String -> Compiler (Item String)
applyCFPTemplates config tocContext pageHTMLItem =
  loadAndApplyTemplate "templates/conceptual-fp/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = tocContext <> conceptualFPContext config
      baseContext = projectIdContext <> headerTitleContext <> defaultContext

conceptualFPContext :: ConceptualFPConfig -> Context String
conceptualFPContext config = mconcat [cfpRSTContext, defaultContext]
  where
    cfpRSTContext = rstContext (cfp_rst config)

getUnderlyingBaseName :: Compiler String
getUnderlyingBaseName = identifierBaseName <$> getUnderlying

identifierBaseName :: Identifier -> String
identifierBaseName = takeBaseName . toFilePath

compilePrevNextContext :: Map String String -> Compiler (Context String)
compilePrevNextContext pgTitles = do
  bn <- getUnderlyingBaseName
  let
    pn = pagePrevNexts !? bn
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

-- don't make too many chapters, or else the lazy foldl will become inefficient!
pagePrevNexts :: Map String PrevNext
pagePrevNexts = foldl go M.empty $ pnTriples pageOrder
  where
    go pns (mp, n, mn) = M.insert n (prevnext mp mn) pns

compilePageTitles :: Compiler (Map String String)
compilePageTitles = getAllMetadata "pages/conceptual-fp/*.rst" >>= pageTitlesMap
  where
    pageTitlesMap :: [(Identifier, Metadata)] -> Compiler (Map String String)
    pageTitlesMap = extractPageTitles . M.mapKeys identifierBaseName . M.fromList

    extractPageTitles :: Map String Metadata -> Compiler (Map String String)
    extractPageTitles mds = sequence (M.mapWithKey getPageTitle mds)

    getPageTitle :: (MonadFail m) => String -> Metadata -> m String
    getPageTitle name md = maybe err return $ lookupString "title" md
      where
        err = fail $ "ConceptualFP.compilePageTitles.getPageTitle: no title metadata for " ++ name

pnTriples :: [a] -> [(Maybe a, a, Maybe a)]
pnTriples [] = []
pnTriples [x] = [(Nothing, x, Nothing)]
pnTriples (x1:x2:xs) = (Nothing, x1, Just x2) : pnTriples' x1 x2 xs
  where
    pnTriples' p x [] = [(Just p, x, Nothing)]
    pnTriples' p x (n:ns) = (Just p, x, Just n) : pnTriples' x n ns
