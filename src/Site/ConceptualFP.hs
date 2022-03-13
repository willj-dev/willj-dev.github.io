module Site.ConceptualFP (conceptualFPRules) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!), (!?))
import Hakyll
import System.FilePath (takeBaseName)

import Site.Common
import Site.ConceptualFP.PseudoML
import Site.Pandoc
import Site.Project

cfpProjectId :: ProjectId
cfpProjectId = "conceptual-fp"

pageOrder :: [PageId]
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
  projectMetadata cfpProjectId "rst"

loadTemplates, compilePages, compileIndex :: Rules ()
loadTemplates = match "templates/conceptual-fp/*" $ compile templateBodyCompiler

compilePages = matchProjectPages cfpProjectId $ do
  route tailHTMLRoute
  compile $ do
    pg <- cfpRSTCompiler
    pn <- compilePageTitles >>= compilePrevNextContext
    applyCFPTemplates pn pg

compileIndex = matchProjectIndex cfpProjectId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/conceptual-fp.rst" defaultContext
    >>= compilePandocRST
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyProjectIndexTemplates pageOrder "rst"

cfpRSTCompiler :: Compiler CompiledPage
cfpRSTCompiler = getResourceBody
  >>= loadAndApplyTemplate "templates/conceptual-fp.rst" defaultContext
  >>= compilePandocWithPseudoML

applyCFPTemplates :: Context String -> CompiledPage -> Compiler (Item String)
applyCFPTemplates pnContext (CompiledPage t _ _ pageHTMLItem toc) =
  loadAndApplyTemplate "templates/conceptual-fp/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> pnContext <> defaultContext
      baseContext = projectIdContext <> constField "header-title" t <> defaultContext

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

-- don't make too many chapters, or else the lazy foldl will become inefficient! :)
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
