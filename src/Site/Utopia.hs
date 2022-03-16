
module Site.Utopia (utopiaRules) where

import Hakyll

import Site.Common
import Site.Config
import Site.Pandoc
import Site.Project
import Site.Utopia.Terms

muProjectId :: ProjectId
muProjectId = "millennial-utopia"

-- Not really a "read this before that" as in ConceptualFP, but just a
-- non-arbitrary order with "fundamentals" earlier on
pageOrder :: [PageId]
pageOrder =
  [ "msc"
  , "constitution"
  , "proving-mu-viability"
  , "a-millennial-utopia"
  , "selected-principles"
  , "transition"
  , "managing-shared-resources"
  , "utopian-capital"
  , "implementation-ideas"
  , "marketing-ideas"
  ]

utopiaRules :: Rules ProjectMetadata
utopiaRules = do
  loadTemplates
  compileJs
  compilePages
  compileIndex
  projectMetadata muProjectId

loadTemplates, compileJs, compilePages :: Rules ()
loadTemplates = match "templates/millennial-utopia/*" $ compile templateBodyCompiler

compileJs = match "js/millennial-utopia/*.js" $ do
  route idRoute
  compile $ do
    config <- config_utopia <$> configCompiler
    jsTemplate <- getResourceBody
    applyAsTemplate (termsContext config) jsTemplate

compilePages = matchProjectPages muProjectId $ do
  route tailHTMLRoute
  compile $ do
    config <- config_utopia <$> configCompiler
    utopiaMDCompiler >>= applyUtopiaTemplates config

compileIndex :: Rules ()
compileIndex = matchProjectIndex muProjectId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/millennial-utopia/base.md" defaultContext
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyProjectIndexTemplates pageOrder

utopiaMDCompiler :: Compiler CompiledPage
utopiaMDCompiler = getResourceBody
  >>= loadAndApplyTemplate "templates/millennial-utopia/base.md" defaultContext
  >>= compilePandocMarkdown
  >>= compilePandocPage

applyUtopiaTemplates :: UtopiaConfig -> CompiledPage -> Compiler (Item String)
applyUtopiaTemplates config (CompiledPage t _ _ pageHTMLItem toc) =
  loadAndApplyTemplate "templates/millennial-utopia/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> termsContext config <> defaultContext
      baseContext = projectIdContext <> headerTitleContext <> defaultContext
      headerTitleContext = constField "header-title" t

-- term list
termsContext :: UtopiaConfig -> Context String
termsContext config = listField "terms" termContext termItems
  where
    termItems = return $ makeTermItem <$> utopia_terms config
    makeTermItem = makeItemWith (\(Term term _ _) -> fromFilePath $ "__term_" ++ term)
