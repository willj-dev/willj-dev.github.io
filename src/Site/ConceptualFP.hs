
module Site.ConceptualFP (conceptualFPRules) where

import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Rules (Rules, route, compile, match)

import Site.Config (config_cfp, configCompiler, cfp_rst, ConceptualFPConfig)
import Site.Common
import Site.Pandoc (rstBodyCompiler, rstContext)
import Site.ConceptualFP.PseudoML (loadPseudoMLSyntax, compilePandocWithPseudoML)
import Hakyll.Web.Template (templateBodyCompiler, loadAndApplyTemplate)
import Hakyll.Web.Template.Context (Context, defaultContext)
import Hakyll.Core.Compiler (getResourceBody, Compiler)
import Hakyll.Core.Item (Item)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)

indexId :: Identifier
indexId = "pages/conceptual-fp/index.rst"

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
    applyCFPTemplates config toc body

-- todo: toc in correct chapter order
compileIndex = matchOnly indexId $ do
  route tailHTMLRoute
  compile $ do
    config <- cfp_rst . config_cfp <$> configCompiler
    rstBodyCompiler config >>= applyIndexTemplates True

  where
    chapterOrder :: [String]
    chapterOrder =
      [ "introduction"
      , "basic-concepts"
      , "intermediate-typeclasses"
      , "monad-field-guide"
      , "recursion-schemes"
      , "advanced-typeclasses"
      , "appendix"
      ]

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
