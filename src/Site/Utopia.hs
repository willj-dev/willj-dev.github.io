{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Site.Utopia (utopiaRules) where

import Site.Common
import Site.Config (UtopiaConfig(utopia_rst), Config (config_utopia), configCompiler, Term (Term), utopia_terms)
import Site.Pandoc (rstBodyCompiler, rstContext, TOCCompiler)
import Site.Utopia.Terms (termContext)

import Hakyll.Core.Compiler (Compiler, getResourceBody)
import Hakyll.Core.Identifier (Identifier, fromFilePath)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules ( compile, match, Rules, route )
import Hakyll.Web.Template (templateBodyCompiler, loadAndApplyTemplate, applyAsTemplate)
import Hakyll.Web.Template.Context (Context, listField, defaultContext)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)

indexId :: Identifier
indexId = "pages/millennial-utopia/index.rst"

utopiaRules :: Rules ProjectMetadata
utopiaRules = do
  loadTemplates
  compileJs
  compilePages
  compileIndex
  projectMetadata indexId

loadTemplates, compileJs, compilePages, compileIndex :: Rules ()
loadTemplates = match "templates/millennial-utopia/*" $ compile templateBodyCompiler

compileJs = match "js/millennial-utopia/*.js" $ do
  route idRoute
  compile $ do
    config <- config_utopia <$> configCompiler
    jsTemplate <- getResourceBody
    applyAsTemplate (utopiaContext config) jsTemplate

compilePages = matchGlobExceptIndex "pages/millennial-utopia/*.rst" $ do
  route tailHTMLRoute
  compile $ do
    config <- config_utopia <$> configCompiler
    (toc, body) <- utopiaRSTCompiler config
    applyUtopiaTemplates config toc body

compileIndex = matchOnly indexId $ do
  route tailHTMLRoute
  compile $ configCompiler >>= utopiaRSTCompiler . config_utopia >>= applyIndexTemplates True . snd

utopiaRSTCompiler :: UtopiaConfig -> TOCCompiler (Item String)
utopiaRSTCompiler = rstBodyCompiler . utopia_rst

applyUtopiaTemplates :: UtopiaConfig -> Context String -> Item String -> Compiler (Item String)
applyUtopiaTemplates config tocContext pageHTMLItem =
  loadAndApplyTemplate "templates/millennial-utopia/page.html" (tocContext <> utopiaContext config) pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      baseContext = projectIdContext <> headerTitleContext <> defaultContext

-- term list; RST prefix/suffix for custom roles and link targets
utopiaContext :: UtopiaConfig -> Context String
utopiaContext config = mconcat [termsField, utopiaRSTContext, defaultContext]
  where
    termsField = listField "terms" termContext termItems
    termItems = return $ makeTermItem <$> utopia_terms config
    makeTermItem = makeItemWith (\(Term term _ _) -> fromFilePath $ "__term_" ++ term)

    utopiaRSTContext = rstContext (utopia_rst config)
