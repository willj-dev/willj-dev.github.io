{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Site.Utopia (utopiaRules) where

import Site.Common

import Hakyll.Web.Template.Context (Context)
import Hakyll.Core.Rules ( compile, match, Rules, route )
import Hakyll.Web.Template (templateBodyCompiler)
import Hakyll.Core.Routes (composeRoutes, setExtension)
import Hakyll.Core.Identifier (Identifier)
import Site.Pandoc (rstBodyCompiler)

indexId :: Identifier
indexId = "pages/millennial-utopia/index.rst"

utopiaRules :: Rules ProjectMetadata
utopiaRules = do
  loadTemplates
  compileIndex
  projectMetadata indexId

loadTemplates, compileIndex :: Rules ()
loadTemplates = match "templates/*" $ compile templateBodyCompiler

compileIndex = matchOnly indexId $ do
  route $ composeRoutes tailRoute (setExtension "html")
  compile $ rstBodyCompiler >>= applyIndexTemplates

utopiaContext :: forall a. Context a
utopiaContext = undefined
