{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Site.Utopia (utopiaRules) where

import Site.Config (configCompiler)

import Hakyll.Web.Template.Context (Context)
import Hakyll.Core.Rules
import Hakyll.Web.Template (templateBodyCompiler)

utopiaRules :: Rules ()
utopiaRules = do
  loadTemplates

utopiaContext :: forall a. Context a
utopiaContext = undefined

loadTemplates :: Rules ()
loadTemplates = match "templates/*" $ compile templateBodyCompiler
