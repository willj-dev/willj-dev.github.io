{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Site.Utopia.Terms (pageContext) where

import Site.Common
import Site.Config
import Site.Pandoc (rstCompiler, rstContext)

import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Identifier (fromFilePath)
import Hakyll.Core.Item (Item (itemBody))
import Hakyll.Web.Template.Context (Context, defaultContext, field, listField, listFieldWith)

-- Context for pages/*.rst, with the term list and RST prefix/suffix
pageContext :: Context String
pageContext = mconcat [termsField, rstContext, defaultContext]
  where
    termsField = listField "terms" termContext termItems
    termItems = fmap makeTermItem . utopia_terms . config_utopia <$> configCompiler
    makeTermItem = makeItemWith (\(Term term _ _) -> fromFilePath $ "__term_" ++ term)

termContext :: Context Term
termContext = mconcat [termField, defnField, altsField]
  where
    termField = field "term" (return . term_term . itemBody)

    defnField = field "definition" compileDefinitionRST

    compileDefinitionRST :: Item Term -> Compiler String
    compileDefinitionRST = rstCompiler . fmap term_definition

    altsField = listFieldWith "alternatives" altContext altItems

    altItems termItem = return $ makeAltItem termItem <$> term_alternatives (itemBody termItem)
    makeAltItem termItem termAlt = makeSubItemWith (const termAlt) (const altTuple) termItem
      where
        altTuple = (term_term $ itemBody termItem, termAlt)

    altContext = mconcat [altField, altTermField]
    altField = field "alternative" (return . snd . itemBody)
    altTermField = field "term" (return . fst . itemBody)
