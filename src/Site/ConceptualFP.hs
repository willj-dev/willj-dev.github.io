module Site.ConceptualFP (conceptualFPRules) where

import Hakyll

import Site.Common
import Site.Page
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
  projectMetadata cfpProjectId

loadTemplates, compilePages, compileIndex :: Rules ()
loadTemplates = match "templates/conceptual-fp/*" $ compile templateBodyCompiler

compilePages = matchProjectPages cfpProjectId $ do
  route tailHTMLRoute
  compile $ do
    pg <- cfpPageCompiler
    pn <- compilePageTitles cfpProjectId >>= compilePrevNextContext pageOrder
    applyCFPTemplates pn pg

compileIndex = matchProjectIndex cfpProjectId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/conceptual-fp/base.md" defaultContext
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyProjectIndexTemplates pageOrder

cfpPageCompiler :: Compiler CompiledPage
cfpPageCompiler = getResourceBody
  >>= loadAndApplyTemplate "templates/conceptual-fp/base.md" defaultContext
  >>= compilePandocWithPseudoML

applyCFPTemplates :: Context String -> CompiledPage -> Compiler (Item String)
applyCFPTemplates pnContext (CompiledPage t _ _ pageHTMLItem toc) =
  loadAndApplyTemplate "templates/conceptual-fp/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> pnContext <> defaultContext
      baseContext = projectIdContext <> constField "header-title" t <> defaultContext
