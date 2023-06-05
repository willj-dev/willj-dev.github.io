module Site.GeometricUniverse (guRules) where

import Hakyll

import Site.Common
import Site.Pandoc
import Site.Project
import Site.Page

guProjectId :: ProjectId
guProjectId = "geometric-universe"

pageOrder :: [PageId]
pageOrder =
  [ "metaphysics"
  , "universe"
  , "algebra"
  , "physics"
  , "science"
  ]

guRules :: Rules ProjectMetadata
guRules = do
  loadTemplates
  compilePages
  compileIndex
  projectMetadata guProjectId

loadTemplates, compilePages :: Rules ()
loadTemplates = match "templates/geometric-universe/*" $ compile templateBodyCompiler

compilePages = matchProjectPages guProjectId $ do
  route tailHTMLRoute
  compile $ do
    pg <- guMDCompiler
    pn <- compilePageTitles guProjectId >>= compilePrevNextContext pageOrder
    applyGUTemplates pn pg

compileIndex :: Rules ()
compileIndex = matchProjectIndex guProjectId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyProjectIndexTemplates pageOrder

guMDCompiler :: Compiler CompiledPage
guMDCompiler = getResourceBody
  >>= compilePandocMarkdown
  >>= compilePandocPage

applyGUTemplates :: Context String -> CompiledPage -> Compiler (Item String)
applyGUTemplates pnContext (CompiledPage t _ _ pageHTMLItem toc) =
  loadAndApplyTemplate "templates/geometric-universe/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> pnContext <> defaultContext
      baseContext = projectIdContext <> headerTitleContext <> defaultContext
      headerTitleContext = constField "header-title" t
