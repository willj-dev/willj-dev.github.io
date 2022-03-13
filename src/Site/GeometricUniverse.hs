module Site.GeometricUniverse (guRules) where

import Hakyll

import Site.Common
import Site.Pandoc
import Site.Project

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
  projectMetadata guProjectId "tex"

loadTemplates, compilePages :: Rules ()
loadTemplates = match "templates/geometric-universe/*" $ compile templateBodyCompiler

compilePages = matchProjectPages guProjectId $ do
  route tailHTMLRoute
  compile $ guRSTCompiler >>= applyGUTemplates

compileIndex :: Rules ()
compileIndex = matchProjectIndex guProjectId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= compilePandocTeXWith defaultHakyllReaderOptions
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyProjectIndexTemplates pageOrder "tex"

guRSTCompiler :: Compiler CompiledPage
guRSTCompiler = getResourceBody
  >>= compilePandocTeXWith defaultHakyllReaderOptions
  >>= compilePandocPage

applyGUTemplates :: CompiledPage -> Compiler (Item String)
applyGUTemplates (CompiledPage t _ _ pageHTMLItem toc) =
  loadAndApplyTemplate "templates/geometric-universe/page.html" pageContext pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> defaultContext
      baseContext = projectIdContext <> headerTitleContext <> defaultContext
      headerTitleContext = constField "header-title" t
