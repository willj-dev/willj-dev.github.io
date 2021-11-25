
-- | Functions ending with ' use the underlying resource; others use the `Item` passed to the function
module Site.Pandoc where

import qualified Data.Text as T
import Data.Text (Text)

import Hakyll
import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc
import Text.Pandoc.Shared (headerShift, inlineListToIdentifier, stringify)
import Text.Pandoc.Writers.Shared (toTableOfContents)

import Site.Common

-- | Apply the base RST template then parse it through Pandoc, using default reader options
compilePandocRST :: Item String -> Compiler (Item Pandoc)
compilePandocRST = compilePandocRSTWith defaultHakyllReaderOptions

-- | Apply the base RST template then parse it through Pandoc, using default reader options
compilePandocRST' :: Compiler (Item Pandoc)
compilePandocRST' = compilePandocRSTWith' defaultHakyllReaderOptions

-- | Apply the base RST template to the current resource then parse it through Pandoc, using given reader options
compilePandocRSTWith' :: ReaderOptions -> Compiler (Item Pandoc)
compilePandocRSTWith' ropt = getResourceBody >>= compilePandocRSTWith ropt

-- | Apply the base RST template then parse it through Pandoc, using given reader options
compilePandocRSTWith :: ReaderOptions -> Item String -> Compiler (Item Pandoc)
compilePandocRSTWith ropt rstItem = do
  preprocRST <- T.pack . itemBody <$> applyRSTTemplate 
  parsedPandoc <- compilePandocPure (headerShift 1 <$> readRST ropt preprocRST)
  makeItem parsedPandoc
  where
    applyRSTTemplate = loadAndApplyTemplate "templates/base.rst" defaultContext rstItem

-- | Compile a Pandoc document to HTML, using default writer options
compileHTMLPandoc :: Item Pandoc -> Compiler String
compileHTMLPandoc = compileHTMLPandocWith myDefaultWriterOptions

-- | Compile a Pandoc document to HTML, using given writer options
compileHTMLPandocWith :: WriterOptions -> Item Pandoc -> Compiler String
compileHTMLPandocWith wopt docItem = compilePandocPure $ T.unpack <$> writeHtml5String wopt (itemBody docItem)

-- | Compile a Pandoc document using default writer options, with attached page metadata for templating
compilePandocPage :: Item Pandoc -> Compiler CompiledPage
compilePandocPage = compilePandocPageWith myDefaultWriterOptions

-- | Compile a Pandoc document using given writer options, with attached page metadata for templating
compilePandocPageWith :: WriterOptions -> Item Pandoc -> Compiler CompiledPage
compilePandocPageWith wopt docItem = do
  title <- getMetadataField' pageIdentifier "title"
  blurb <- getMetadataField pageIdentifier "blurb"
  (toc, htmlBody) <- compilePandocPure runPandoc
  bodyItem <- makeItem htmlBody
  return $ CompiledPage title pageId blurb bodyItem toc
  where
    -- note: this is the full Hakyll Identifier (i.e. file path), not what I call a page-id
    pageIdentifier = itemIdentifier docItem
    pageId = FPL.takeBaseName $ toFilePath pageIdentifier
    doc = itemBody docItem

    runPandoc = do
      toc <- pandocContents doc
      htmlBody <- writeHtml5 wopt (addAnchorLinks doc)
      return (T.unpack toc, renderHtml htmlBody)

    pandocContents (Pandoc meta blocks) = writeHtml5String wopt (Pandoc meta [toTableOfContents wopt blocks])

myDefaultWriterOptions :: WriterOptions
myDefaultWriterOptions = defaultHakyllWriterOptions { writerWrapText = WrapNone }

compilePandocPure :: PandocPure a -> Compiler a
compilePandocPure p = either (fail . pandocErrorMsg) pandocLogger (runPure $ pair getLog p)
  where
    pandocErrorMsg err = "Site.Pandoc.compilePandocPure: pandoc failed: " ++ show err

    pandocLogger (logs, out) = do
      sequence_ (debugCompiler . T.unpack . showLogMessage <$> logs)
      return out

pair :: Applicative m => m a -> m b -> m (a, b)
pair x y = (,) <$> x <*> y

addAnchorLinks :: Pandoc -> Pandoc
addAnchorLinks (Pandoc m bs) = Pandoc m (addInlineAnchor <$> bs)
  where
    addInlineAnchor :: Block -> Block
    addInlineAnchor (Header l a ils) = Header l a (ils ++ [anchorLink ils])
    addInlineAnchor (Div a divBs) = Div a (addInlineAnchor <$> divBs)
    addInlineAnchor b = b

    anchorLink :: [Inline] -> Inline
    anchorLink hInlines = Link attr [Str "anchor"] ("#" <> anchorId hInlines, anchorTitle hInlines)
      where attr = ("", ["anchor"], [])

    anchorId :: [Inline] -> Text
    anchorId = inlineListToIdentifier emptyExtensions

    anchorTitle :: [Inline] -> Text
    anchorTitle = stringify
