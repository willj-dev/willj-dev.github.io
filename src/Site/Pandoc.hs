{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Site.Pandoc(TOCCompiler, rstContext, rstCompiler, rstBodyCompiler) where
  
import Site.Config (RSTConfig, rst_prefix, rst_suffix)

import Data.Text (Text)
import qualified Data.Text as T
import Hakyll.Core.Compiler (Compiler, getResourceBody, makeItem, debugCompiler)
import Hakyll.Core.Item (Item (itemBody))
import Hakyll.Web.Pandoc
  ( defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
  )
import Hakyll.Web.Template.Context (Context, field, defaultContext, constField)
import Text.Pandoc.Class (PandocPure, getLog, runPure)
import Text.Pandoc.Options (ReaderOptions (readerStandalone), WriterOptions (writerWrapText, writerTableOfContents), WrapOption (WrapNone))
import Text.Pandoc.Readers.RST (readRST)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Logging (showLogMessage)
import Text.Pandoc.Shared (headerShift)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Text.Pandoc (Pandoc (Pandoc))
import Text.Pandoc.Writers.Shared (toTableOfContents)

-- a compiler containing a TOC context (a $toc$ field with an HTLM5 string), and the actual output
type TOCCompiler a = Compiler (Context String, a)

rstContext :: forall a. RSTConfig -> Context a
rstContext rstConfig = rstPrefixField <> rstSuffixField
  where
    rstPrefixField = rstField "rst-prefix" rst_prefix
    rstSuffixField = rstField "rst-suffix" rst_suffix

    rstField n f = field n (const . return $ f rstConfig)

rstCompiler :: RSTConfig -> Item String -> TOCCompiler String
rstCompiler rstConfig rstItem = do
  (htmlTOC, htmlBody) <- applyRSTTemplate >>= compilePandocWithTOC
  return (constField "toc" htmlTOC, htmlBody)
  where
    applyRSTTemplate :: Compiler Text
    applyRSTTemplate = T.pack . itemBody <$>
      loadAndApplyTemplate "templates/base.rst" (rstContext rstConfig <> defaultContext) rstItem

-- Compiles RST from the current resource body, rather than an arbitrary string
rstBodyCompiler :: RSTConfig -> TOCCompiler (Item String)
rstBodyCompiler rstConfig = do
  (toc, body) <- getResourceBody >>= rstCompiler rstConfig
  bodyItem <- makeItem body
  return (toc, bodyItem)

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions
  { readerStandalone = True
  }

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
  { writerWrapText = WrapNone
  , writerTableOfContents = True
  }

compilePandocWithTOC :: Text -> Compiler (String, String)
compilePandocWithTOC bodyRST = compilePandocPure $ do
  doc <- headerShift 1 <$> readRST myReaderOptions bodyRST
  body <- T.unpack <$> writeHtml5String myWriterOptions doc
  toc <- T.unpack <$> pandocPureToHTML5TOC doc
  return (toc, body)

pandocPureToHTML5TOC :: Pandoc -> PandocPure Text
pandocPureToHTML5TOC (Pandoc meta blocks) = writeHtml5String myWriterOptions (Pandoc meta [tocBlock])
  where
    tocBlock = toTableOfContents myWriterOptions blocks

compilePandocPure :: PandocPure a -> Compiler a
compilePandocPure p = either (fail . pandocErrorMsg) pandocLogger (runPure $ pair getLog p)
  where
    pandocErrorMsg err = "Site.Terms.compileTermDefinitionPandoc: pandoc failed: " ++ show err

    pandocLogger (logs, out) = do
      sequence_ (debugCompiler . T.unpack . showLogMessage <$> logs)
      return out

pair :: Applicative m => m a -> m b -> m (a, b)
pair x y = (,) <$> x <*> y
