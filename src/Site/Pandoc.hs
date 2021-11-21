
module Site.Pandoc(
  rstContext, applyRSTTemplate,
  myReaderOptions, myWriterOptions,
  rstCompiler, rstBodyCompiler,
  TOCCompiler, rstTOCCompiler, rstBodyTOCCompiler,
  compilePandocWithTOC, compilePandocWithTOC') where

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
import Text.Pandoc.Options (
  ReaderOptions (readerStandalone),
  WriterOptions (writerWrapText),
  WrapOption (WrapNone)
  )
import Text.Pandoc.Readers.RST (readRST)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Logging (showLogMessage)
import Text.Pandoc.Shared (headerShift)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Text.Pandoc (Pandoc (Pandoc))
import Text.Pandoc.Writers.Shared (toTableOfContents)
  
import Site.Config (RSTConfig, rst_prefix, rst_suffix)

-- a compiler containing a TOC context (a $toc$ field with an HTLM5 string), and the actual output
type TOCCompiler a = Compiler (Context String, a)

rstContext :: forall a. RSTConfig -> Context a
rstContext rstConfig = rstPrefixField <> rstSuffixField
  where
    rstPrefixField = rstField "rst-prefix" rst_prefix
    rstSuffixField = rstField "rst-suffix" rst_suffix

    rstField n f = field n (const . return $ f rstConfig)

applyRSTTemplate :: RSTConfig -> Item String -> Compiler Text
applyRSTTemplate rstConfig rstItem = T.pack . itemBody <$>
  loadAndApplyTemplate "templates/base.rst" (rstContext rstConfig <> defaultContext) rstItem

rstCompiler :: RSTConfig -> Item String -> Compiler String
rstCompiler rstConfig rstItem = snd <$> rstTOCCompiler rstConfig rstItem

rstTOCCompiler :: RSTConfig -> Item String -> TOCCompiler String
rstTOCCompiler rstConfig rstItem = do
  (htmlTOC, htmlBody) <- applyRSTTemplate rstConfig rstItem >>= compilePandocWithTOC
  return (constField "toc" htmlTOC, htmlBody)

rstBodyCompiler :: RSTConfig -> Compiler (Item String)
rstBodyCompiler = fmap snd . rstBodyTOCCompiler

-- Compiles RST from the current resource body, rather than an arbitrary string
rstBodyTOCCompiler :: RSTConfig -> TOCCompiler (Item String)
rstBodyTOCCompiler rstConfig = do
  (toc, body) <- getResourceBody >>= rstTOCCompiler rstConfig
  bodyItem <- makeItem body
  return (toc, bodyItem)

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions { readerStandalone = True }

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions { writerWrapText = WrapNone }

compilePandocWithTOC :: Text -> Compiler (String, String)
compilePandocWithTOC = compilePandocWithTOC' myReaderOptions myWriterOptions

compilePandocWithTOC' :: ReaderOptions -> WriterOptions -> Text -> Compiler (String, String)
compilePandocWithTOC' ropt wopt bodyRST = compilePandocPure $ do
  doc <- headerShift 1 <$> readRST ropt bodyRST
  body <- T.unpack <$> writeHtml5String wopt doc
  toc <- T.unpack <$> pandocPureToHTML5TOC doc
  return (toc, body)

pandocPureToHTML5TOC :: Pandoc -> PandocPure Text
pandocPureToHTML5TOC (Pandoc meta blocks) = writeHtml5String myWriterOptions (Pandoc meta [tocBlock])
  where
    tocBlock = toTableOfContents myWriterOptions blocks

compilePandocPure :: PandocPure a -> Compiler a
compilePandocPure p = either (fail . pandocErrorMsg) pandocLogger (runPure $ pair getLog p)
  where
    pandocErrorMsg err = "Site.Pandoc.compilePandocPure: pandoc failed: " ++ show err

    pandocLogger (logs, out) = do
      sequence_ (debugCompiler . T.unpack . showLogMessage <$> logs)
      return out

pair :: Applicative m => m a -> m b -> m (a, b)
pair x y = (,) <$> x <*> y
