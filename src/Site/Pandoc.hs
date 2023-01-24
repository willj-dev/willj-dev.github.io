{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Site.Pandoc where
  
import Site.Config (configCompiler, config_rst, rst_prefix, rst_suffix)

import Data.Text (Text)
import qualified Data.Text as T
import Hakyll.Core.Compiler (Compiler, getResourceBody, makeItem, debugCompiler)
import Hakyll.Core.Item (Item (itemBody))
import Hakyll.Web.Pandoc
  ( defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
  )
import Hakyll.Web.Template.Context (Context, field, defaultContext)
import Text.Pandoc.Class (PandocPure, getLog, runPure)
import Text.Pandoc.Options (ReaderOptions, WriterOptions (writerWrapText), WrapOption (WrapNone))
import Text.Pandoc.Readers.RST (readRST)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Logging (LogMessage, showLogMessage)
import Text.Pandoc.Shared (headerShift)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Data.Functor ((<&>))

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {writerWrapText = WrapNone}

rstContext :: forall a. Context a
rstContext = rstPrefixField <> rstSuffixField
  where
    rstPrefixField = rstField "rst-prefix" rst_prefix
    rstSuffixField = rstField "rst-suffix" rst_suffix

    rstField n f = field n (const $ f . config_rst <$> configCompiler)

rstCompiler :: Item String -> Compiler String
rstCompiler rstItem = applyRSTTemplate >>= compilePandoc <&> T.unpack
  where
    applyRSTTemplate :: Compiler Text
    applyRSTTemplate = T.pack . itemBody <$>
      loadAndApplyTemplate "templates/base.rst" (rstContext <> defaultContext ) rstItem

    pandocPureRSTtoHTML5 :: Text -> PandocPure Text
    pandocPureRSTtoHTML5 rst = readRST myReaderOptions rst
      >>= writeHtml5String myWriterOptions . headerShift 1

    compilePandocPure :: PandocPure Text -> Compiler Text
    compilePandocPure p = either (fail . pandocErrorMsg) pandocLogger (runPure pWithLogs)
      where
        pandocErrorMsg err = "Site.Terms.compileTermDefinitionPandoc: pandoc failed: " ++ show err

        pWithLogs = (,) <$> getLog <*> p

        pandocLogger :: ([LogMessage], Text) -> Compiler Text
        pandocLogger (logs, out) = do
          sequence_ (debugCompiler . T.unpack . showLogMessage <$> logs)
          return out

    compilePandoc = compilePandocPure . pandocPureRSTtoHTML5


-- Compiler for pages/*.rst
pageCompiler :: Compiler (Item String)
pageCompiler = getResourceBody >>= rstCompiler >>= makeItem
