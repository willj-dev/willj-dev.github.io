{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DeriveFunctor #-}

module Site.ConceptualFP.PseudoML (loadPseudoMLSyntax, compilePandocWithPseudoML) where

import Control.Monad.Error (throwError)
import Data.Binary (Binary(..), encode)
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import Hakyll.Core.Compiler (Compiler, getResourceBody, makeItem, loadBody, debugCompiler)
import Hakyll.Core.Item (itemBody, Item)
import Hakyll.Core.Rules (Rules, match, compile)
import Hakyll.Core.Writable (Writable(..))
import Hakyll.Web.Template.Context (constField)
import Skylighting.Parser (parseSyntaxDefinitionFromText)
import Skylighting.Types (Syntax)
import Skylighting.Syntax (defaultSyntaxMap)
import Text.Pandoc.Options (WriterOptions(writerSyntaxMap))

import Site.Config
import Site.Pandoc

loadPseudoMLSyntax :: Rules ()
loadPseudoMLSyntax = match "pseudoml.xml" $ compile syntaxCompiler
  where
    syntaxCompiler = getResourceBody >>= handleSyntaxParserError . parseSyntaxItemString
    parseSyntaxItemString = parseSyntaxDefinitionFromText "pseudoml.xml" . TL.pack . itemBody
    handleSyntaxParserError = either handler (makeItem . WriteableBinary)

    handler :: String -> Compiler (Item (WriteableBinary Syntax))
    handler parserMsg = throwError
      [ "Site.ConceptualFP.PseudoML.loadPseudoMLSyntax: failed to parse syntax"
      , parserMsg
      ]

compilePandocWithPseudoML :: Item String -> TOCCompiler (Item String)
compilePandocWithPseudoML rstItem = do
  rstConfig <- cfp_rst . config_cfp <$> configCompiler
  pseudoMLSyntax <- (unWB <$> loadBody "pseudoml.xml") :: Compiler Syntax
  debugCompiler $ show pseudoMLSyntax
  (htmlTOC, htmlBody) <- applyRSTTemplate rstConfig rstItem >>= compilePandocWithTOC' ropt (wopt pseudoMLSyntax)
  htmlItem <- makeItem htmlBody
  return (constField "toc" htmlTOC, htmlItem)
  where
    ropt = myReaderOptions
    wopt s = myWriterOptions { writerSyntaxMap = M.insert "PseudoML" s defaultSyntaxMap }

-- Need a bit of misdirection here because Hakyll has a Writable instance for
-- a lazy bytestring, but not for Binary.
newtype WriteableBinary a = WriteableBinary {unWB :: a}
  deriving (Eq, Show, Functor)

instance (Binary a) => Binary (WriteableBinary a) where
  get = WriteableBinary <$> get
  put = put . unWB

instance (Binary a) => Writable (WriteableBinary a) where
  write fp binaryItem = write fp (fmap encode <$> binaryItem)
