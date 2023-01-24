{-# LANGUAGE DeriveFunctor #-}

module Site.ConceptualFP.PseudoML (loadPseudoMLSyntax, compilePandocWithPseudoML) where

import Control.Monad.Except (throwError)
import Data.Binary (Binary(..), encode)
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import Hakyll
import Skylighting
import Text.Pandoc

import Site.Common
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

compilePandocWithPseudoML :: Item String -> Compiler CompiledPage
compilePandocWithPseudoML rstItem = do
  woptPseudoML <- wopt <$> (unWB <$> loadBody "pseudoml.xml")
  compilePandocRST rstItem >>= compilePandocPageWith woptPseudoML
  where
    wopt s = myDefaultWriterOptions { writerSyntaxMap = M.insert "PseudoML" s defaultSyntaxMap }

-- Need a bit of misdirection here because Hakyll has a Writable instance for
-- a lazy bytestring, but not for Binary.
newtype WriteableBinary a = WriteableBinary {unWB :: a}
  deriving (Eq, Show, Functor)

instance (Binary a) => Binary (WriteableBinary a) where
  get = WriteableBinary <$> get
  put = put . unWB

instance (Binary a) => Writable (WriteableBinary a) where
  write fp binaryItem = write fp (fmap encode <$> binaryItem)
