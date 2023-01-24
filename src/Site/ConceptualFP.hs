{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.ConceptualFP (conceptualFPRules) where

import Site.Common

import Data.Binary (Binary, encode, get, put)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Routes (setExtension, composeRoutes)
import Hakyll.Core.Rules (Rules, compile, match, route)
import Hakyll.Core.Writable (Writable, write)
import Skylighting.Parser (parseSyntaxDefinitionFromText)
import Skylighting.Types (Syntax)
import Hakyll.Core.Identifier (Identifier)
import Site.Pandoc (rstBodyCompiler)

indexId :: Identifier
indexId = "pages/conceptual-fp/index.rst"

conceptualFPRules :: Rules ProjectMetadata
conceptualFPRules = do
  loadPseudoMLSyntax
  compileIndex
  projectMetadata indexId

loadPseudoMLSyntax, compileIndex :: Rules ()

loadPseudoMLSyntax = match "pseudoml.xml" $ return () -- compile syntaxCompiler
  where
    syntaxCompiler :: Compiler (Item (WriteableBinary Syntax))
    syntaxCompiler = undefined

compileIndex = matchOnly indexId $ do
  route $ composeRoutes tailRoute (setExtension "html")
  compile $ rstBodyCompiler >>= applyIndexTemplates

-- Need a bit of misdirection here because Hakyll has a Writable instance for
-- a lazy bytestring, but not for Binary.
newtype WriteableBinary a = WriteableBinary {unWB :: a}
  deriving (Eq, Show, Functor)

instance (Binary a) => Binary (WriteableBinary a) where
  get = WriteableBinary <$> get
  put = put . unWB

instance (Binary a) => Writable (WriteableBinary a) where
  write fp binaryItem = write fp (fmap encode <$> binaryItem)