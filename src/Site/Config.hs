{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Site.Config where

import Control.Monad.Error (MonadError (throwError))
import Data.Aeson (FromJSON, Options (fieldLabelModifier))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Yaml.Aeson (ParseException, decodeEither')
import Hakyll.Core.Compiler (Compiler, loadBody)
import Hakyll.Core.Identifier (Identifier, fromFilePath, toFilePath)
import Hakyll.Core.Item (Item (..))

data RSTConfig = RSTConfig
  { rst_prefix :: String,
    rst_suffix :: String
  }

$(deriveJSON (defaultOptions {fieldLabelModifier = drop 4}) ''RSTConfig)

data Term = Term
  { term_term :: String,
    term_alternatives :: [String],
    term_definition :: String
  }

$(deriveJSON (defaultOptions {fieldLabelModifier = drop 5}) ''Term)

data UtopiaConfig = UtopiaConfig
  { utopia_rst :: RSTConfig,
    utopia_terms :: [Term]
  }

$(deriveJSON (defaultOptions {fieldLabelModifier = drop 7}) ''UtopiaConfig)

data Config = Config
  { config_rst :: RSTConfig,
    config_utopia :: UtopiaConfig
  }

$(deriveJSON (defaultOptions {fieldLabelModifier = drop 7}) ''Config)

-- Note: Don't forget to load the config file in Hakyll rules before using this!
configCompiler :: Compiler Config
configCompiler = loadBody "config.yaml" >>= yamlCompiler
  where
    yamlCompiler bs = either handleYamlError return (decodeEither' bs)

    -- Note: needs FlexibleContexts to compile without a type signature!
    handleYamlError :: FromJSON a => ParseException -> Compiler a
    handleYamlError pe = throwError [show pe]

makeItemWith :: (a -> Identifier) -> a -> Item a
makeItemWith makeId body = Item (makeId body) body

makeSubItemWith :: (a -> String) -> (a -> b) -> Item a -> Item b
makeSubItemWith mapIdSuffix mapBody (Item parentId parentBody) = Item newId newBody
  where
    newId = addIdSuffix parentId (mapIdSuffix parentBody)
    newBody = mapBody parentBody

addIdSuffix :: Identifier -> String -> Identifier
addIdSuffix parentId suffix = fromFilePath (toFilePath parentId ++ "_" ++ suffix)
