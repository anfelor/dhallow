module Config
  ( Config(..)
  , CssConfig(..)
  , JsConfig(..)
  , readConfig
  ) where

import Imports
import qualified Dhall
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TL

data Config = Config
  { configCss :: [CssConfig]
  , configJs :: [JsConfig]
  , configLatexTemplate :: Dhall.Text
  , configDomain :: Dhall.Text
  , configHttps :: Bool
  , configAuthor :: Dhall.Text
  , configSiteName :: Dhall.Text
  , configFolder :: FilePath
  } deriving (Eq, Show)

data DhallConfig = DhallConfig
  { css :: Dhall.Vector CssConfig
  , js :: Dhall.Vector JsConfig
  , latexTemplate :: Dhall.Text
  , domain :: Dhall.Text
  , https :: Bool
  , author :: Dhall.Text
  , siteName :: Dhall.Text
  , folder :: Dhall.Text
  } deriving (Eq, Generic)
instance Dhall.Interpret DhallConfig

newtype CssConfig = CssConfig
  { cssSrc :: Dhall.Text
  } deriving (Eq, Show, Generic)
instance Dhall.Interpret CssConfig

data JsConfig = JsConfig
  { jsSrc :: Dhall.Text
  , jsAsLastElement :: Bool
  } deriving (Eq, Show, Generic)
instance Dhall.Interpret JsConfig

readConfig :: IO Config
readConfig = do
  file <- readFile "config.dhall"
  DhallConfig{..} <- Dhall.input Dhall.auto (fromStrict file)
  pure $ Config
    { configCss = V.toList css
    , configJs = V.toList js
    , configLatexTemplate = latexTemplate
    , configDomain = domain
    , configHttps = https
    , configAuthor = author
    , configSiteName = siteName
    , configFolder = TL.unpack folder
    }

