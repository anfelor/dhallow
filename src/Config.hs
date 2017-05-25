module Config
  ( Config(..)
  , CssConfig(..)
  , JsConfig(..)
  , TwitterConfig(..)
  , MonadConfig(..)
  , readConfig
  ) where

import Imports hiding (handle)
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
  , configTwitter :: Maybe TwitterConfig
  , configLocale :: Dhall.Text
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
  , twitter :: Maybe DhallTwitterConfig
  , locale :: Dhall.Text
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

data DhallTwitterConfig = DhallTwitterConfig
  { handle :: Dhall.Text
  , imageUrl :: Dhall.Text
  } deriving (Eq, Show, Generic)
instance Dhall.Interpret DhallTwitterConfig

data TwitterConfig = TwitterConfig
  { twitterHandle :: Dhall.Text
  , twitterImageUrl :: Dhall.Text
  } deriving (Eq, Show, Generic)
instance Dhall.Interpret TwitterConfig

-- | A monad which can access the config.
-- This could have a default implementation
-- `getConfig = liftIO readConfig`
-- but that might cause issues with System.Directory.setDirectory.
class Monad m => MonadConfig m where
  getConfig :: m Config

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
    , configTwitter = convertTwitter <$> twitter
    , configLocale = locale
    }
  where
    convertTwitter dt = TwitterConfig (handle dt) (imageUrl dt)
