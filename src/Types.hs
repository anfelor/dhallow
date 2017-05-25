module Types where

import GHC.Generics (to)
import Imports hiding (Text)
import qualified Data.HashSet as Set
import Data.List (zip3)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Dhall (Interpret(..), Natural, Text, Vector, InterpretOptions(..), defaultInterpretOptions, genericAutoWith)


data DhallDay = DhallDay
  { year :: Natural
  , month :: Natural
  , day :: Natural
  } deriving (Eq, Ord, Show, Generic)

instance Interpret DhallDay where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions


-- | The structure of a blog entry.
-- See below for specialized versions.
data Entry a b = Entry
  { entryTitle :: Text
  , entryCreated :: b
  , entryKeywords :: Vector Keyword
  , entryImportance :: Importance
  , entryAbstract :: a
  , entryContent :: a
  , entryComments :: Comments
  } deriving (Show, Functor, Generic)

instance (Eq b) => Eq (Entry a b) where
  a == b = entryTitle a == entryTitle b
        && entryCreated a == entryCreated b

instance (Ord b) => Ord (Entry a b) where
  compare a b = case comparing entryCreated a b of
    LT -> LT
    GT -> GT
    EQ -> comparing entryTitle a b

instance (Interpret a, Interpret b) => Interpret (Entry a b) where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 5 }


dropStart :: Int64 -> Text -> Text
dropStart n = firstLower . TL.drop n
  where
    firstLower txt = case TL.uncons txt of
      Nothing -> txt
      Just (h, t) -> TL.cons (toLower h) t


-- | A blog entry as written by the user.
data RawEntry = RawEntry
  { fromRawEntry :: Entry Text DhallDay
  , rawEntryModified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)


-- | The processing step includes parsing the text with pandoc,
-- making the datastructures more Haskell friendly and
-- adding a generated url field.
data ProcessedEntry = ProcessedEntry
  { entryUrl :: T.Text
  , entryModified :: UTCTime
  , fromProcessedEntry :: Entry Pandoc Day
  } deriving (Eq, Ord, Show)

class Monad m => MonadEntries m where
  getEntries :: m [ProcessedEntry]


data ProcessingException
  = PandocException PandocError
  | CouldntFindUrl
  deriving (Show)
instance Exception ProcessingException

processEntries :: [RawEntry] -> Either ProcessingException [ProcessedEntry]
processEntries re = do
  entries <- sequence $ fmap (parseEntry . fromRawEntry) re
  urls <- fmap fst $ flip runStateT Set.empty $ mapM addEntryURL entries
  pure $ map (\(u, r, e) -> ProcessedEntry u r e) (zip3 urls (fmap rawEntryModified re) entries)
  where
    parseEntry :: Entry Text DhallDay -> Either ProcessingException (Entry Pandoc Day)
    parseEntry ent =
      let entry = ent {
            entryCreated = (\DhallDay{..} -> fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) (entryCreated ent)
          }
      in case (readMarkdown def (TL.unpack $ entryAbstract entry), readMarkdown def (TL.unpack $ entryContent entry)) of
        (Left p1, _) -> Left $ PandocException p1
        (_, Left p2) -> Left $ PandocException p2
        (Right p1, Right p2) -> Right $ entry {entryAbstract = p1, entryContent = p2}


    addEntryURL :: Ord b => Entry a b -> StateT (Set.HashSet T.Text) (Either ProcessingException) T.Text
    addEntryURL Entry{..} = do
      used <- get
      let kws = T.intercalate "-" $ fmap displayUrl $ toList entryKeywords
          title = displayUrl (toStrict entryTitle)
          url = "posts/" <> title <> "-" <> kws <> ".html"
      if url `elem` used
        then lift $ Left CouldntFindUrl
        else do
          put (Set.insert url used)
          pure url


-- | A reduced version of a blog post,
-- featuring only information used in the
-- overview pages.
data Headline = Headline
  { headlineTitle :: Text
  , headlineCreated :: Day
  , headlineUpdated :: UTCTime
  , headlineKeywords :: Vector Keyword
  , headlineURL :: T.Text
  , headlineAbstract :: String
  } deriving (Eq, Show)

instance Ord Headline where
  compare h g = case comparing headlineCreated h g of
    LT -> LT
    GT -> GT
    EQ -> comparing headlineTitle h g

entryToHeadline :: ProcessedEntry -> Headline
entryToHeadline (ProcessedEntry url entryModified Entry {..}) = Headline
      { headlineTitle = entryTitle
      , headlineCreated = entryCreated
      , headlineUpdated = entryModified
      , headlineKeywords = entryKeywords
      , headlineAbstract = writeHtmlString def entryAbstract
      , headlineURL = url
      }


data PageData = PageData
  { pageLocation :: T.Text
  , pageLastMod :: UTCTime
  , pageType :: PageType
  } deriving (Eq, Show)

data PageType = Article Importance | FrontPage
  deriving (Eq, Show)


-- | The place where comments should be posted.
data Comments
 = Reddit {_redditUrl :: Text}
 | Github -- ^ open a new issue on github.
 deriving (Eq, Show, Generic)

instance Interpret Comments where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = const "url" }



-- | The importance of a blog post.
data Importance
  = Normal -- ^ In doubt, pick this.
  | Ignore -- ^ Not shown on homepage and low priority in sitemap
  | Promote -- ^ Highlighted on homepage and high priority in sitemap
  deriving (Eq, Show, Generic)
instance Interpret Importance


-- | The keywords for a blog post.
-- Using Data types instead of text has the advantage,
-- that posts can be grouped by keyword and also, that
-- the number of keywords is limited to 100-1000.
data Keyword = Keyword
  { keywordTitle :: Dhall.Text
  , keywordDescription :: Dhall.Text
  } deriving (Eq, Show, Generic)

instance Interpret Keyword where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 7 }

instance UrlDisplay Keyword where
  displayShow = toStrict . keywordTitle

class MonadKeywords m where
  getKeywords :: m [Keyword]


data Config = Config
  { configCss :: Vector CssConfig
  , configJs :: Vector JsConfig
  , configLatexTemplate :: Text
  , configDomain :: Text
  , configHttps :: Bool
  , configAuthor :: Text
  , configSiteName :: Text
  , configFolder :: Text
  , configTwitter :: Maybe TwitterConfig
  , configLocale :: Text
  } deriving (Eq, Show, Generic)

instance Interpret Config where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 6 }


newtype CssConfig = CssConfig
  { cssSrc :: Dhall.Text
  } deriving (Eq, Show, Generic)

instance Interpret CssConfig where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 3 }


data JsConfig = JsConfig
  { jsSrc :: Dhall.Text
  , jsAsLastElement :: Bool
  } deriving (Eq, Show, Generic)

instance Interpret JsConfig where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 2 }


data TwitterConfig = TwitterConfig
  { twitterHandle :: Dhall.Text
  , twitterImageUrl :: Dhall.Text
  } deriving (Eq, Show, Generic)

instance Interpret TwitterConfig where
  autoWith _ = fmap GHC.Generics.to $ genericAutoWith defaultInterpretOptions
    { fieldModifier = dropStart 7 }


-- | A monad which can access the config.
-- This could have a default implementation
-- `getConfig = liftIO readConfig`
-- but that might cause issues with System.Directory.setDirectory.
class Monad m => MonadConfig m where
  getConfig :: m Config
