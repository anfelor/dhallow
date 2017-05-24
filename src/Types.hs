module Types where

import Imports hiding (Text)
import qualified Data.HashSet as Set
import qualified Data.Text as T
import Dhall (Interpret(..), Natural, Text, Vector)


data DhallDay = DhallDay
  { year :: Natural
  , month :: Natural
  , day :: Natural
  } deriving (Eq, Show, Generic)

instance Interpret DhallDay

-- | A blog entry.
data Entry a b = Entry
  { entryTitle :: Text
  , entryCreated :: b
  , entryUpdated :: b
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

instance (Interpret a, Interpret b) => Interpret (Entry a b)

-- | A reduced version of a blog post,
-- featuring only information used in the
-- overview pages.
data Headline = Headline
  { headlineTitle :: Text
  , headlineCreated :: Day
  , headlineUpdated :: Day
  , headlineKeywords :: Vector Keyword
  , headlineURL :: T.Text
  , headlineAbstract :: String
  } deriving (Eq, Show)

instance Ord Headline where
  compare h g = case comparing headlineCreated h g of
    LT -> LT
    GT -> GT
    EQ -> comparing headlineTitle h g


addEntryURLs :: Ord b => [Entry a b] -> Either (Entry a b) [(T.Text, Entry a b)]
addEntryURLs = fmap fst . flip runStateT Set.empty . mapM go . sort
  where
    go :: Entry a b -> StateT (Set.HashSet T.Text) (Either (Entry a b)) (T.Text, Entry a b)
    go e@Entry{..} = do
      used <- get
      let poss = fmap (T.intercalate "-" . fmap displayUrl) $ permutations $ toList entryKeywords
          title = displayUrl (toStrict entryTitle)
          urls = fmap (\p -> "posts/" <> title <> "-" <> p <> ".html") poss
          choose = filter (`notElem` used) urls
      case choose of
        [] -> lift (Left e)
        (x:_) -> do
          put (Set.insert x used)
          pure (x, e)

entriesToHeadline :: [(T.Text, Entry Pandoc Day)] -> [Headline]
entriesToHeadline = fmap go
  where
    go (url, Entry {..}) = Headline
      { headlineTitle = entryTitle
      , headlineCreated = entryCreated
      , headlineUpdated = entryUpdated
      , headlineKeywords = entryKeywords
      , headlineAbstract = writeHtmlString def entryAbstract
      , headlineURL = url
      }


data PageData = PageData
  { pageLocation :: T.Text
  , pageLastMod :: Day
  , pageType :: PageType
  } deriving (Eq, Show)

data PageType = Article Importance | FrontPage
  deriving (Eq, Show)


-- | The place where comments should be posted.
data Comments
 = Reddit {_redditUrl :: Text}
 | Github -- ^ open a new issue on github.
 deriving (Eq, Show, Generic)
instance Interpret Comments


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
instance Dhall.Interpret Keyword
instance UrlDisplay Keyword where
  displayShow = toStrict . keywordTitle

