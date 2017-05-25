module Main where

import Imports
import Page
import Types
import Sitemap
import Config

import qualified Dhall

import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.PDF
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Error (IOError)


data UserData = UserData
  { userKeywords :: [Keyword]
  , userEntries  :: [ProcessedEntry]
  , userConfig   :: Config
  } deriving (Eq, Show)

newtype Dhallow a = Dhallow
  { fromDhallow :: ReaderT UserData IO a
  } deriving (Functor, Applicative, Monad, MonadReader UserData, MonadIO)

instance MonadKeywords Dhallow where
  getKeywords = userKeywords <$> ask

instance MonadConfig Dhallow where
  getConfig = userConfig <$> ask

instance MonadEntries Dhallow where
  getEntries = userEntries <$> ask


runDhallow :: Dhallow a -> IO a
runDhallow dhallow = do
  posts <- filterM (doesFileExist . ("posts/"++)) =<< listDirectory "posts/"
  rawEntries <- forM posts $ \p -> do
    ent <- Dhall.detailed $ Dhall.input Dhall.auto
      (TL.pack $ "./posts/" ++ p)
    utc <- getModificationTime $ "posts/" ++ p
    pure (RawEntry ent utc)

  keywordFiles <- filterM (doesFileExist . ("keywords/"++)) =<< listDirectory "keywords/"
  allKeywords <- forM keywordFiles $ \k -> do
    t <- readFile ("keywords/" ++ k)
    Dhall.input Dhall.auto (fromStrict t)

  config@Config{..} <- readConfig

  case processEntries rawEntries of
    Left e -> fail (displayException e)
    Right processedEntries -> do
      let userData = UserData allKeywords processedEntries config
      runReaderT (fromDhallow dhallow) userData

main :: IO ()
main = runDhallow $ do
  Config{..} <- getConfig
  putStrLn $ "Switching into directory '" <> configFolder <> "'."
  liftIO $ setCurrentDirectory configFolder

  writeFrontPages
  writeEntries
  writeSitemap


writeSitemap :: (MonadConfig m, MonadIO m, MonadKeywords m, MonadEntries m) => m ()
writeSitemap = do
  entries <- getEntries
  allKeywords <- getKeywords
  today <- utctDay <$> liftIO getCurrentTime
  sitemap <- createSitemap $ (concat :: [[a]] -> [a])
    [ flip map entries $ \(ProcessedEntry url entryModified Entry{..}) ->
        PageData
          { pageLocation = url
          , pageLastMod = entryModified
          , pageType = Article entryImportance
          }
    , flip map entries $ \(ProcessedEntry url entryModified Entry{..}) ->
        PageData
          { pageLocation = T.pack $ T.unpack url -<.> ".pdf"
          , pageLastMod = entryModified
          , pageType = Article entryImportance
          }
    , flip map allKeywords $ \k -> do
        PageData
          { pageLocation = displayUrl k
          , pageLastMod = maximum
              $ (UTCTime (addDays (-3000) today) 0:) -- in case the category is empty
              $ map entryModified $ filter ((k `elem`) . entryKeywords . fromProcessedEntry) $ entries
          , pageType = FrontPage
          }
    , (:[]) $ PageData
      { pageLocation = "index.html"
      , pageLastMod = maximum
              $ (UTCTime (addDays (-3000) today) 0:) -- in case there are no entries
              $ map entryModified entries
      , pageType = FrontPage
      }
    ]
  liftIO $ BL.writeFile "sitemap.xml" sitemap


writeFrontPages :: (MonadConfig m, MonadIO m, MonadKeywords m, MonadEntries m) => m ()
writeFrontPages = do
  entries <- getEntries
  allKeywords <- getKeywords
  page <- renderFrontPage Nothing $ map entryToHeadline entries
  liftIO $ BL.writeFile "index.html" page

  forM_ allKeywords $ \k -> do
    let name = T.unpack $ displayUrl k
    let entr = filter ((k `elem`) . entryKeywords . fromProcessedEntry) $ entries
    liftIO $ createDirectoryIfMissing False name
    page <- renderFrontPage (Just k) $ map entryToHeadline entr
    liftIO $ BL.writeFile (name </> "index.html") page


writeEntries :: (MonadConfig m, MonadIO m, MonadEntries m) => m ()
writeEntries = do
  Config{..} <- getConfig
  entries <- getEntries
  latexTemplate <- liftIO $ readFile (".." </> TL.unpack configLatexTemplate)
  forM_ entries $ \pe@(ProcessedEntry u entryModified e) -> do
    liftIO $ createDirectoryIfMissing False "posts"

    utc <- liftIO $ try $ getAccessTime (T.unpack u)
    when (either (const True) (<entryModified)
                 (utc :: Either IOError UTCTime)) $ do
      putStrLn $ "Writing '" <> u <> "'"
      page <- renderPage pe
      liftIO $ BL.writeFile (T.unpack u) $ page
      let pdfOptions = def {
          writerHighlight = True
        , writerTemplate = Just $ T.unpack latexTemplate
        , writerVariables =
          [ ("title", TL.unpack $ entryTitle e)
          , ("author", TL.unpack $ configAuthor)
          , ("documentclass", "article")
          , ("papersize", "a4")
          , ("fontsize", "12pt")
          , ("linestretch", "1.5")
          , ("geometry", "margin=3cm")
          ]
        }

      let pdfname = T.unpack u -<.> ".pdf"
      putStrLn $ "Writing '" <> pdfname <> "'"
      pdf <- liftIO $ makePDF "xelatex" writeLaTeX pdfOptions (entryContent e)
      case pdf of
        Left b -> putStr $ "Error while creating '"
                        <> BL.pack (map (fromIntegral . ord) pdfname)
                        <> "': " <> b
        Right b -> liftIO $ BL.writeFile pdfname b
