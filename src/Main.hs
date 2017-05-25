module Main where

import Imports
import Page
import Types
import Sitemap
import Config

import qualified Dhall

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.PDF
import Data.Time.Clock
import Data.Time.Calendar


data UserData = UserData
  { userKeywords :: [Keyword]
  , userEntries  :: [ProcessedEntry]
  } deriving (Eq, Show)

newtype Dhallow a = Dhallow
  { fromDhallow :: ReaderT UserData IO a
  } deriving (Functor, Applicative, Monad, MonadReader UserData, MonadIO)

instance MonadKeywords Dhallow where
  getUserData = userKeywords <$> ask

main :: IO ()
main = do
  posts <- filterM (doesFileExist . ("posts/"++)) =<< listDirectory "posts/"
  rawEntries <- forM posts $ \p -> do
    Dhall.detailed $ Dhall.input
      (Dhall.auto :: Dhall.Type RawEntry)
      (TL.pack $ "./posts/" ++ p)

  keywordFiles <- filterM (doesFileExist . ("keywords/"++)) =<< listDirectory "keywords/"
  allKeywords <- forM keywordFiles $ \k -> do
    t <- readFile ("keywords/" ++ k)
    Dhall.input Dhall.auto (fromStrict t)

  case processEntries rawEntries of
    Left e -> fail (TL.unpack e)
    Right en -> writeFiles en allKeywords


writeFiles :: [ProcessedEntry] -> [Keyword] -> IO ()
writeFiles entries allKeywords = do
  config@Config{..} <- readConfig

  removePathForcibly configFolder
  createDirectory configFolder
  putStrLn $ "Switching into directory '" <> configFolder <> "'."
  setCurrentDirectory configFolder

  writeFrontPages config entries allKeywords
  writeEntries config entries allKeywords
  writeSitemap config entries allKeywords


writeSitemap :: Config -> [ProcessedEntry] -> [Keyword] -> IO ()
writeSitemap config entries allKeywords = do
  today <- utctDay <$> getCurrentTime
  BL.writeFile "sitemap.xml" $ createSitemap config $ (concat :: [[a]] -> [a])
    [ flip map entries $ \(ProcessedEntry url Entry{..}) ->
        PageData
          { pageLocation = url
          , pageLastMod = entryUpdated
          , pageType = Article entryImportance
          }
    , flip map entries $ \(ProcessedEntry url Entry{..}) ->
        PageData
          { pageLocation = T.pack $ T.unpack url -<.> ".pdf"
          , pageLastMod = entryUpdated
          , pageType = Article entryImportance
          }
    , flip map allKeywords $ \k -> do
        PageData
          { pageLocation = displayUrl k
          , pageLastMod = maximum
              $ (addDays (-1000) today:)
              $ map (entryUpdated . fromProcessedEntry) $ filter ((k `elem`) . entryKeywords . fromProcessedEntry) $ entries
          , pageType = FrontPage
          }
    , (:[]) $ PageData
      { pageLocation = "index.html"
      , pageLastMod = maximum
              $ (addDays (-1000) today:)
              $ map (entryUpdated . fromProcessedEntry) $ entries
      , pageType = FrontPage
      }
    ]


writeFrontPages :: Config -> [ProcessedEntry] -> [Keyword] -> IO ()
writeFrontPages config entries allKeywords = do
  BL.writeFile "index.html"
    $ renderFrontPage config Nothing allKeywords
    $ map entryToHeadline entries

  forM_ allKeywords $ \k -> do
    let name = T.unpack $ displayUrl k
    let entr = filter ((k `elem`) . entryKeywords . fromProcessedEntry) $ entries
    createDirectoryIfMissing False name
    BL.writeFile (name </> "index.html")
      $ renderFrontPage config (Just k) allKeywords
      $ map entryToHeadline entr


writeEntries :: Config -> [ProcessedEntry] -> [Keyword] -> IO ()
writeEntries config@Config{..} entries allKeywords = do
  latexTemplate <- readFile (".." </> TL.unpack configLatexTemplate)
  forM_ entries $ \pe@(ProcessedEntry u e) -> do
    createDirectoryIfMissing False "posts"
    putStrLn $ "Writing '" <> u <> "'"
    BL.writeFile (T.unpack u) $ renderPage config pe
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
    pdf <- makePDF "xelatex" writeLaTeX pdfOptions (entryContent e)
    case pdf of
      Left b -> putStr $ "Error while creating '"
                      <> BL.pack (map (fromIntegral . ord) pdfname)
                      <> "': " <> b
      Right b -> BL.writeFile pdfname b
