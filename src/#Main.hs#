{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (isSpace)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Binary as CB
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import GHC.Generics
import Network.HTTP.Simple (getResponseBody, httpSink, httpSource, parseRequest)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment
import System.Exit
import System.FilePath ((</>), takeExtension)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.HTML.DOM (sinkDoc)
import qualified Text.Pandoc as Pan
import qualified Text.Pandoc.Options as PanOptions
import Text.XML (Element(..), Name, Node(..))
import Text.XML.Cursor
  ( ($/)
  , ($//)
  , ($|)
  , (&/)
  , (&//)
  , (&|)
  , (>=>)
  , attribute
  , attributeIs
  , check
  , content
  , element
  , fromDocument
  , hasAttribute
  , parent
  )
import Text.XML.Cursor.Generic (Cursor(..), node, toCursor)

data Exposition = Exposition
  { expositionToc :: [(T.Text, T.Text)]
  , expositionId :: T.Text
  , expositionMetaData :: ExpositionMetaData
  , expositionWeaves :: [Weave]
  } deriving (Generic, Show)

instance ToJSON (Exposition)

data Weave = Weave
  { weaveTitle :: T.Text
  , weaveUrl :: T.Text
  , weaveTools :: [Tool]
  } deriving (Generic, Show)

instance ToJSON (Weave)

data Position = Position
  { left :: Maybe Int
  , top :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON (Position)

data Size = Size
  { width :: Maybe Int
  , height :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON (Size)

data Tool = Tool
  { toolMediaFile :: Maybe String
  , toolId :: T.Text
  , position :: Position
  , size :: Size
  , toolContent :: ToolContent
  } deriving (Generic, Show)

instance ToJSON (Tool)

-- | Content types to be extended
data ToolContent
  = TextContent { textToolContent :: T.Text }
  | ImageContent { imageUrl :: T.Text }
  | VideoContent { videoUrl :: T.Text }
  deriving (Generic, Show)

instance ToJSON (ToolContent)

data ImportOptions = ImportOptions
  { markdown :: Bool
  , expId :: String
  , download :: Maybe String
  }

type ToolTypeName = T.Text

type ToolSpec = (ToolTypeName, (Cursor Node -> ToolContent))

data ExpositionMetaData = ExpoMetaData
  { metaTitle :: T.Text
  , metaDate :: T.Text
  , metaAuthors :: [T.Text]
  , metaKeywords :: [T.Text]
  , metaExpMainUrl :: T.Text
  } deriving (Generic, Show)

instance ToJSON (ExpositionMetaData)

-- | Encodes an object as strict json text
encodeTxt :: ToJSON a => a -> T.Text
encodeTxt a = toStrict . decodeUtf8 $ encodePretty a

-- | Checks if class attribute string contains a certain class
checkClass :: T.Text -> Cursor Node -> Bool
checkClass cls cursor =
  case (node cursor) of
    (NodeElement (Element _ attr _)) ->
      case M.lookup "class" attr of
        Just classes -> T.isInfixOf cls classes
        _ -> False
    _ -> False

-- | Get style property from a list of lists
styleProperty :: T.Text -> [[T.Text]] -> Maybe Int
styleProperty prop propLst =
  case L.find (\propPair -> (head propPair) == prop) propLst of
    Just val ->
      case decimal $ second val of
        Right (num, _) -> Just num
        _ -> Nothing
    _ -> Nothing
  where
    second = head . tail

-- | Extract size and position from a list of style strings
extractSizePos :: [T.Text] -> ([Position], [Size])
extractSizePos styles = (positions, sizes)
  where
    assocStyles =
      map (\style -> map (T.splitOn ":") $ T.splitOn ";" style) styles
    lefts = map (styleProperty "left") assocStyles
    tops = map (styleProperty "top") assocStyles
    positions = Position <$> lefts <*> tops
    widths = map (styleProperty "width") assocStyles
    heights = map (styleProperty "height") assocStyles
    sizes = Size <$> widths <*> heights

--------------------
--- Specification of tool functions
--------------------
textContent :: Cursor Node -> ToolContent
textContent cursor =
  TextContent $ (toStrict . renderHtml . toHtml . node) cursor

imageContent :: Cursor Node -> ToolContent
imageContent cursor = ImageContent img
  where
    img = T.concat $ cursor $// element "img" >=> attribute "src"

videoContent :: Cursor Node -> ToolContent
videoContent cursor = VideoContent video
  where
    video = T.concat $ cursor $// attribute "data-file"

toolSpecs :: [ToolSpec]
toolSpecs =
  [("text", textContent), ("picture", imageContent), ("video", videoContent)]

-- | Get tools of a certain type
getTools :: ToolSpec -> Cursor Node -> [Tool]
getTools (toolTypeName, contentFun) cursor =
  L.zipWith4 (Tool Nothing) ids positions sizes toolContent
  where
    tools = cursor $// attributeIs "data-tool" toolTypeName
    ids = map (T.concat . attribute "data-id") tools
    styles = map (T.concat . attribute "style") tools
    (positions, sizes) = extractSizePos styles
    content =
      cursor $// attributeIs "data-tool" toolTypeName &/
      check (checkClass "tool-content")
    toolContent = map contentFun content

--------------------
--- File download
--------------------
downloadFile :: String -> String -> IO ()
downloadFile url fname = do
  req <- (parseRequest url)
  runConduitRes $ httpSource req getResponseBody .| CB.sinkFile fname

toolUrl :: Tool -> Maybe String
toolUrl tool =
  case toolContent tool of
    ImageContent url -> Just (T.unpack url)
    VideoContent url -> Just (T.unpack url)
    _ -> Nothing

toolFileExtension :: Tool -> Maybe String
toolFileExtension tool =
  let fname url = Just $ (takeWhile (/= '?') $ takeExtension url)
   in fname =<< toolUrl tool

toolFileName :: Tool -> String -> Maybe String
toolFileName tool dir =
  (\ending -> dir </> (T.unpack $ toolId tool) ++ ending) <$>
  toolFileExtension tool

downloadTool :: String -> Tool -> IO ()
downloadTool dir tool =
  let fileName = toolFileName tool dir
      url = toolUrl tool
   in case (fileName, url) of
        (Just fnameStr, Just urlStr) -> do
          downloadFile urlStr fnameStr
        _ -> do
          return ()

downloadTools :: ImportOptions -> Exposition -> IO ()
downloadTools options exposition =
  let tools = L.concat $ map weaveTools (expositionWeaves exposition)
   in case download options of
        Nothing -> do
          return ()
        Just dir -> do
          exists <- doesDirectoryExist dir
          if not exists
            then createDirectory dir
            else return ()
          mapM (downloadTool dir) tools
          return ()

--------------------
--- Markdown conversion
--------------------
toolToMarkdown :: Pan.PandocMonad m => Tool -> m Tool
toolToMarkdown tool =
  case toolContent tool of
    TextContent txt -> do
      pantxt <- Pan.readHtml PanOptions.def txt
      mdtxt <- Pan.writeMarkdown PanOptions.def pantxt
      return tool {toolContent = TextContent mdtxt}
    _ -> do
      return tool

expToMarkdown :: Pan.PandocMonad m => Exposition -> m Exposition
expToMarkdown exp = do
  mdTools <- mapM (mapM toolToMarkdown) (map weaveTools (expositionWeaves exp))
  return $
    exp
      { expositionWeaves =
          zipWith (\w t -> w {weaveTools = t}) (expositionWeaves exp) mdTools
      }

--------------------
--- Main functions
--------------------
insertFileNames :: ImportOptions -> Exposition -> Exposition
insertFileNames options exp =
  case download options of
    Nothing -> exp
    Just dir ->
      let toolsFname =
            map
              (map (\tool -> tool {toolMediaFile = toolFileName tool dir}))
              (map weaveTools (expositionWeaves exp))
       in exp
            { expositionWeaves =
                zipWith
                  (\w t -> w {weaveTools = t})
                  (expositionWeaves exp)
                  toolsFname
            }

getWeave :: (T.Text, T.Text) -> IO Weave
getWeave (title, url) = do
  req <- parseRequest $ "https://www.researchcatalogue.net" <> T.unpack url
  doc <- httpSink req $ const sinkDoc
  let cursor = fromDocument doc
  let tools = L.concat $ map (\spec -> getTools spec cursor) toolSpecs
  return $ Weave {weaveTitle = title, weaveUrl = url, weaveTools = tools}

parseExposition :: T.Text -> ExpositionMetaData -> Cursor Node -> IO Exposition
parseExposition id metadata cursor = do
  let expToc = toc cursor
  weaves <- mapM getWeave expToc
  return $ Exposition expToc id metadata weaves

toc :: Cursor Node -> [(T.Text, T.Text)]
toc cursor =
  let tocTitles =
        cursor $// attributeIs "class" "mainmenu" &/
        check (checkClass "menu-home") &//
        element "a" &/
        content
      tocUrls =
        cursor $// attributeIs "class" "mainmenu" &/
        check (checkClass "menu-home") &//
        element "a" >=>
        attribute "href"
   in L.filter (\(title, url) -> T.isInfixOf "view" url) $ zip tocTitles tocUrls

getExposition :: ImportOptions -> ExpositionMetaData -> IO Exposition
getExposition options metadata = do
  req <- parseRequest $ T.unpack (metaExpMainUrl metadata)
  doc <- httpSink req $ const sinkDoc
  exp <- parseExposition (T.pack (expId options)) metadata (fromDocument doc)
  return $ insertFileNames options exp

detailsUrl :: String -> String
detailsUrl expId =
  "https://www.researchcatalogue.net/profile/show-exposition?exposition=" <>
  expId

trim :: T.Text -> T.Text
trim = T.dropWhileEnd isSpace . T.dropWhile isSpace

unwrapTxt :: Maybe T.Text -> T.Text
unwrapTxt Nothing = ""
unwrapTxt (Just txt) = txt

parseDetailsPage :: Cursor Node -> ExpositionMetaData
parseDetailsPage cursor =
  let title =
        T.concat $
        cursor $// element "h2" >=>
        attributeIs "class" "meta-headline" &/ content
      authors =
        cursor $// element "h2" >=>
        attributeIs "class" "meta-headline" &| parent &// element "a" &/ content
      mainUrl =
        T.concat $
        cursor $// attributeIs "class" "meta-left-col" &/
        attributeIs "class" "button" >=>
        attribute "href"
      ths =
        cursor $// attributeIs "class" "meta-table" &// element "th" &/ content
      tds =
        cursor $// attributeIs "class" "meta-table" &// element "td" &/ content
      metadata = zip ths tds
   in ExpoMetaData
        { metaTitle = trim title
        , metaDate = unwrapTxt $ lookup "date" metadata
        , metaKeywords = T.splitOn "," $ unwrapTxt $ lookup "keywords" metadata
        , metaAuthors = L.concat authors
        , metaExpMainUrl = mainUrl
        }

getDetailsPageData :: ImportOptions -> IO ExpositionMetaData
getDetailsPageData options = do
  req <- parseRequest $ detailsUrl (expId options)
  doc <- httpSink req $ const sinkDoc
  return $ parseDetailsPage (fromDocument doc)

parseArgs :: [String] -> ImportOptions
parseArgs args = makeOptions args (ImportOptions False "" Nothing)
  where
    makeOptions :: [String] -> ImportOptions -> ImportOptions
    makeOptions ("-m":t) options = makeOptions t (options {markdown = True})
    makeOptions ("-d":t) options =
      makeOptions t (options {download = Just "media"})
    makeOptions (id:t) options = makeOptions t (options {expId = id})
    makeOptions [] options = options

usage = putStrLn "Usage: parse-exposition [-m] [-d] exposition-id"

exit = exitWith ExitSuccess

expToTxt :: Pan.PandocMonad m => m Exposition -> m T.Text
expToTxt exp = encodeTxt <$> exp

encodeMdTxt :: Exposition -> IO T.Text
encodeMdTxt exp = Pan.runIOorExplode $ expToTxt (expToMarkdown exp)

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs args
  details <- getDetailsPageData options
  exp <- getExposition options details
  downloadTools options exp
  if markdown options
    then TIO.putStrLn =<< encodeMdTxt exp
    else TIO.putStrLn $ encodeTxt exp
