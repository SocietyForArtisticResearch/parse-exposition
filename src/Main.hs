{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
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
  )
import Text.XML.Cursor.Generic (Cursor(..), node)

data Exposition = Exposition
  { title :: T.Text
  , author :: T.Text
  , date :: T.Text
  , tools :: [Tool]
  } deriving (Generic, Show)

instance ToJSON (Exposition)

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
  , url :: String
  , download :: Maybe String
  }

type ToolTypeName = T.Text

type ToolSpec = (ToolTypeName, (Cursor Node -> ToolContent))

-- | Get json-ld metadata for a particular content field
-- Only works for published expositions
getMeta :: Cursor Node -> T.Text -> T.Text
getMeta cursor name =
  T.concat $ cursor $// attributeIs "name" name >=> attribute "content"

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

downloadTools :: ImportOptions -> [Tool] -> IO ()
downloadTools options tools =
  case download options of
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
  mdTools <- mapM toolToMarkdown (tools exp)
  return $ exp {tools = mdTools}

-- writeExpMarkdown :: Exposition -> IO ()
-- writeExpMarkdown exp = do
--   md <- liftM expToMarkdown exp
--   TIO.putStrLn $ encodeTxt md
--------------------
--- Main functions
--------------------
insertFileNames :: ImportOptions -> Exposition -> Exposition
insertFileNames options exp =
  case download options of
    Nothing -> exp
    Just dir ->
      exp
        { tools =
            map
              (\tool -> tool {toolMediaFile = toolFileName tool dir})
              (tools exp)
        }

parseExposition :: Cursor Node -> Exposition
parseExposition cursor =
  Exposition
    { title = getMeta cursor "citation_title"
    , author = getMeta cursor "citation_author"
    , date = getMeta cursor "citation_publication_date"
    , tools = L.concat $ map (\spec -> getTools spec cursor) toolSpecs
    }

getExposition :: ImportOptions -> IO Exposition
getExposition options = do
  req <- parseRequest (url options)
  doc <- httpSink req $ const sinkDoc
  return $ insertFileNames options $ parseExposition (fromDocument doc)

parseArgs :: [String] -> ImportOptions
parseArgs args = makeOptions args (ImportOptions False "" Nothing)
  where
    makeOptions :: [String] -> ImportOptions -> ImportOptions
    makeOptions ("-m":t) options = makeOptions t (options {markdown = True})
    makeOptions ("-d":t) options =
      makeOptions t (options {download = Just "media"})
    makeOptions (url:t) options = makeOptions t (options {url = url})
    makeOptions [] options = options

usage = putStrLn "Usage: parse-exposition [-m] [-d] rc-url"

exit = exitWith ExitSuccess

expToTxt :: Pan.PandocMonad m => m Exposition -> m T.Text
expToTxt exp = encodeTxt <$> exp

encodeMdTxt :: Exposition -> IO T.Text
encodeMdTxt exp = Pan.runIOorExplode $ expToTxt (expToMarkdown exp)

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs args
  exp <- getExposition options
  downloadTools options (tools exp)
  if markdown options
    then TIO.putStrLn =<< encodeMdTxt exp
    else TIO.putStrLn $ encodeTxt exp
