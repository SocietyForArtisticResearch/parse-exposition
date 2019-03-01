{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import GHC.Generics
import Network.HTTP.Simple (httpSink, parseRequest)
import System.Environment
import System.Exit
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
  { id :: T.Text
  , position :: Position
  , size :: Size
  , content :: ToolContent
  } deriving (Generic, Show)

instance ToJSON (Tool)

-- | Content types to be extended
data ToolContent
  = TextContent { content :: T.Text }
  | ImageContent { imageUrl :: T.Text }
  deriving (Generic, Show)

instance ToJSON (ToolContent)

data ImportOptions = ImportOptions
  { markdown :: Bool
  , url :: String
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

textContent :: Cursor Node -> ToolContent
textContent cursor =
  TextContent $ (toStrict . renderHtml . toHtml . node) cursor

imageContent :: Cursor Node -> ToolContent
imageContent cursor = ImageContent img
  where
    img = T.concat $ cursor $// element "img" >=> attribute "src"

textSpec :: ToolSpec
textSpec = ("text", textContent)

imageSpec :: ToolSpec
imageSpec = ("picture", imageContent)

-- | Get tools of a certain type
getTools :: ToolSpec -> Cursor Node -> [Tool]
getTools (toolTypeName, contentFun) cursor =
  L.zipWith4 Tool ids positions sizes toolContent
  where
    tools = cursor $// attributeIs "data-tool" toolTypeName
    ids = map (T.concat . attribute "data-id") tools
    styles = map (T.concat . attribute "style") tools
    (positions, sizes) = extractSizePos styles
    content =
      cursor $// attributeIs "data-tool" toolTypeName &/
      check (checkClass "tool-content")
    toolContent = map contentFun content

parseExposition :: Cursor Node -> Exposition
parseExposition cursor =
  Exposition
    { title = getMeta cursor "citation_title"
    , author = getMeta cursor "citation_author"
    , date = getMeta cursor "citation_publication_date"
    , tools = getTools textSpec cursor ++ getTools imageSpec cursor
    }

getExposition :: ImportOptions -> IO Exposition
getExposition options = do
  req <- parseRequest (url options)
  doc <- httpSink req $ const sinkDoc
  return $ parseExposition (fromDocument doc)

parseArgs :: [String] -> ImportOptions
parseArgs args = makeOptions args (ImportOptions False "")
  where
    makeOptions :: [String] -> ImportOptions -> ImportOptions
    makeOptions ("-m":t) options = makeOptions t (options {markdown = True})
    makeOptions (url:t) options = makeOptions t (options {url = url})
    makeOptions [] options = options

usage = putStrLn "Usage: parse-exposition [-m] rc-url"

exit = exitWith ExitSuccess

main :: IO ()
main = do
  args <- getArgs
  exp <- getExposition $ parseArgs args
  -- TODO deal with markdown conversion here
  TIO.putStrLn $ encodeTxt exp
