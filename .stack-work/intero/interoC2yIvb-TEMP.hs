{-# LANGUAGE DeriveGeneric #-}
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
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.HTML.DOM (sinkDoc)
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
data ToolContent =
  TextContent T.Text
  deriving (Generic, Show)

instance ToJSON (ToolContent)

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

-- |Get text tools
-- TODO: needs to be generalized a bit for all tool types
getTextTools :: Cursor Node -> [Tool]
getTextTools cursor = L.zipWith4 Tool ids positions sizes textContent
  where
    tools = cursor $// check (checkClass "tool-text")
    ids = map (T.concat . attribute "data-id") tools
    styles = map (T.concat . attribute "style") tools
    (positions, sizes) = extractSizePos styles
    content =
      cursor $// check (checkClass "tool-text") &/
      check (checkClass "tool-content")
    textContent =
      map (TextContent . toStrict . renderHtml . toHtml . node) content

parseExposition :: Cursor Node -> Exposition
parseExposition cursor =
  Exposition
    { title = getMeta cursor "citation_title"
    , author = getMeta cursor "citation_author"
    , date = getMeta cursor "citation_publication_date"
    , tools = getTextTools cursor
    }

getExposition :: String -> IO Exposition
getExposition url = do
  req <- parseRequest url
  doc <- httpSink req $ const sinkDoc
  return (parseExposition (fromDocument doc))

-- main :: IO ()
-- main = do
--   exposition <-
--     getExposition "https://www.researchcatalogue.net/view/343349/343350"
--   TIO.putStrLn $ encodeTxt exposition
main :: IO ()
main = do
  url <- getArgs
  exp <- getExposition $ head url
  TIO.putStrLn $ encodeTxt exp
-- main = getArgs >>= parse >>= putStr . tac
    -- tac  = unlines . reverse . lines
    -- parse ["-h"] = usage   >> exit
    -- parse ["-v"] = version >> exit
    -- parse []     = getContents
    -- parse fs     = concat `fmap` mapM readFile fs
    -- usage   = putStrLn "Usage: tac [-vh] [file ..]"
    -- version = putStrLn "Haskell tac 0.1"
    -- exit    = exitWith ExitSuccess
    -- die     = exitWith (ExitFailure 1)
    -- "https://www.researchcatalogue.net/view/343349/343350"
