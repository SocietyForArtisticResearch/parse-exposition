{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as Map
import System.FilePath.Posix

import qualified Data.ByteString.Lazy as ByteString
import Data.Char (isSpace, toLower)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Binary as CB
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Debug.Trace
import qualified Debug.Trace as Debug
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

data Exposition =
  Exposition
    { expositionToc :: [(T.Text, T.Text)]
    , expositionId :: T.Text
    , expositionMetaData :: ExpositionMetaData
    , expositionWeaves :: [Weave]
    }
  deriving (Generic, Show)

instance ToJSON Exposition

data Weave =
  Weave
    { weaveTitle :: T.Text
    , weaveUrl :: T.Text
    , weaveTools :: [Tool]
    , weavePopovers :: [Popover]
    }
  deriving (Generic, Show)

instance ToJSON Weave

data Popover =
  Popover
    { popoverId :: T.Text
  -- , popoverPosition :: Position
  -- , popoverSize     :: Size
    , popoverContent :: Weave
    }
  deriving (Generic, Show)

instance ToJSON Popover

data Position =
  Position
    { left :: Maybe Int
    , top :: Maybe Int
    }
  deriving (Generic, Show, Eq)

instance ToJSON Position

data Size =
  Size
    { width :: Maybe Int
    , height :: Maybe Int
    }
  deriving (Generic, Show, Eq)

instance ToJSON Size

data Tool =
  Tool
      -- toolMediaFile :: Maybe String
    { toolId :: T.Text
    , position :: Position
    , size :: Size
    , toolContent :: ToolContent
    }
  deriving (Generic, Show, Eq)

instance ToJSON Tool

instance Ord Tool where
  compare t1 t2 =
    let p1 = position t1
        p2 = position t2
     in case (left p1, top p1, left p2, top p2) of
          (Just l1, Just t1, Just l2, Just t2)
            | abs (l1 - l2) < 50 -> compare t1 t2
          (Nothing, Nothing, Nothing, Nothing) -> EQ
          (Nothing, Nothing, _, _) -> GT
          (_, _, Nothing, Nothing) -> LT
          (Nothing, Just t1, Nothing, Just t2) -> compare t1 t2
          (Just l1, _, Just l2, _) -> compare l1 l2
          _ -> EQ

--            | l1 == l2 -> compare t1 t2
-- x x
-- x x
data Resource =
  Resource
    { resourceUrl :: T.Text
    , localFile :: Maybe T.Text
    }
  deriving (Generic, Show, Eq)

instance ToJSON Resource

resourceEmpty :: Resource -> Bool
resourceEmpty = T.null . resourceUrl

toolEmpty :: Tool -> Bool
toolEmpty t =
  case toolContent t of
    TextContent t -> T.null t
    ImageContent r -> resourceEmpty r
    VideoContent r p -> resourceEmpty r || resourceEmpty p
    AudioContent r -> resourceEmpty r

-- | Content types to be extended
data ToolContent
  = TextContent
      { textToolContent :: T.Text
      }
  | ImageContent
      { imageUrl :: Resource
      }
  | VideoContent
      { videoUrl :: Resource
      , videoPreview :: Resource
      }
  | AudioContent
      { audioUrl :: Resource
      }
  deriving (Generic, Show, Eq)

instance ToJSON ToolContent

urlResource :: Tool -> Maybe Resource
urlResource t =
  case toolContent t of
    ImageContent u -> Just u
    VideoContent u _ -> Just u
    AudioContent u -> Just u
    _ -> Nothing

urlResourceImages :: Tool -> Maybe Resource
urlResourceImages t =
  case toolContent t of
    ImageContent u -> Just u
    _ -> Nothing

previewResource :: Tool -> Maybe Resource
previewResource t =
  case toolContent t of
    VideoContent _ p -> Just p
    _ -> Nothing

data ImportOptions =
  ImportOptions
    { markdown :: Bool -- convert content of text tools
    , writeMarkdown :: Bool -- for output
    , epub :: Bool
    , latex :: Bool
    , expId :: String
    , download :: Maybe String
    , host :: String
    }

type ToolTypeName = T.Text

type ToolSpec = (ToolTypeName, Cursor Node -> ToolContent)

data ExpositionMetaData =
  ExpoMetaData
    { metaTitle :: T.Text
    , metaDate :: T.Text
    , metaAuthors :: [T.Text]
    , metaKeywords :: [T.Text]
    , metaExpMainUrl :: T.Text
    }
  deriving (Generic, Show)

instance ToJSON ExpositionMetaData

-- | Encodes an object as strict json text
encodeTxt :: ToJSON a => a -> T.Text
encodeTxt a = toStrict . decodeUtf8 $ encodePretty a

-- | Checks if class attribute string contains a certain class
checkClass :: T.Text -> Cursor Node -> Bool
checkClass cls cursor =
  case node cursor of
    (NodeElement (Element _ attr _)) ->
      case M.lookup "class" attr of
        Just classes -> T.isInfixOf cls classes
        _ -> False
    _ -> False

-- | Get style property from a list of lists
styleProperty :: T.Text -> [[T.Text]] -> Maybe Int
styleProperty prop propLst =
  case L.find (\propPair -> head propPair == prop) propLst of
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
    assocStyles = map (map (T.splitOn ":") . T.splitOn ";") styles
    lefts = map (styleProperty "left") assocStyles
    tops = map (styleProperty "top") assocStyles
    positions = zipWith Position lefts tops
    widths = map (styleProperty "width") assocStyles
    heights = map (styleProperty "height") assocStyles
    sizes = zipWith Size widths heights

--------------------
--- Specification of tool functions
--------------------
textContent :: Cursor Node -> ToolContent
textContent cursor =
  TextContent $ (toStrict . renderHtml . toHtml . node) cursor

imageContent :: Cursor Node -> ToolContent
imageContent cursor = ImageContent (Resource img Nothing)
  where
    img = T.concat $ cursor $// element "img" >=> attribute "src"

videoContent :: Cursor Node -> ToolContent
videoContent cursor =
  VideoContent (Resource video Nothing) (Resource preview Nothing)
  where
    video = T.concat $ cursor $// attribute "data-file"
    preview = T.concat $ cursor $// attribute "data-image"

audioContent :: Cursor Node -> ToolContent
audioContent cursor = AudioContent (Resource audio Nothing)
  where
    audio = T.concat $ cursor $// attribute "data-file"
    -- lstinfo = T.pack $ show $cursor
    -- audio = T.concat $ cursor $// element "video" >=> attribute "src"

toolSpecs :: [ToolSpec]
toolSpecs =
  [ ("text", textContent)
  , ("simpletext", textContent)
  , ("picture", imageContent)
  , ("video", videoContent)
  , ("audio", audioContent)
  ]

-- | Get tools of a certain type
getTools :: ToolSpec -> Cursor Node -> [Tool]
getTools (toolTypeName, contentFun) cursor
  -- Debug.trace
  --   (show toolTypeName ++ show ids ++ show styles)
 = L.zipWith4 Tool ids positions sizes toolContent
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
  req <- parseRequest url
  runConduitRes $ httpSource req getResponseBody .| CB.sinkFile fname

-- toolUrl :: Tool -> [String]
-- toolUrl tool =
--   case toolContent tool of
--     ImageContent url -> [T.unpack (resourceUrl url)]
--     VideoContent url pv ->
--       [T.unpack (resourceUrl url), T.unpack (resourceUrl pv)]
--     AudioContent url -> [T.unpack (resourceUrl url)]
--     _ -> []
resourceFileExtension :: Resource -> String
resourceFileExtension r =
  takeWhile (/= '?') (takeExtension (T.unpack (resourceUrl r)))

resourceFileName :: Tool -> String -> String -> Resource -> String
resourceFileName tool dir suffix r =
  dir </> T.unpack (toolId tool) ++ suffix ++ (resourceFileExtension r)

setResourceFileName :: Tool -> String -> String -> Resource -> Resource
setResourceFileName tool dir suffix r =
  r {localFile = Just $ T.pack $ resourceFileName tool dir suffix r}

resourceToUrlAndFilename :: Resource -> Maybe (String, String)
resourceToUrlAndFilename r =
  case localFile r of
    Just f -> Just (T.unpack $ resourceUrl r, T.unpack f)
    Nothing -> Nothing

downloadTool :: String -> Bool -> Tool -> IO ()
downloadTool dir preview tool =
  let resourcesWithNames =
        if preview
          then catMaybes
                 [ fmap
                     (setResourceFileName tool dir "")
                     (urlResourceImages tool)
                 , fmap
                     (setResourceFileName tool dir "_preview")
                     (previewResource tool)
                 ]
          else catMaybes
                 [ fmap (setResourceFileName tool dir "") (urlResource tool)
                 , fmap
                     (setResourceFileName tool dir "_preview")
                     (previewResource tool)
                 ]
      resources = catMaybes $ fmap resourceToUrlAndFilename $ resourcesWithNames
   in mapM_ (\(url, fname) -> downloadFile url fname) resources
    -- case (fileName, urls) of
    --     ([fnameStr], [urlStr]) -> downloadFile urlStr fnameStr
    --     ([fnameStr, prevName], [urlStr, prevUrl]) -> do
    --       downloadFile urlStr fnameStr
    --       downloadFile prevUrl prevName
    --     _ -> return ()

downloadTools :: ImportOptions -> Exposition -> IO ()
downloadTools options exposition =
  let tools = concatMap weaveTools (expositionWeaves exposition)
   in case download options of
        Nothing -> return ()
        Just dir -> do
          exists <- doesDirectoryExist dir
          unless exists $ createDirectory dir
          mapM_ (downloadTool dir (latex options)) tools

--------------------
--- Popovers
--------------------
popoverUrl :: String -> String -> String
popoverUrl expoId popId = "/view/popover/" ++ expoId ++ "/" ++ popId

getPopovers :: String -> String -> Cursor Node -> IO [Popover]
getPopovers host expoId cursor = L.zipWith Popover ids <$> content
  where
    ids = cursor $// element "a" >=> attribute "data-popover"
    content =
      traverse
        (\u ->
           getWeave
             host
             expoId
             ("popover", (T.pack (popoverUrl expoId (T.unpack u)))))
        ids

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

textToolsToMarkdown :: Pan.PandocMonad m => Exposition -> m Exposition
textToolsToMarkdown exp = do
  mdTools <- mapM (mapM toolToMarkdown) (map weaveTools (expositionWeaves exp))
  return $
    exp
      { expositionWeaves =
          zipWith (\w t -> w {weaveTools = t}) (expositionWeaves exp) mdTools
      }

--------------------
--- Adding Metadata
--------------------
addMetaString :: Pan.Pandoc -> String -> String -> Pan.Pandoc
addMetaString (Pan.Pandoc (Pan.Meta meta) blocks) key value =
  Pan.Pandoc newMeta blocks
  where
    newMeta = Pan.Meta $ Map.insert key (Pan.MetaString value) meta

addMetaStringList :: Pan.Pandoc -> String -> [String] -> Pan.Pandoc
addMetaStringList (Pan.Pandoc (Pan.Meta meta) blocks) key values =
  Pan.Pandoc newMeta blocks
  where
    newMeta =
      Pan.Meta $ Map.insert key (Pan.MetaList $ map Pan.MetaString values) meta

--------------------
--- EPub3 conversion
--------------------
mdReaderOptions =
  (PanOptions.def
     { PanOptions.readerExtensions =
         PanOptions.extensionsFromList [PanOptions.Ext_multiline_tables]
     })

mdWriterOptions =
  (PanOptions.def
     { PanOptions.writerExtensions =
         PanOptions.extensionsFromList [PanOptions.Ext_multiline_tables]
     })

extractLocalFile :: Resource -> T.Text
extractLocalFile r = fromMaybe "" $ localFile r

extractLocalImageFile :: Resource -> T.Text
extractLocalImageFile r =
  let file = T.unpack $ fromMaybe "" $ localFile r
   in if (fmap toLower (takeExtension file)) == ".gif"
        then T.pack ((takeDirectory file) </> (takeBaseName file ++ "-0.png"))
        else if (fmap toLower (takeExtension file)) == ".svg"
        then T.pack ((takeDirectory file) </> (takeBaseName file ++ ".png"))
        else T.pack file

toolToMd :: Pan.PandocMonad m => Tool -> m Pan.Pandoc
toolToMd tool =
  case toolContent tool of
    (TextContent txt) -> Pan.readHtml mdReaderOptions txt
    (ImageContent img) ->
      Pan.readMarkdown
        PanOptions.def
        ("\n![](" <> (extractLocalImageFile img) <> ")\n")
    (AudioContent url) ->
      Pan.readMarkdown
        PanOptions.def
        ("\nAudio tool with id " <> toolId tool <> "\n")
    (VideoContent url preview) ->
      Pan.readMarkdown
        (PanOptions.def
           { PanOptions.readerExtensions =
               (PanOptions.enableExtension
                  PanOptions.Ext_implicit_figures
                  (PanOptions.getDefaultExtensions "latex"))
           })
        ("\n![" <>
         "Video tool with id " <>
         toolId tool <> "](" <> (extractLocalFile preview) <> ")\n")

--      Pan.readMarkdown PanOptions.def ("\n![" <> url <> "](" <> url <> ")\n")
--      Pan.readMarkdown PanOptions.def ("\n![" <> url <> "](" <> url <> ")\n")
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

expToEPub ::
     Pan.PandocMonad m
  => Exposition
  -> ExpositionMetaData
  -> m ByteString.ByteString
expToEPub exp meta = do
  let sortedTools = concat $ map (L.sort . weaveTools) (expositionWeaves exp)
  pan <- fmap mconcat $ traverse toolToMd sortedTools
  template <- Pan.getDefaultTemplate "epub"
  let year = lastN 4 (T.unpack $ metaDate meta)
  Pan.writeEPUB3
    (PanOptions.def
       { Pan.writerVariables = [("coverpage", "true"), ("titlepage", "true")]
       , Pan.writerTemplate = Just template
       })
    (addMetaString
       (addMetaStringList
          (addMetaString pan "title" (T.unpack $ metaTitle meta))
          "creator"
          (map T.unpack $ metaAuthors meta))
       "date"
       year)

--        map toolContent $
expToLaTeX :: Pan.PandocMonad m => Exposition -> ExpositionMetaData -> m T.Text
--  -> m ByteString.ByteString
expToLaTeX exp meta = do
  let sortedTools
  --      map toolContent $
       = concat $ map (L.sort . weaveTools) (expositionWeaves exp)
  pan <- fmap mconcat $ traverse toolToMd sortedTools
  template <- Pan.getDefaultTemplate "latex"
  let year = lastN 4 (T.unpack $ metaDate meta)
  Pan.writeLaTeX
    (PanOptions.def
       { Pan.writerVariables = [("coverpage", "true"), ("titlepage", "true")]
       , Pan.writerTemplate = Just template
       })
    (addMetaString
       (addMetaStringList
          (addMetaString pan "title" (T.unpack $ metaTitle meta))
          "author"
          (map T.unpack $ metaAuthors meta))
       "date"
       year)

--------------------
--- Markdown conversion
--------------------
mkYamlHeader :: ExpositionMetaData -> T.Text
mkYamlHeader meta =
  T.unlines $
  [ "---"
  , "title:"
  , "- type: main"
  , "  text: \"" <> metaTitle meta <> "\""
  , "author:"
  ] ++
  authors ++ ["..."]
  where
    authors =
      mconcat $
      map
        (\author -> ["- role: author", "  text: \"" <> author <> "\""])
        (metaAuthors meta)

expToMarkdown ::
     Pan.PandocMonad m => Exposition -> ExpositionMetaData -> m T.Text
expToMarkdown exp meta = do
  let sortedTools = concat $ map (L.sort . weaveTools) (expositionWeaves exp)
  pan <- fmap mconcat $ traverse toolToMd sortedTools
  template <- Pan.getDefaultTemplate "markdown"
  let year = lastN 4 (T.unpack $ metaDate meta)
  Pan.writeMarkdown
    mdWriterOptions
    (addMetaString
       (addMetaStringList
          (addMetaString pan "title" (T.unpack $ metaTitle meta))
          "creator"
          (map T.unpack $ metaAuthors meta))
       "date"
       year)

--        map toolContent $
--------------------
--- Main functions
--------------------
insertToolFileName :: String -> Tool -> Tool
insertToolFileName dir t = t {toolContent = newContent}
  where
    newContent =
      case toolContent t of
        VideoContent url prev ->
          VideoContent
            (setResourceFileName t dir "" url)
            (setResourceFileName t dir "_preview" prev)
        ImageContent url -> ImageContent (setResourceFileName t dir "" url)
        AudioContent url -> AudioContent (setResourceFileName t dir "" url)
        c -> c

insertFileNames :: ImportOptions -> Exposition -> Exposition
insertFileNames options exp =
  let dir = ""
        -- case download options of
        --   Nothing -> "media/"
        --   Just dir -> dir
   in let toolsFname =
            map
              (map (insertToolFileName dir))
              (map weaveTools (expositionWeaves exp))
       in exp
            { expositionWeaves =
                zipWith
                  (\w t -> w {weaveTools = t})
                  (expositionWeaves exp)
                  toolsFname
            }

getWeave :: String -> String -> (T.Text, T.Text) -> IO Weave
getWeave host id (title, url) = do
  req <- parseRequest $ host <> T.unpack url
  doc <- httpSink req $ const sinkDoc
  let cursor = fromDocument doc
  let tools =
        L.filter (not . toolEmpty) $
        concatMap (\spec -> getTools spec cursor) toolSpecs
  popovers <- getPopovers host id cursor
  let w =
        Weave
          { weaveTitle = title
          , weaveUrl = url
          , weaveTools = tools
          , weavePopovers = popovers
          }
  return $ w

defaultWeave :: String -> (T.Text, T.Text)
defaultWeave id =
  let idN = read id :: Integer
   in ("default", T.pack ("/view/" ++ id ++ "/" ++ (show (idN + 1))))

parseExposition :: String -> String -> ExpositionMetaData -> Cursor Node -> IO Exposition
parseExposition host id metadata cursor = do
  let expToc =
        case toc cursor of
          [] -> [defaultWeave id]
          tocContent -> tocContent
  weaves <- mapM (getWeave host id) expToc
  return $ Exposition expToc (T.pack id) metadata weaves

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
  exp <- parseExposition (host options) (expId options) metadata (fromDocument doc)
  return $ insertFileNames options exp

detailsUrl :: String -> String -> String
detailsUrl host expId = host <> "/profile/show-exposition?exposition=" <> expId

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
  req <- parseRequest $ detailsUrl (host options) (expId options)
  doc <- httpSink req $ const sinkDoc
  return $ parseDetailsPage (fromDocument doc)

parseArgs :: [String] -> ImportOptions
parseArgs args =
  makeOptions
    args
    (ImportOptions
       False
       False
       False
       False
       ""
       Nothing
       "https://www.researchcatalogue.net")
  where
    makeOptions :: [String] -> ImportOptions -> ImportOptions
    makeOptions ("-epub":t) options = makeOptions t (options {epub = True})
    makeOptions ("-md":t) options =
      makeOptions t (options {writeMarkdown = True})
    makeOptions ("-latex":t) options = makeOptions t (options {latex = True})
    makeOptions ("-textmd":t) options =
      makeOptions t (options {markdown = True})
    makeOptions ("-d":dir:t) options =
      makeOptions t (options {download = Just dir})
    makeOptions ("-host":h:t) options = makeOptions t (options {host = h})
    makeOptions (id:t) options = makeOptions t (options {expId = id})
    makeOptions [] options = options

usage =
  putStrLn
    "Usage: parse-exposition [-epub] [-textmd] [-md] [-d] [-h <host>] exposition-id"

exit = exitSuccess

expToTxt :: Pan.PandocMonad m => m Exposition -> m T.Text
expToTxt exp = encodeTxt <$> exp

encodeMdTxt :: Exposition -> IO T.Text
encodeMdTxt exp = Pan.runIOorExplode $ expToTxt (textToolsToMarkdown exp)

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs args
  details <- getDetailsPageData options
  exp <- getExposition options details
  downloadTools options exp
  -- if (markdown options)
  --   then TIO.putStrLn =<< encodeMdTxt exp
  --   else TIO.putStrLn $ encodeTxt exp
  let fname = fromMaybe "" (download options) <> "export"
  if (epub options)
    then do
      epubBs <- Pan.runIOorExplode $ expToEPub exp details
      ByteString.writeFile (fname <> ".epub") epubBs
    else return ()
  if (latex options)
    then do
      latexExp <- Pan.runIOorExplode $ expToLaTeX exp details
      TIO.writeFile (fname <> ".tex") latexExp
    else return ()
  if (writeMarkdown options)
    then do
      md <- Pan.runIOorExplode $ expToMarkdown exp details
      TIO.writeFile (fname <> ".md") (mkYamlHeader details <> md)
    else return ()
