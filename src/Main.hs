{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding (null, writeFile)

import Blizzard.Html (Attribute(..))
import Clay (Css)
import Clay.Render (compact, renderWith)
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<), forM_, forever)
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Hash.Murmur (murmur3)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, pack, null)
import Data.Text.Lazy.IO (writeFile)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.Interpreter
import System.FSNotify
import Text.Regex.Posix ((=~), getAllTextMatches)

import qualified Data.HashTable.IO as Dict


type Dict = Dict.BasicHashTable String [(Text, Text)]
type Hash = Dict.BasicHashTable Text Text


main :: IO ()
main =
    withManager $ \mgr -> do
        dict <- Dict.new :: IO Dict

        -- start a watching job (in the background)
        watchTree
            mgr           -- manager
            "./src"       -- directory to watch
            (const True)  -- predicate
            (loadFile dict) -- action

        -- sleep forever (until interrupted)
        forever $ threadDelay 1000000


loadFile :: Dict -> Action
loadFile dict = \case
    Added    path _ _ -> parseFile dict path
    Modified path _ _ -> parseFile dict path
    Removed  path _ _ -> pure ()
    Unknown  {}       -> pure ()


parseFile :: Dict -> String -> IO ()
parseFile dict path
     =  readFile path
    >>= mapM interpretProps . parseCss
    >>= updateDict dict path
     .  zipCss
     .  filter (not . null)


updateDict :: Dict -> String -> [(Text, Text)] -> IO ()
updateDict dict key value = do
    hash <- Dict.new :: IO Hash
    Dict.insert dict key value
    Dict.mapM_ (updateHash hash) dict
    Dict.foldM concatHash "" hash
        >>= writeFile "app.css"


concatHash :: Text -> (Text, Text) -> IO Text
concatHash a (b, c) =
    pure $ a <> b <> c


updateHash :: Hash -> (String, [(Text, Text)]) -> IO ()
updateHash hash (_, v) = do
    forM_ v $ \(k, v) -> do
        Dict.insert hash k v


hashCss :: Text -> Text
hashCss
    = pack
    . ('.' :)
    . ('_' :)
    . show
    . murmur3 15739
    . toStrict
    . encodeUtf8


zipCss :: [Text] -> [(Text, Text)]
zipCss = map (\x -> (hashCss x, x))


parseCss :: String -> [[String]]
parseCss
    = map extract
    . match


extract :: String -> [String]
extract
    = splitOn ","
    . tail
    . init
    . dropWhile (/= '[')


match :: String -> [String]
match
    = getAllTextMatches
    . flip (=~) regex


regex :: String
regex = "([^[:alnum:]_']|\n|^)(docType|docTypeHtml|a|abbr|address|area|article|aside|audio|b|base|bdo|blockquote|body|br|button|canvas|caption|cite|code|col|colgroup|command|datalist|dd|del|details|dfn|div|dl|dt|em|embed|fieldset|figcaption|figure|footer|form|h1|h2|h3|h4|h5|h6|head|header|hgroup|hr|html|i|iframe|img|input|ins|kbd|keygen|label|legend|li|link|main|map|mark|menu|menuitem|meta|meter|nav|noscript|object|ol|optgroup|option|output|p|param|pre|progress|q|rp|rt|ruby|samp|script|section|select|small|source|span|strong|style|sub|summary|sup|table|tbody|td|textarea|tfoot|th|thead|time|title|tr|track|u|ul|var|video|wbr)(\\s|\n)*\\[([^]]|\n)*\\]"


interpretProp :: String -> IO (Either InterpreterError Attribute)
interpretProp css = runInterpreter $ do
    set [ languageExtensions := [OverloadedStrings] ]
    setImports
        [ "Prelude"
        , "Blizzard.Css"
        , "Blizzard.Html"
        , "Data.Text.Lazy"
        ]
    interpret css (as :: Attribute)


interpretProps :: [String] -> IO Text
interpretProps props
     =  mapM (interpretCss <=< interpretProp) props
    <&> renderWith compact []
     .  mconcat
     .  catMaybes


interpretCss :: Either InterpreterError Attribute -> IO (Maybe Css)
interpretCss = \case
    Right (AttrCss css) -> pure $ Just css
    Right (AttrRaw _ _) -> pure   Nothing
    Left  _             -> pure   Nothing
