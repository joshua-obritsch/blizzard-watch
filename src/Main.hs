{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding (null)
import Blizzard.Html (Attribute(..))
import Clay.Render (compact, renderWith)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forever)
import Data.ByteString.Lazy (toStrict)
import Data.Hash.Murmur (murmur3)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, pack, null)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.Interpreter
import System.FSNotify
import Text.Regex.Posix ((=~), getAllTextMatches)


main :: IO ()
main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      "./src"      -- directory to watch
      (const True) -- predicate
      loadFile     -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000


loadFile :: Action
loadFile = \case
    Added    path _ _ -> parseFile path
    Modified path _ _ -> parseFile path
    Removed  path _ _ -> pure ()
    Unknown  {}       -> pure ()


parseFile :: String -> IO ()
parseFile path = do
    file <- readFile path
    css <- forM (parseCss file) $ \parsedCss -> do
        interpretProps parsedCss
    print $ zipCss . filter (not . null) $ css


hashedCss :: Text -> Text
hashedCss = pack . ('_' :) . show . murmur3 15739 . toStrict . encodeUtf8


zipCss :: [Text] -> [(Text, Text)]
zipCss = map (\x -> (hashedCss x, x))


parseCss :: String -> [[String]]
parseCss str = map (splitOn "," . firstLast . dropWhile (/= '[')) (getAllTextMatches (str =~ regex) :: [String])


firstLast:: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)


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
interpretProps props = do
    css <- forM props $ \prop -> do
        interpretProp prop
            >>= \case
                Left  _             -> pure Nothing
                Right (AttrCss css) -> pure $ Just css
                Right (AttrRaw _ _) -> pure Nothing
    pure $ renderWith compact [] . mconcat . catMaybes $ css
