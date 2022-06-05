{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding (null, writeFile)

import Blizzard.Html (Attribute(..))
import Clay (Css)
import Clay.Render (compact, renderWith)
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<), forM_, forever, when)
import Data.ByteString.Lazy (toStrict)
import Data.Hash.Murmur (murmur3)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, pack, null)
import Data.Text.Lazy.IO (writeFile)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Haskell.Interpreter
import System.Directory.Recursive (getFilesRecursive)
import System.Environment
import System.Exit
import System.FSNotify
import Text.Regex.Posix ((=~), getAllTextMatches)

import qualified Data.HashTable.IO as Dict


type FileDict = Dict.BasicHashTable String [(Text, Text)]
type HashDict = Dict.BasicHashTable Text Text


main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        die "Error: Missing arguments. Please specify (1) a directory to watch and (2) the name of the output file."
    let sourceDir = head args
    let outputDir = args !! 1
    putStrLn $ "Watching " <> sourceDir <> " for changes..."
    fileDict <- Dict.new :: IO FileDict
    files <- getFilesRecursive sourceDir
    mapM_ (parseFile fileDict) $ filter (isSuffixOf ".hs") files
    writeCss fileDict outputDir
    withManager $ \mgr -> do
        watchTree
            mgr
            sourceDir
            (const True)
            (loadFile fileDict outputDir)
        forever $ threadDelay 1000000


loadFile :: FileDict -> String -> Action
loadFile fileDict outputDir = \case
    Added path _ _ -> do
        putStrLn $ "New file detected. Updating " <> outputDir <> "..."
        parseFile fileDict path
        writeCss fileDict outputDir
    Modified path _ _ -> do
        putStrLn $ "File change detected. Updating " <> outputDir <> "..."
        parseFile fileDict path
        writeCss fileDict outputDir
    Removed path _ _ -> do
        putStrLn $ "File deletion detected. Updating " <> outputDir <> "..."
        Dict.delete fileDict path
        writeCss fileDict outputDir
    Unknown {} -> pure ()


parseFile :: FileDict -> String -> IO ()
parseFile fileDict filePath = do
    fileContents <- readFile filePath
    parsedCss    <- mapM interpretProps $ parseCss fileContents
    updateDict fileDict filePath parsedCss


updateDict :: FileDict -> String -> [Text] -> IO ()
updateDict fileDict key value = do
    Dict.insert fileDict key
        $ zipCss
        . filter (not . null)
        $ value


writeCss :: FileDict -> String -> IO ()
writeCss fileDict outputDir = do
    outputText <- generateOutput fileDict
    writeFile outputDir outputText


generateOutput :: FileDict -> IO Text
generateOutput fileDict = do
    hashDict <- Dict.new :: IO HashDict
    Dict.mapM_ (updateHash hashDict) fileDict
    Dict.foldM concatHash "" hashDict


concatHash :: Text -> (Text, Text) -> IO Text
concatHash a (b, c) = pure $ a <> b <> c


updateHash :: HashDict -> (String, [(Text, Text)]) -> IO ()
updateHash hash (_, v) = forM_ v $ uncurry (Dict.insert hash)


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
interpretProps props = do
    css <- mapM (interpretCss <=< interpretProp) props
    pure
        $ renderWith compact []
        . mconcat
        . catMaybes
        $ css


interpretCss :: Either InterpreterError Attribute -> IO (Maybe Css)
interpretCss = \case
    Right (AttrCss css) -> pure $ Just css
    Right (AttrRaw _ _) -> pure   Nothing
    Left  _             -> pure   Nothing
