{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import Data.Text.Lazy (Text)
import Language.Haskell.Interpreter
import System.FSNotify
import Text.Regex.PCRE ((=~))


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
    let styles = map (dropWhile (/= '[')) . concat $ (file =~ regex :: [[String]])
    forM_ styles $ \style -> do
        tmp <- interpretStyles style
        print tmp


interpretStyles :: String -> IO (Either InterpreterError (Maybe Text))
interpretStyles styles = runInterpreter $ do
    setImports
        [ "Prelude"
        , "Blizzard.Css"
        , "Data.Text.Lazy"
        ]
    interpret ("rawCss " <> styles) (as :: Maybe Text)


regex :: String
regex = "(?>\\s)+(?>h1|h2|h3|h4|h5|h6)(?>\\s)*(?>\\[){1}[^\\]]*(?>\\]){1}"
