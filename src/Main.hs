{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Text.Lazy (Text)
import Language.Haskell.Exts (parseFile)
import Language.Haskell.Interpreter
import System.FSNotify


main :: IO ()
main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      "./src"      -- directory to watch
      (const True) -- predicate
      tmp        -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000


tmp :: Action
tmp = \case
    Added    path _ _ -> do
        file <- parseFile path
        tmp <- runInterpreter $ do
            setImports
                [ "Prelude"
                , "Blizzard.Css"
                , "Data.Text.Lazy"
                ]
            interpret "rawCss [borderStyle solid]" (as :: Maybe Text)
        print tmp
    Modified path _ _ -> print path
    Removed  path _ _ -> print path
    Unknown  {}       -> pure ()
