module Svgone where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Graphics.SvgTree (Document, parseSvgFile, saveXmlFile)
import Svgone.Plugin
import qualified Svgone.Plugin.CollapseGroups as CollapseGroups
import qualified Svgone.Plugin.MergePaths as MergePaths
import qualified Svgone.Plugin.RemoveAttributes as RemoveAttributes

data SomePlugin where
    SomePlugin :: Plugin a => PluginOptions a -> SomePlugin

runFile ::
    -- | Operations to perform, left to right.
    [SomePlugin] ->
    -- | Input file.
    FilePath ->
    -- | Output file
    FilePath ->
    IO ()
runFile ps in' out = do
    t <- T.readFile in'
    run ps in' t out

run ::
    -- | Operations to perform, left to right.
    [SomePlugin] ->
    -- | Source path/URL of the document, used to resolve relative links.
    FilePath ->
    -- | Contents
    Text ->
    -- | Output file
    FilePath ->
    IO ()
run ps in' t out = do
    Just d <- pure $ parseSvgFile in' t
    let d' = runDoc ps d
    saveXmlFile out d'

runDoc :: [SomePlugin] -> Document -> Document
runDoc = flip foldr id \(SomePlugin opts) -> (. plugin opts)

allPluginsWithDefaults :: [SomePlugin]
allPluginsWithDefaults =
    [ SomePlugin $ defaultOpts @CollapseGroups.P
    , SomePlugin $ defaultOpts @RemoveAttributes.P
    , SomePlugin $ defaultOpts @MergePaths.P
    ]
