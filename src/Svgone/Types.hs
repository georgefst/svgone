{-# LANGUAGE AllowAmbiguousTypes #-}

module Svgone.Types where

import Data.Text (Text)
import Graphics.SvgTree (Document)

class Plugin a where
    data PluginOptions a
    defaultOpts :: PluginOptions a
    plugin :: PluginOptions a -> Document -> Document
    pluginName :: Text
