module Svgone.Plugin.CollapseGroups where

import Control.Lens
import Control.Monad
import Data.Maybe
import Graphics.SvgTree
import Svgone.Plugin

data P
type Opts = PluginOptions P

instance Plugin P where
    data PluginOptions P = Opts
    defaultOpts = Opts
    plugin :: Opts -> Document -> Document
    plugin Opts = documentElements %~ trees
    pluginName = "collapse-groups"

trees :: [Tree] -> [Tree]
trees = map $ branch . (^. treeBranch)

branch :: TreeBranch -> Tree
branch t = case t of
    GroupNode g -> fromMaybe (Tree $ GroupNode $ g & groupChildren %~ trees) do
        [Tree (PathNode p)] <- pure $ g ^. groupChildren
        guard $ isNothing $ g ^. groupViewBox
        guard $ g ^. groupAspectRatio == defaultSvg
        pure $ Tree $ PathNode p & drawAttributes %~ ((g ^. groupDrawAttributes) <>)
    x -> Tree x
