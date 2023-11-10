module Svgone.Plugin.RemoveAttributes (P, PluginOptions (..)) where

import Control.Lens
import Graphics.SvgTree
import Linear.Epsilon
import Svgone.Plugin
import Util

data P
type Opts = PluginOptions P

instance Plugin P where
    data PluginOptions P = Opts
        { defaultAttributes :: Bool
        , -- | Remove all stroke attributes if the stroke isn't visible.
          invisiblePathStroke :: Bool
        }
    defaultOpts = Opts True True
    plugin :: Opts -> Document -> Document
    plugin Opts{..} =
        documentElements
            %~ map
                ( Tree
                    . ( \x ->
                            maybe
                                x
                                ( PathNode
                                    . over
                                        drawAttributes
                                        ( applyWhen invisiblePathStroke removeInvisibleStroke
                                            . applyWhen defaultAttributes removeDefaultAttributes
                                        )
                                )
                                $ pathBranch x
                      )
                    . (^. treeBranch)
                )
    pluginName = "remove-attributes"

removeDefaultAttributes :: HasDrawAttributes p => p -> p
removeDefaultAttributes attrs
    | Just x <- attrs ^. fillOpacity, nearZero $ abs $ x - 1 = attrs & fillOpacity .~ Nothing
    | otherwise = attrs

removeInvisibleStroke :: DrawAttributes -> DrawAttributes
removeInvisibleStroke attrs
    | Just x <- attrs ^. strokeWidth, nearZeroNumber x = remove
    | Just x <- attrs ^. strokeOpacity, nearZero x = remove
    | otherwise = attrs
  where
    remove =
        attrs
            & strokeWidth .~ Nothing
            & strokeColor .~ Nothing
            & strokeOpacity .~ Nothing
            & strokeLineCap .~ Nothing
            & strokeLineJoin .~ Nothing
            & strokeMiterLimit .~ Nothing
            & strokeOffset .~ Nothing
            & strokeDashArray .~ Nothing
