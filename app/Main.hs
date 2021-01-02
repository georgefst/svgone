module Main (main) where

import Graphics.SvgTree (loadSvgFile, saveXmlFile)
import Svgone.Plugin
import Svgone.Plugin.CollapseGroups qualified as CollapseGroups
import Svgone.Plugin.MergePaths qualified as MergePaths
import System.Environment (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        [inF, outF] ->
            loadSvgFile inF >>= \case
                Just in' ->
                    saveXmlFile outF
                        . plugin @MergePaths.P defaultOpts
                        . plugin @CollapseGroups.P defaultOpts
                        $ in'
                Nothing -> error "couldn't read input"
        _ -> error "bad args"
