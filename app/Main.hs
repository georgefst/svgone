module Main (main) where

import Graphics.SvgTree (loadSvgFile, saveXmlFile)
import Svgone.Processors.CollapseGroups qualified as CollapseGroups
import Svgone.Processors.MergePaths qualified as MergePaths
import Svgone.Types
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
