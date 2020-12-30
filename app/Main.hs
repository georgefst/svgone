module Main (main) where

import Graphics.SvgTree
import Svgone.Processors.MergePaths
import System.Environment

main :: IO ()
main =
    getArgs >>= \case
        [inF, outF] ->
            loadSvgFile inF >>= \case
                Just in' -> do
                    let out = process in'
                    saveXmlFile outF out
                Nothing -> error "couldn't read input"
        _ -> error "bad args"
