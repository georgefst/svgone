module Main (main) where

import Data.Text.IO qualified as T
import Svgone
import System.Environment

main :: IO ()
main =
    getArgs >>= \case
        [inF, outF] -> do
            in' <- T.readFile inF
            run allPluginsWithDefaults inF in' outF
        _ -> error "bad args"
