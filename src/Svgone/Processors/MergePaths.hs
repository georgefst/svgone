module Svgone.Processors.MergePaths (process) where

import Control.Lens
import Data.Either
import Data.Either.Extra
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Graphics.SvgTree
import Linear.V2

process :: Document -> Document
process = documentElements %~ map Tree . branches . map (^. treeBranch)

branches :: [TreeBranch] -> [TreeBranch]
branches bs = map (PathNode . fromPolygonPath) (concatMap mergePaths $ groupOn ppAttrs polygons) ++ nonPolygons
  where
    (nonPolygons, polygons) = partitionEithers $ map (\x -> maybeToEither x $ toPolygonPath =<< pathBranch x) bs

--TODO the actual processing
mergePaths :: [PolygonPath] -> [PolygonPath]
mergePaths = id

pathBranch :: TreeBranch -> Maybe Path
pathBranch = \case
    PathNode p -> Just p
    _ -> Nothing

data PolygonPath = PolygonPath
    { ppAttrs :: DrawAttributes
    , ppPath :: NonEmpty (V2 Double)
    }
    deriving (Eq, Show)

toPolygonPath :: Path -> Maybe PolygonPath
toPolygonPath (Path attrs pcs) = case pcs of
    MoveTo OriginAbsolute [v] : xs -> PolygonPath attrs . (v :|) <$> f v xs
    _ -> Nothing
  where
    f :: V2 Double -> [PathCommand] -> Maybe [V2 Double]
    f v0@(V2 x0 y0) = \case
        c : cs -> case c of
            LineTo OriginRelative [v] -> g $ v0 + v
            LineTo OriginAbsolute [v] -> g v
            HorizontalTo OriginAbsolute [x] -> g $ V2 x y0
            HorizontalTo OriginRelative [x] -> g $ V2 (x0 + x) y0
            VerticalTo OriginAbsolute [y] -> g $ V2 x0 y
            VerticalTo OriginRelative [y] -> g $ V2 x0 (y0 + y)
            EndPath -> Just []
            _ -> Nothing
          where
            g v = (v :) <$> f v cs
        [] -> Nothing -- should end with 'EndPath'

fromPolygonPath :: PolygonPath -> Path
fromPolygonPath (PolygonPath attrs (v0 :| p)) =
    Path attrs $
        [MoveTo OriginAbsolute [v0]]
            ++ map (LineTo OriginAbsolute . pure) p
            ++ [EndPath]
