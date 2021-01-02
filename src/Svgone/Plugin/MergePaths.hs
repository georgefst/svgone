{-# OPTIONS_GHC -Wno-type-defaults #-}

module Svgone.Plugin.MergePaths (P, PluginOptions (..)) where

import Control.Lens
import Control.Monad
import Data.Either
import Data.Either.Extra
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Tuple
import GHC.Generics
import Graphics.SvgTree hiding (Text)
import Linear
import Svgone.Plugin
import Util

data P
type Opts = PluginOptions P

instance Plugin P where
    data PluginOptions P = Opts
        { -- | For floating-point equality.
          optsTolerance :: Double
        }
    defaultOpts = Opts 1
    plugin :: Opts -> Document -> Document
    plugin opts = documentElements %~ map Tree . branches opts . map (^. treeBranch)
    pluginName = "merge-paths"

branches :: Opts -> [TreeBranch] -> [TreeBranch]
branches opts bs = polygons' ++ nonPolygons
  where
    (nonPolygons, polygons) = partitionEithers $ map (\b -> maybeToEither b $ toPolygonPath =<< pathBranch b) bs
    polygons' = do
        (attrs, paths) <- snd <<<$>>> classifyOn fst polygons
        merged <- mergePaths opts $ NE.toList paths
        pure $ PathNode $ fromPolygonPath merged attrs

mergePaths :: Opts -> [PolygonPath] -> [PolygonPath]
mergePaths opts = \case
    [] -> []
    p : ps -> case mapMaybe (traverse (mergePaths2 opts p) . swap) $ select ps of
        -- p can't be merged with any shape in ps
        [] -> p : mergePaths opts ps
        -- p' is p merged with whichever path is missing from ps' - run again
        (ps', p') : _ -> mergePaths opts $ p' : ps'

mergePaths2 :: Opts -> PolygonPath -> PolygonPath -> Maybe PolygonPath
mergePaths2 Opts{..} us vs =
    listToMaybe . catMaybes $
        mergeOne
            <$> #unPolygonPath equivalentCycles us
            <*> #unPolygonPath equivalentCycles vs
  where
    a ~= b = distance a b < optsTolerance
    mergeOne (PolygonPath (u0 :| us0)) (PolygonPath (v0 :| vs0)) = do
        guard $ u0 ~= v0
        u1 : _us1 <- pure us0
        v1 : vs1 <- pure vs0

        -- we don't care about intersections at the points we're merging
        let here :: V2 Double -> Bool
            here w = w ~= u0 || w ~= u1

        let a = distance u0 u1
            b = distance u0 v1
            c = distance u1 v1
            d = ((a ^ 2) + (b ^ 2) - (c ^ 2)) / (2 * a)
            r = sqrt $ (b ^ 2) - (d ^ 2) -- distance from v1 to the closest point on (u0,u1)

        -- avoid exceptions in calculating r
        guard $ a /= 0 -- would mean 'us' contains adjacent duplicates
        -- d is one side of a triangle of which b is the hypotenuse:
        -- this can only fail if v1 is right on the line and we get rounding errors
        guard $ d <= b

        -- v1 is on the line
        guard $ r < optsTolerance

        -- the shapes have no other intersections
        guard $ all here $ catMaybes $ intersectLines <$> pairAdjacent (us0 ++ [u0]) <*> pairAdjacent (vs0 ++ [v0])

        pure $ PolygonPath $ u0 :| reverse us0 ++ [v1 | not $ u1 ~= v1] ++ vs1

newtype PolygonPath = PolygonPath {unPolygonPath :: NonEmpty (V2 Double)}
    deriving (Eq, Show, Generic)

toPolygonPath :: Path -> Maybe (DrawAttributes, PolygonPath)
toPolygonPath (Path attrs pcs) = case pcs of
    MoveTo OriginAbsolute [v] : xs -> (attrs,) . PolygonPath . (v :|) <$> f v xs
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

fromPolygonPath :: PolygonPath -> DrawAttributes -> Path
fromPolygonPath (PolygonPath (v0 :| p)) =
    flip Path $
        [MoveTo OriginAbsolute [v0]]
            ++ map (LineTo OriginAbsolute . pure) p
            ++ [EndPath]
