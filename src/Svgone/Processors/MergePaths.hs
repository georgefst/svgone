module Svgone.Processors.MergePaths (P, PluginOptions (..)) where

import Control.Lens
import Control.Monad
import Data.Either
import Data.Either.Extra
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import GHC.Generics
import Graphics.SvgTree hiding (Text)
import Linear
import Svgone.Types
import Util

data P
type Opts = PluginOptions P

instance Plugin P where
    data PluginOptions P = Opts
        { -- | For floating-point equality.
          optsTolerance :: Double
        , -- | What to use when a pair can be merged in multiple ways.
          optsChoosePath :: NonEmpty PolygonPath -> PolygonPath
        }
    defaultOpts = Opts 1 NE.head
    plugin :: Opts -> Document -> Document
    plugin opts = documentElements %~ map Tree . branches opts . map (^. treeBranch)
    pluginName = "merge-paths"

branches :: Opts -> [TreeBranch] -> [TreeBranch]
branches opts bs = polygons' ++ nonPolygons
  where
    (nonPolygons, polygons) = partitionEithers $ map (\b -> maybeToEither b $ toPolygonPath =<< pathBranch b) bs
    polygons' = do
        (attrs, paths) <- map snd . NE.toList <<$>> classifyOn fst polygons
        merged <- mergePaths opts paths
        pure $ PathNode $ fromPolygonPath merged attrs

--TODO check all pairs against each other - not just ones adjacent in the input list
mergePaths :: Opts -> [PolygonPath] -> [PolygonPath]
mergePaths opts = \case
    x : y : zs -> case mergePaths2 opts x y of
        Just p -> mergePaths opts $ p : zs
        Nothing -> x : mergePaths opts (y : zs)
    xs -> xs

mergePaths2 :: Opts -> PolygonPath -> PolygonPath -> Maybe PolygonPath
mergePaths2 Opts{..} us vs =
    fmap optsChoosePath . NE.nonEmpty . catMaybes $
        merge
            <$> #unPolygonPath equivalentCycles us
            <*> #unPolygonPath equivalentCycles vs
  where
    a ~= b = distance a b < optsTolerance
    merge (PolygonPath (u0 :| us0)) (PolygonPath (v0 :| vs0)) = do
        guard $ u0 ~= v0
        u1 : _us1 <- pure us0
        v1 : vs1 <- pure vs0
        let here :: V2 Double -> Bool -- we don't care about intersections at the points we're merging
            here w = w ~= u0 || w ~= u1
        guard $ u1 ~= v1 --TODO should just be checking if the points lie on the same line
        guard $ all here $ catMaybes $ intersectLines <$> pairAdjacent (us0 ++ [u0]) <*> pairAdjacent (vs0 ++ [v0])
        Just . PolygonPath $ u0 :| reverse us0 ++ vs1

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
