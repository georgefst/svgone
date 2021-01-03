{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Monad
import Data.Bool
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Tuple.Extra
import Graphics.SvgTree
import Linear

--TODO upstream to svg-tree, extra, linear etc.

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

infixl 5 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 5 <<<$>>>
(<<<$>>>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>>>) = fmap . fmap . fmap

(++|) :: NonEmpty a -> [a] -> NonEmpty a
(++|) (x :| xs) ys = x :| xs ++ ys

pairAdjacent :: [a] -> [(a, a)]
pairAdjacent xs = zip xs $ tail xs

classifyOn :: Ord b => (a -> b) -> [a] -> [(b, NonEmpty a)]
classifyOn f = Map.toList . Map.fromListWith (<>) . map (f &&& pure)

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : ((x :) <<$>> select xs)

{- | Rotate and reflect. Same coordinates in same order.

>>> equivalentCycles $ 1 :| [2,3,4]
[ 1 :| [2, 3, 4, 5]
, 5 :| [4, 3, 2, 1]
, 2 :| [3, 4, 5, 1]
, 1 :| [5, 4, 3, 2]
, 3 :| [4, 5, 1, 2]
, 2 :| [1, 5, 4, 3]
, 4 :| [5, 1, 2, 3]
, 3 :| [2, 1, 5, 4]
, 5 :| [1, 2, 3, 4]
, 4 :| [3, 2, 1, 5]
]
-}
equivalentCycles :: NonEmpty a -> [NonEmpty a]
equivalentCycles = concatMap refls . rots
  where
    refls xs = [xs, NE.reverse xs]
    rots = go []
    go ys = \case
        x :| xs -> (x :| xs ++ ys) : maybe [] (go $ ys ++ [x]) (NE.nonEmpty xs)

-- | Based on wikipedia: https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
intersectLines :: (Eq a, Ord a, Fractional a, Show a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
intersectLines (V2 x1 y1, V2 x2 y2) (V2 x3 y3, V2 x4 y4) = do
    guard $ den /= 0 -- lines are not parallel or coincident
    guard $ t >= 0 && t <= 1 -- intersection is on first line segment
    guard $ u >= 0 && u <= 1 -- intersection is on second line segment
    pure $ V2 x y
  where
    dx12 = x1 - x2
    dx13 = x1 - x3
    dx34 = x3 - x4

    dy12 = y1 - y2
    dy13 = y1 - y3
    dy34 = y3 - y4

    den = dx12 * dy34 - dy12 * dx34

    u = (dy12 * dx13 - dx12 * dy13) / den
    t = (dx13 * dy34 - dy13 * dx34) / den

    x = x1 - t * dx12
    y = y1 - t * dy12

pathBranch :: TreeBranch -> Maybe Path
pathBranch = \case
    PathNode p -> Just p
    _ -> Nothing

deriving instance Ord Cap
deriving instance Ord DrawAttributes
deriving instance Ord ElementRef
deriving instance Ord FillRule
deriving instance Ord FontStyle
deriving instance Ord LineJoin
deriving instance Ord Number
deriving instance Ord TextAnchor
deriving instance Ord Texture
deriving instance Ord Transformation

nearZeroNumber :: Number -> Bool
nearZeroNumber = \case
    Num d -> nearZero d
    Px d -> nearZero d
    Em d -> nearZero d
    Percent d -> nearZero d
    Pc d -> nearZero d
    Mm d -> nearZero d
    Cm d -> nearZero d
    Point d -> nearZero d
    Inches d -> nearZero d
