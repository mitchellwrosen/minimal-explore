module GameLogic.GridZipper
    ( GridZipper(..)
    , PlaneZipper(..)
    , LineZipper(..)
    , bounded
    , gridLeft
    , gridRight
    , gridUp
    , gridDown
    , gridIn
    , gridOut
    , gridFocus
    , gridToList
    , gridPlaneToList
    ) where

import Data.Maybe (containsNothing, fromJust, fromMaybe, isNothing)

-- left, focus, right
data LineZipper a = LineZipper [a] a [a] deriving (Eq, Show)

-- up, focus, down
data PlaneZipper a = PlaneZipper [LineZipper a] (LineZipper a) [LineZipper a] deriving (Eq, Show)

-- in, focus, out
data GridZipper a = GridZipper [PlaneZipper a] (PlaneZipper a) [PlaneZipper a] deriving (Eq, Show)

gridLeft, gridRight, gridUp, gridDown, gridIn, gridOut :: GridZipper a -> Maybe (GridZipper a)
gridLeft  = gridTranspose planeLeft
gridRight = gridTranspose planeRight
gridUp    = gridTranspose planeUp
gridDown  = gridTranspose planeDown

gridTranspose f (GridZipper xs a ys) =
    case (containsNothing xs', isNothing a', containsNothing ys') of
        (False, False, False) -> Just $ GridZipper (map fromJust xs') (fromJust a') (map fromJust ys')
        _ -> Nothing
  where
    xs' = map f xs
    a'  = f a
    ys' = map f ys

gridIn (GridZipper (x:xs) a ys) = Just $ GridZipper xs x (a:ys)
gridIn _ = Nothing
gridOut (GridZipper xs a (y:ys)) = Just $ GridZipper (a:xs) y ys
gridOut _ = Nothing

gridFocus :: GridZipper a -> a
gridFocus (GridZipper _ plane _) = planeFocus plane

gridToList :: GridZipper a -> [[[a]]]
gridToList (GridZipper xs a ys) = reverse (map planeToList xs) ++ [planeToList a] ++ map planeToList ys

gridPlaneToList :: GridZipper a -> [[a]]
gridPlaneToList (GridZipper _ plane _) = planeToList plane

-- Move along the grid, bound by the borders. Returns the original zipper if a boundary is crossed.
bounded :: (GridZipper a -> Maybe (GridZipper a)) -> GridZipper a -> GridZipper a
bounded f grid = fromMaybe grid (f grid)

planeLeft, planeRight, planeUp, planeDown :: PlaneZipper a -> Maybe (PlaneZipper a)
planeLeft  = planeTranspose lineLeft
planeRight = planeTranspose lineRight

planeTranspose :: (LineZipper a -> Maybe (LineZipper a)) -> PlaneZipper a -> Maybe (PlaneZipper a)
planeTranspose f (PlaneZipper xs a ys) =
    case (containsNothing xs', isNothing a', containsNothing ys') of
        (False, False, False) -> Just $ PlaneZipper (map fromJust xs') (fromJust a') (map fromJust ys')
        _ -> Nothing
  where
    xs' = map f xs
    a'  = f a
    ys' = map f ys

planeUp (PlaneZipper (x:xs) a ys) = Just $ PlaneZipper xs x (a:ys)
planeUp _ = Nothing
planeDown (PlaneZipper xs a (y:ys)) = Just $ PlaneZipper (a:xs) y ys
planeDown _ = Nothing

planeFocus :: PlaneZipper a -> a
planeFocus (PlaneZipper _ line _) = lineFocus line

planeToList :: PlaneZipper a -> [[a]]
planeToList (PlaneZipper xs a ys) = reverse (map lineToList xs) ++ [lineToList a] ++ map lineToList ys

lineLeft, lineRight :: LineZipper a -> Maybe (LineZipper a)
lineLeft  (LineZipper (x:xs) a ys) = Just $ LineZipper xs x (a:ys)
lineLeft  _ = Nothing
lineRight (LineZipper xs a (y:ys)) = Just $ LineZipper (a:xs) y ys
lineRight _ = Nothing

lineFocus :: LineZipper a -> a
lineFocus (LineZipper _ a _) = a

lineToList :: LineZipper a -> [a]
lineToList (LineZipper xs a ys) = reverse xs ++ [a] ++ ys

