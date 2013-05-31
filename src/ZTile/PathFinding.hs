{-# LANGUAGE RecordWildCards #-}

module ZTile.PathFinding where

import ZTile

type Weight = Int

-- Weighted coordinates. A negative weight value indicates that the coordinate
-- is impassable. The greater the positive weight, the greater the movement
-- penalty for this coordinate.
type WCoord = (Coord, Weight)
