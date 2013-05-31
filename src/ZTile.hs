{-# LANGUAGE RecordWildCards #-}

module ZTile where

import Data.List

-- Coord is (X, Y); this can represent both square and hex tiles; for hex tiles,
-- the third axis, Z, can be calculated on the fly using the X and Y values.
type Coord = (Int, Int)

data Direction
	= DXPlus
	| DXMinus
	| DYPlus
	| DYMinus
	deriving (Eq, Show)

class ZTile a where
	indices :: a -> [Coord]
	adjacent :: a -> Coord -> [Coord] -- adjacent edge-to-edge adjacent
	-- edge/point-to-edge/point; for square tiles, this routine check for the
	-- diagonal directions
	adjacent' :: a -> Coord -> [Coord]
	distance :: a -> Coord -> Coord -> Int

data Plane
	= FlatSq
	-- Hex tiles where perfect East/West movement is possible, but not for
	-- North/South.
	| FlatHex
	deriving (Eq, Show)

data PlaneGeom = PlaneGeom
	{ pgPlane :: Plane
	, pgSizeX :: Int
	, pgSizeY :: Int
	, pgTiles :: [Coord]
	} deriving (Eq, Show)

instance ZTile PlaneGeom where
	indices = pgTiles
	adjacent PlaneGeom{..} idx = filter (flip elem pgTiles) $ case pgPlane of
		FlatSq -> map (go idx)
			[ DXPlus
			, DXMinus
			, DYPlus
			, DYMinus
			]
		FlatHex -> map (go' idx)
			[ [DXPlus]
			, [DXMinus]
			, [DYPlus]
			, [DYMinus]
			, [DYPlus, DXMinus]
			, [DYMinus, DXPlus]
			]
	adjacent' pg@PlaneGeom{..} idx = case pgPlane of
		FlatSq -> filter (flip elem pgTiles) $ map (go' idx)
			[ [DXPlus]
			, [DXMinus]
			, [DYPlus]
			, [DYMinus]
			, [DXPlus, DYPlus]
			, [DXMinus, DYMinus]
			, [DYPlus, DXMinus]
			, [DYMinus, DXPlus]
			]
		FlatHex -> adjacent pg idx
	-- Distance between two squares along the grid, assuming no obstacles.
	distance PlaneGeom{..} (x1, y1) (x2, y2) = case pgPlane of
		FlatSq -> max (abs $ x2 - x1) (abs $ y2 - y1)
		FlatHex -> maximum
			[ abs $ x2 - x1
			, abs $ y2 - y1
			, abs $ z2 - z1
			]
			where
			z2 = (-x2) - y2
			z1 = (-x1) - y1

go :: Coord -> Direction -> Coord
go (x, y) d = case d of
	DYPlus -> (x, y + 1)
	DYMinus -> (x, y - 1)
	DXPlus -> (x + 1, y)
	DXMinus -> (x - 1, y)

go' :: Coord -> [Direction] -> Coord
go' = foldl go

flatSqDefault :: PlaneGeom
flatSqDefault = flatSqInit 19 19

-- Default FlatSq plane. It is a regular arrangement of squares, identical to
-- that of a Chess or Go board; each square's *edge* lies in one of the four
-- cardinal directions: North, South, East or West.
flatSqInit :: Int -> Int -> PlaneGeom
flatSqInit x y
	| x < 1 || y < 1 = PlaneGeom
		{ pgPlane = FlatSq
		, pgSizeX = 1
		, pgSizeY = 1
		, pgTiles = [(0, 0)]
		}
	| otherwise = PlaneGeom
		{ pgPlane = FlatSq
		, pgSizeX = x
		, pgSizeY = y
		, pgTiles = [ (x', y') | x' <- [0..(x - 1)], y' <- [0..(y - 1)] ]
		}

flatHexInit :: Int -> Int -> PlaneGeom
flatHexInit x y
	| x < 1 || y < 1 = PlaneGeom
		{ pgPlane = FlatHex
		, pgSizeX = 1
		, pgSizeY = 1
		, pgTiles = [(0, 0)]
		}
	| otherwise = PlaneGeom
		{ pgPlane = FlatHex
		, pgSizeX = x
		, pgSizeY = y
		, pgTiles = buildHexes x y
		}

buildHexes :: Int -> Int -> [Coord]
buildHexes xWidth yHeight = snd $ foldl step (0, []) ys
	where
	ys = [0..(yHeight - 1)]
	step (x, acc) y = (x', acc ++ map (flip (,) y) xs)
		where
		xs = [x..(x + (xWidth - 1))]
		-- If we encounter an odd row, decrement the starting x index for the
		-- next iteration.
		x' = if mod y 2 == 1
			then x - 1
			else x
