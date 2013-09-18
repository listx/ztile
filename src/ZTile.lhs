\chapter{ZTile}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module ZTile where

import Data.List

-- Vertex is (X, Y); this can represent both square and hex tiles; for hex tiles,
-- the third axis, Z, can be calculated on the fly using the X and Y values.
type Vertex = (Int, Int)

data Direction
	= DXPlus
	| DXMinus
	| DYPlus
	| DYMinus
	deriving (Eq, Show)

class ZTile a where
	vertices :: a -> [Vertex]
	adjacent :: a -> Vertex -> [Vertex] -- adjacent edge-to-edge adjacent
	-- edge/point-to-edge/point; for square tiles, this routine check for the
	-- diagonal directions
	adjacent' :: a -> Vertex -> [Vertex]
	distance :: a -> Vertex -> Vertex -> Int
	contains :: a -> Vertex -> Bool
	size :: a -> (Int, Int)

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
	, pgTiles :: [Vertex]
	} deriving (Eq)

instance Show PlaneGeom where
	show PlaneGeom{..}
		= "PlaneGeom { pgPlane = " ++ show pgPlane ++ ",\n"
		++ "pgSizeX = " ++ show pgSizeX ++ ",\n"
		++ "pgSizeY = " ++ show pgSizeY ++ ",\n"
		++ "pgTiles =\n" ++ showPlaneGeom ++ "\n}"
		where
		showPlaneGeom = intercalate "\n" . map showPGRow $ reverse [0..(pgSizeY - 1)]
		showPGRow yIdx = indent ++ (intersperse ' ' $ map (\_ -> 'x') [0..(pgSizeX - 1)])
			where
			indent = if odd yIdx
				then " "
				else ""

instance ZTile PlaneGeom where
	vertices = pgTiles
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
	-- Distance between two squares (or hexes) along the grid, assuming no obstacles.
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
	contains PlaneGeom{..} = flip elem pgTiles
	size PlaneGeom{..} = (pgSizeX, pgSizeY)

go :: Vertex -> Direction -> Vertex
go (x, y) d = case d of
	DYPlus -> (x, y + 1)
	DYMinus -> (x, y - 1)
	DXPlus -> (x + 1, y)
	DXMinus -> (x - 1, y)

go' :: Vertex -> [Direction] -> Vertex
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

-- E.g., for a size x=4 and y=3, we get:
--
--  x x x x		<- row 2, an even row, so we shift the tiles left by 1 unit
--   x x x x
--  x x x x		<- row 0
buildHexes :: Int -> Int -> [Vertex]
buildHexes xWidth yHeight = snd $ foldl step (0, []) ys
	where
	ys = [0..(yHeight - 1)]
	step (x, acc) y = (x', acc ++ map (flip (,) y) xs)
		where
		xs = [x..(x + (xWidth - 1))]
		-- If we encounter an odd row, decrement the starting x index for the
		-- next iteration (an even row).
		x' = if odd y
			then x - 1
			else x

flatPlaneInit :: Plane -> Int -> Int -> PlaneGeom
flatPlaneInit p = case p of
	FlatSq -> flatSqInit
	FlatHex -> flatHexInit

genVertices :: Plane -> Int -> Int -> [Vertex]
genVertices p x y = case p of
	FlatSq -> pgTiles $ flatSqInit x y
	FlatHex -> pgTiles $ flatHexInit x y
\end{code}
