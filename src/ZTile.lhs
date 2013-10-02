\chapter{ZTile}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module ZTile where

import Data.List (intercalate, intersperse)

type Tile = (Int, Int)
\end{code}

The \ct{Tile} type is based on a \((x, y)\) coordinate system.
This simple system can represent both square and hex tiles.
For hex tiles, the third \(z\)-axis coordinate can be calculated on the fly using the \(x\) and \(y\) values, using the formula
\begin{equation}
\label{eq:zcoord}
z = (-x) - y
\end{equation}
.

\begin{code}
data Direction
	= DXPlus
	| DXMinus
	| DYPlus
	| DYMinus
	deriving (Eq, Show)

go :: Tile -> [Direction] -> Tile
go = foldl go'

go' :: Tile -> Direction -> Tile
go' (x, y) d = case d of
	DYPlus -> (x, y + 1)
	DYMinus -> (x, y - 1)
	DXPlus -> (x + 1, y)
	DXMinus -> (x - 1, y)
\end{code}

The \ct{Direction} type represents the four ways a \ct{Tile} can change by the smallest amount.
A \ct{Tile} can change by adding or subtracting from its \(x\) value or \(y\) value.
By isolating these four possible operations into a single data type, we can more easily reason about changes to a \ct{Tile} elsewhere.

\begin{code}
data Plane
	= FlatSq
	| FlatHex
	deriving (Eq, Show)
\end{code}

The \ct{Plane} represents the type of arrangement of tiles possible.
Currently, only two arrangements are possible --- \ct{FlatSq} and \ct{FlatHex}.
\ct{FlatSq} is a flat plane composed of regular squares (like a chess board); \ct{FlatHex} is a flat plane composed of hexagons, but aligned so that perfect East/West movement is possible by simply changing the \(x\) value in the \ct{Tile}.

\begin{code}
data TileGeom = TileGeom
	{ tgPlane :: Plane
	, tgSizeX :: Int
	, tgSizeY :: Int
	, tgTiles :: [Tile]
	} deriving (Eq)

instance Show TileGeom where
	show TileGeom{..}
		= "TileGeom { tgPlane = " ++ show tgPlane ++ ",\n"
		++ "tgSizeX = " ++ show tgSizeX ++ ",\n"
		++ "tgSizeY = " ++ show tgSizeY ++ ",\n"
		++ "tgTiles =\n" ++ showTileGeom ++ "\n}"
		where
		showTileGeom = intercalate "\n" . map showTGRow $ reverse [0..(tgSizeY - 1)]
		showTGRow yIdx = indent ++ (intersperse ' ' $ map (\_ -> 'x') [0..(tgSizeX - 1)])
			where
			indent = if odd yIdx
				then " "
				else ""
\end{code}

The \ct{TileGeom} type, or simply \ct{TG}, is the core data type offered by ZTile.
The \ct{tgPlane} parameter describes which \ct{Plane} type is used.
The \ct{tgSizeX} and \ct{tgSizeY} parameters state the size, in \(x\) and \(y\) coordinate space, the tiles take up.
Lastly, the \ct{tgTiles} parameter lists every \ct{Tile} in \ct{TG}.

The custom \ct{Show} instance is there to make \ct{TG}s look easier to the human eye; in particular, the \ct{showTileGeom} function displays the \ct{TG} with basic ASCII-art.

\begin{code}
class ZTile a where
	tiles :: a -> [Tile]
	adjacent :: a -> Tile -> [Tile]
	adjacent' :: a -> Tile -> [Tile]
	distance :: a -> Tile -> Tile -> Int
	contains :: a -> Tile -> Bool
	size :: a -> (Int, Int)
\end{code}

The ZTile class defines a set of common functions that a tile map should support:
\begin{itemize}
	\item \ct{tiles}: List all tiles.
	\item \ct{adjacent}: Given a vertex, return all tiles that share a common edge.
	\item \ct{adjacent'}: Given a vertex, return all tiles that share a common edge \textit{or vertex}.
For square tiles, this would check diagonal tiles as well.
For hex tiles, as they always share a common edge, this function is the same as \ct{adjacent}.
	\item \ct{distance}: The minimum distance between two tiles, if there are no obstructions.
	\item \ct{contains}: Checks if a given tile exists in the tile map.
	\item \ct{size}: Return the size of the tile map, in \((x, y)\) form.
\end{itemize}

The \ct{ZTile} class instance for \ct{TileGeom} is relatively straightforward.
The highlight is the ease in which we describe the \ct{adjacent} and \ct{adjacent'} functions with the help of the \ct{Direction} type we defined in the beginning.

\begin{code}
instance ZTile TileGeom where
	tiles = tgTiles
	adjacent TileGeom{..} idx = filter (flip elem tgTiles) $ case tgPlane of
		FlatSq -> map (go' idx)
			[ DXPlus
			, DXMinus
			, DYPlus
			, DYMinus
			]
		FlatHex -> map (go idx)
			[ [DXPlus]
			, [DXMinus]
			, [DYPlus]
			, [DYMinus]
			, [DYPlus, DXMinus]
			, [DYMinus, DXPlus]
			]
	adjacent' tg@TileGeom{..} idx = case tgPlane of
		FlatSq -> filter (flip elem tgTiles) $ map (go idx)
			[ [DXPlus]
			, [DXMinus]
			, [DYPlus]
			, [DYMinus]
			, [DXPlus, DYPlus]
			, [DXMinus, DYMinus]
			, [DYPlus, DXMinus]
			, [DYMinus, DXPlus]
			]
		FlatHex -> adjacent tg idx
	distance TileGeom{..} (x1, y1) (x2, y2) = case tgPlane of
		FlatSq -> max (abs $ x2 - x1) (abs $ y2 - y1)
		FlatHex -> maximum
			[ abs $ x2 - x1
			, abs $ y2 - y1
			, abs $ z2 - z1
			]
			where
			z2 = (-x2) - y2
			z1 = (-x1) - y1
	contains TileGeom{..} = flip elem tgTiles
	size TileGeom{..} = (tgSizeX, tgSizeY)
\end{code}

Notice how the \ct{distance} function uses the equation at \ref{eq:zcoord} to determine the \(z\) coordinate distance between two tiles.

\begin{code}
flatPlaneInit :: Plane -> Int -> Int -> TileGeom
flatPlaneInit p = case p of
	FlatSq -> flatSqInit
	FlatHex -> flatHexInit

flatSqInit :: Int -> Int -> TileGeom
flatSqInit x y
	| x < 1 || y < 1 = TileGeom
		{ tgPlane = FlatSq
		, tgSizeX = 1
		, tgSizeY = 1
		, tgTiles = [(0, 0)]
		}
	| otherwise = TileGeom
		{ tgPlane = FlatSq
		, tgSizeX = x
		, tgSizeY = y
		, tgTiles = [ (x', y') | x' <- [0..(x - 1)], y' <- [0..(y - 1)] ]
		}

flatHexInit :: Int -> Int -> TileGeom
flatHexInit x y
	| x < 1 || y < 1 = TileGeom
		{ tgPlane = FlatHex
		, tgSizeX = 1
		, tgSizeY = 1
		, tgTiles = [(0, 0)]
		}
	| otherwise = TileGeom
		{ tgPlane = FlatHex
		, tgSizeX = x
		, tgSizeY = y
		, tgTiles = buildHexes x y
		}
\end{code}

The \ct{flatPlaneInit} function initializes a \ct{TG} based on the given \ct{Plane} and \((x, y)\) size.
The helper functions \ct{flatSqInit} and \ct{flatHexInit} do the real work.
The \ct{flatHexInit} function's real work is done with \ct{buildHexes}, which carefully sets each row of hex tiles with the correct \(x\) coordinate.

\begin{code}
-- E.g., for a size x=4 and y=3, we get:
--
--  x x x x		<- row 2, an even row, so we shift the tiles left by 1 unit
--   x x x x
--  x x x x		<- row 0
buildHexes :: Int -> Int -> [Tile]
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
\end{code}

Below are some miscellaneous functions.

\begin{code}
flatSqDefault :: TileGeom
flatSqDefault = flatSqInit 19 19

genTiles :: Plane -> Int -> Int -> [Tile]
genTiles p x y = case p of
	FlatSq -> tgTiles $ flatSqInit x y
	FlatHex -> tgTiles $ flatHexInit x y
\end{code}
