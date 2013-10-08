\chapter{ZTile/PathFinding}

For this module, we borrow terms from computer science when describing the shortest path problem.
We speak of vertices, edges, and graphs.
Vertices are the points, or nodes, where we can visit (e.g., cities).
Edges connect two vertices together (e.g., roads).
A graph is the collection of vertices and edges; more specifically, for purposes of the \ct{dijkstra} algorithm, it only deals with edges that are non-negative.

The \ct{Data.List.Key} module is from the \ct{utility-ht} package.

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module ZTile.PathFinding where

import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

import ZTile
\end{code}

For pathfinding problems, we are interested in distances (the length of edges) between vertices in a graph.
The weight can be either \ct{Infinity} for unvisited vertices, or \ct{Finite Int} for visited ones.

\begin{code}
data Weight
	= Finite Int
	| Infinity
	deriving (Eq, Show)
\end{code}

Because we need to perform basic math operations on the \ct{Weight} type, we define the \ct{Ord} and \ct{Num} typeclass instances here.

\begin{code}
instance Ord Weight where
	compare (Finite a) (Finite b) = compare a b
	compare (Finite _) Infinity = LT
	compare Infinity (Finite _) = GT
	compare Infinity Infinity = EQ

instance Num Weight where
	Finite a + Finite b = Finite (a + b)
	Finite a + _ = Infinity
	Infinity + _ = Infinity

	Finite a * Finite b = Finite (a * b)
	Finite a * _ = Infinity
	Infinity * _ = Infinity

	abs (Finite a) = Finite (abs a)
	abs _ = Infinity

	signum (Finite a) = Finite (signum a)
	signum _ = Infinity

	fromInteger a = Finite (fromInteger a)
\end{code}

We define a \ct{WTile} type here for weighted tiles, which could perhaps be used by the user of this package.
The idea is that each tile will have a weight associated with it, and that moving from tile A to tile B will incur a movement cost that is the interpolation between the weight of A and B divided by 2, or some other scheme.
It is up to the user to decide how to determine the values of the weights between two tiles, and to generate the \ct{[(a, a, Weight)]} list required to feed to \ct{buildGraph}.

\begin{code}
type TileId = Int
type WTile = (TileId, Weight)
\end{code}

\ct{buildGraph} generates the graph structure we will be working with, where each vertex has a list of neighboring vertices.

\begin{code}
buildGraph :: Ord a => [(a, a, Weight)] -> Map a [(a, Weight)]
buildGraph graph = fromListWith (++) $ graph
	>>= \(a, b, w) -> [(a, [(b, w)]), (b, [(a, w)])]
\end{code}

Dijkstra's algorithm.

\begin{code}
dijkstra :: Ord a => a -> Map a [(a, Weight)] -> Map a (Weight, Maybe a)
dijkstra source graph = f
	(fromList [(v, (if v == source then Finite 0 else Infinity, Nothing))
		| v <- keys graph]) $ keys graph
	where
	f ds [] = ds
	f ds q = f (foldr relax ds $ graph ! m) $ delete m q
		where
		m = K.minimum (fst . (ds !)) q
		relax (e, d) = adjust (min (fst (ds ! m) + d, Just m)) e
\end{code}

To retrieve the shortest path, we simply reverse our direction from the destination.

\begin{code}
shortestPath :: Ord a => a -> a -> Map a [(a, Weight)] -> [a]
shortestPath source dest graph = reverse $ f dest
	where
	f x = x : maybe [] f (snd $ dijkstra source graph ! x)
\end{code}
