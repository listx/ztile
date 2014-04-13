\chapter{ZTile/PathFinding}

For this module, we borrow terms from computer science when describing the shortest path problem.
We speak of vertices, edges, and graphs.
Vertices are the points, or nodes, where we can visit (e.g., cities).
Edges connect two vertices together (e.g., roads).
A graph is the collection of vertices and edges; more specifically, for purposes of the \ct{dijkstra} algorithm, it only deals with edges that are non-negative.

The \ct{Data.List.Key} module is from the \href{http://hackage.haskell.org/package/utility-ht}{utility-ht} package.

\begin{code}
module ZTile.PathFinding where

import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map, notMember)

import ZTile.Util

data Weight
	= Finite Int
	| Infinity
	deriving (Eq, Show)
\end{code}

For pathfinding problems, we are interested in distances (the length of edges) between vertices in a graph.
The weight can be either \ct{Infinity} for unvisited vertices, or \ct{Finite Int} for visited ones.

\begin{code}
instance Ord Weight where
	compare (Finite a) (Finite b) = compare a b
	compare (Finite _) Infinity = LT
	compare Infinity (Finite _) = GT
	compare Infinity Infinity = EQ

instance Num Weight where
	Finite a + Finite b = Finite (a + b)
	Finite _ + _ = Infinity
	Infinity + _ = Infinity

	Finite a * Finite b = Finite (a * b)
	Finite _ * _ = Infinity
	Infinity * _ = Infinity

	abs (Finite a) = Finite (abs a)
	abs _ = Infinity

	negate (Finite a) = Finite (negate a)
	negate _ = Infinity

	signum (Finite a) = Finite (signum a)
	signum _ = Infinity

	fromInteger a = Finite (fromInteger a)
\end{code}

Because we need to perform basic math operations on the \ct{Weight} type, we define the \ct{Ord} and \ct{Num} typeclass instances here.

\begin{code}
type TileId = Int
type WTile = (TileId, Weight)
\end{code}

We define a \ct{WTile} type here for weighted tiles, which could perhaps be used by the user of this package.
The idea is that each tile will have a weight associated with it, and that moving from tile A to tile B will incur a movement cost that is the interpolation between the weight of A and B divided by 2, or some other scheme.
It is up to the user to decide how to determine the values of the weights between two tiles, and to generate the \ct{[(a, a, Int)]} list required to feed to \ct{buildGraph}.

\section{Dijkstra's Algorithm}

\begin{code}
buildGraph :: Ord a
	=> [(a, a, Int)]
	-> Either (String, [(a, a)]) (Map a [(a, Weight)])
buildGraph edges
	| length es /= length (nub es)
		= Left ("duplicate edge weight definitions", es \\ nub es)
	| any (<0) ws
		= Left
			( "negative weights detected"
			, map getEdge $ filter ((<0) . getWeight) edges
			)
	| otherwise = Right
		. fromListWith (++)
		$ concatMap (\(a, b, w) -> [(a, [(b, Finite w)]), (b, [])]) edges
	where
	getEdge = fstSnd3
	getWeight = thd3
	es = map getEdge edges
	ws = map getWeight edges
\end{code}

\ct{buildGraph} generates the graph structure we will be working with, where each vertex has a list of neighboring vertices.
It takes a list of directed edges, in the format \((v_1, v_2, w)\); e.g., if it is given an edge \ct{(LA, NY, 1000)}, this is interpreted as an arrow pointing from LA to NY (and *only* in this direction), with a weight of 1000 units.

We also check if the given list of edges makes sense, in that
	\begin{itemize}
	\item it does not contain any duplicate edge definitions, and
	\item it does not contain any negative weights (because Dijkstra's algorithm cannot handle negative weights).
	\end{itemize}

\begin{code}
dijkstra :: Ord a => Map a [(a, Weight)] -> a -> Map a (Weight, Maybe a)
dijkstra graph source = dijkstra' graph wverts verts
	where
	verts = keys graph
	wverts = fromList $ map setVertex verts
	setVertex v =
		( v
		, (if v == source then Finite 0 else Infinity, Nothing)
		)
\end{code}

Dijkstra's algorithm.
The return type is another \ct{Map} type, where each vertex has a final weight (i.e., distance) associated with it (the shortest distance from the source vertex), as well as a \ct{Maybe a} type, which holds the previous vertex traveled to reach this vertex from the source.
Unreachable vertices will have the value \ct{(Infinity, Nothing)}.

\begin{code}
dijkstra' :: Ord a
	=> Map a [(a, Weight)]
	-> Map a (Weight, Maybe a)
	-> [a]
	-> Map a (Weight, Maybe a)
dijkstra' _ finished [] = finished
dijkstra' graph finished unvisited
	= dijkstra' graph (foldl' readjust finished uNeighbors)
	$ delete u unvisited
	where
	u = K.minimum (fst . (finished !)) unvisited
	uNeighbors = graph ! u
	neighborWeight = fst (finished ! u)
	readjust vxmap (neighbor, weight)
		= adjust (min (weight + neighborWeight, Just u)) neighbor vxmap
\end{code}

We examine one unvisited vertex at a time, until the set of all unvisited vertices becomes empty (at every iteration, we call \ct{delete} to remove the minimum-distance vertex \ct{u} from it).
\ct{K.minimum} has type \ct{minimum :: Ord b => (a -> b) -> [a] -> a}.
That is, \ct{u} is the vertex with the shortest distance to the source that is in the unvisited set.

\begin{code}
shortestPath :: Ord a => a -> a -> Map a [(a, Weight)] -> [a]
shortestPath source dest graph
	| source == dest = [] -- ignore self-loops
	| notMember dest graph = []
	| Infinity == fst (dijkstra graph source ! dest) = [] -- dest is unreachable
	| otherwise = reverse $ traceBack dest
	where
	traceBack x = x : maybe [] traceBack (snd $ dijkstra graph source ! x)
\end{code}

To retrieve the shortest path, we simply reverse our direction from the destination.
