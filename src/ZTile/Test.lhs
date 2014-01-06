\chapter{ZTile/Test}

\begin{code}
module ZTile.Test where

import Control.Monad
import Control.Monad.Primitive
import Data.Graph.Inductive as GI
import Data.Graph.Inductive.Example (genLNodes)
import Data.GraphViz
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Vector as V
import System.Exit
import System.Random.MWC as MWC
import System.Random.MWC.CondensedTable

import ZTile.PathFinding
import ZTile.Util
\end{code}

\section{Random Graph Generation}

\begin{code}
type Vertex = Int
\end{code}

We abstract away all vertices to just integers.

\begin{code}
data GraphProperty
	= GpNoLoops
	| GpNoBidirs
	| GpDAG
	deriving (Eq, Show)
\end{code}

We classify the kind of graph properties we are interested in.

\begin{code}
randWGraph :: (DynGraph g, PrimMonad m)
	=> Int -> Double -> [GraphProperty] -> MWC.Gen (PrimState m) -> m (g Int Int)
randWGraph n p gps rng
	| n < 1 = return GI.empty
	| p <= 0.0 = return $ mkGraph (genLNodes 1 n) []
	| p >= 1.0 = prepareGraph =<< mkEdges' n gps rng
	| otherwise = prepareGraph =<< randInclude rng p =<< mkEdges' n gps rng
	where
	prepareGraph es = return . mkGraph (genLNodes 1 n)
		=<< mapM addWeight
		=<< incIdxs es
	incIdxs = return . map (\(v1, v2) -> (v1 + 1, v2 + 1))
	addWeight (v1, v2) = do
		w <- genFromTable table rng
		return (v1, v2, w)
	table = tableFromWeights . V.fromList . zip [1..] $ map (1/) [0.5,1..5]
\end{code}

This is our baseline Erd\H{o}s-Rényi random graph generation algorithm, with some caveats.
The original algorithm considers every single possible edge in the graph, and adds it into the graph if the random number is less than \(p\).
Our version in \ct{randWGraph} does not consider every single edge; rather, the output graph depends on the \ct{[GraphProperty]} list passed in as follows:
\begin{itemize}
\item GpDAG: all edges point from a smaller vertex to a bigger one, and thus the appearance looks like a DAG (directed acyclic graph);
\item GpNoLoops: all edges' endpoints point to different vertices (i.e., there is no edge from a vertex back to itself);
\item GpNoBidirs: if there is an edge \((v_1, v_2)\), then there is no edge \((v_2, v_1)\) (i.e., there can only be 1 edge between any two vertices, if at all).
\end{itemize}
The other parameters to this function are the standard ones -- \ct{n} for the total number of vertices, and \ct{p} for the percentage in which an arbitrary edge will be created.
The \ct{incIdxs} function simply increments all vertices in the graph by 1, to make it compatible with FGL's vertex indexing scheme, which counts starting from 1, not 0.

\begin{code}
mkEdges':: PrimMonad m
	=> Int -> [GraphProperty] -> MWC.Gen (PrimState m) -> m [(Vertex, Vertex)]
mkEdges' n gps rng
	| elem GpDAG gps = return $ edgesDAG n
	| allElem [GpNoLoops, GpNoBidirs] gps = randFstSnd rng $ edgesSimple n
	| elem GpNoLoops gps = return $ edgesNoLoops n
	| elem GpNoBidirs gps = return $ edgesDAGselfLoops n
	| otherwise = return $ edgesAll n

edgesAll :: Int -> [(Vertex, Vertex)]
edgesAll n = [(x, y) | x <- [0..(n - 1)], y <- [0..(n - 1)]]

edgesNoLoops :: Int -> [(Vertex, Vertex)]
edgesNoLoops = filter (uncurry (/=)) . edgesAll

edgesDAG :: Int -> [(Vertex, Vertex)]
edgesDAG n = thd3 $ foldl' step ((0, 1), 1, []) [1..(div (n * (n + 1)) 2)]
	where
	step (e@(x, y), yStart, acc) _
		| y == n = ((x + 1, yStart + 1), yStart + 1, acc)
		| otherwise = ((x, y + 1), yStart, e:acc)

edgesDAGselfLoops :: Int -> [(Vertex, Vertex)]
edgesDAGselfLoops n = edgesDAG n ++ selfLoops
	where
	selfLoops = [(x, x) | x <- [0..(n - 1)]]

edgesSimple :: Int -> [((Vertex, Vertex), (Vertex, Vertex))]
edgesSimple n = zip (edgesDAG n) . map swap $ edgesDAG n
\end{code}

\ct{mkEdges'} returns the set of edges with the given desired properties.
The real workhorses are the various edge generation functions.
\ct{edgesDAG} is the most complex one; there are two main ideas behind it: first, do not generate self-loop edges, and second, given any two vertices, choose the edge direction that goes from the smaller vertex to the greater one.
Here are some sample values:\fn{The contents of each list have been reversed for clarity.}

\begin{center}
\rowcolors[]{4}{}{gray!15}
\begin{tabular}{ccl}
    \toprule
	Vertices & Edge list length & Edges \\
    \midrule
	2 & 1 & [(0, 1)] \\
	3 & 3 & [(0,1),(0,2),(1,2)] \\
	4 & 6 & [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)] \\
	5 & 10 & [(0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] \\
    \bottomrule
\end{tabular}
\end{center}

\ct{edgesDAG} thus generates all unique edges, in the sense that \((v_2, v_1)\) is discounted as a duplicate of \((v_1, v_2)\).
An interesting tidbit is that this list grows in the same manner as that of triangular numbers -- hence the formula
\[
\mathrm{EdgeListLength} = \frac{n(n + 1)}{2}
\]
used to generate the list fed into \ct{foldl'}.

\begin{code}
randInclude :: (Ord a, PrimMonad m, Variate a)
	=> MWC.Gen (PrimState m) -> a -> [b] -> m [b]
randInclude rng p = foldM f []
	where
	f acc edge = do
		theta <- uniform rng
		return $ if theta < p
			then edge:acc
			else acc
\end{code}

\ct{randInclude} randomly selects items from a list, based on a threshold percentage \ct{p}.

\begin{code}
randFstSnd :: PrimMonad m => MWC.Gen (PrimState m) -> [(a, a)] -> m [a]
randFstSnd rng = foldM f []
	where
	f acc pair = do
		theta <- uniformR ((0, 1) :: (Int, Int)) rng
		return $ (if theta == 0 then fst else snd) pair:acc
\end{code}

\ct{randFstSnd} randomly chooses between either the first or second item in a tuple.

\section{Test Suite}

We now test ZTile's \ct{shortestPath} function, to see if it matches FGL's version.
Because there can be multiple shortest paths in a graph, we only check to see if the chosen path lengths are the same in \ct{prop\_shortestPath}.

\begin{code}
test :: IO ()
test = do
	rng <- createSystemRandom
	mapM_ (testSP rng) [1..100]

testSP :: Gen (PrimState IO) -> Int -> IO ()
testSP rng testCase = do
	g <- randWGraph 10 0.3 [GpNoLoops] rng :: IO (Gr Int Int)
	unless (prop_shortestPath 1 10 g) $ do
		preview g
		putStrLn $ "test number: " ++ show testCase
		putStrLn $ "ztPath: " ++ show (ztPath g)
		putStrLn $ "fglPath: " ++ show (fglPath g)
		putStrLn $ "zt's graph: let gr = (\\(Right x) -> x) $ buildGraph "
			++ show (labEdges g)
		putStrLn $ "labEdges g: " ++ show (labEdges g)
		exitWith $ ExitFailure 1
	where
	fglPath = sp 1 10
	ztPath g = shortestPath 1 10 g'
		where
		g' = (\(Right x) -> x) . buildGraph $ labEdges g

prop_shortestPath :: (Ord a, DynGraph g) => Node -> Node -> g a Int -> Bool
prop_shortestPath a b g
	| null fglPath && null ztPath = True
	| otherwise = ztPathCost == fglPathCost
	where
	fglPath = sp a b g
	fglPathCost = Finite $ spLength a b g
	ztPath = shortestPath a b g'
	ztPathCost = Finite . sum $ map getEdgeWeight pathEdges
	getEdgeWeight e = fromJust $ lookup e edges'
		where
		edges' = map (\(x, y, w) -> ((x, y), w)) $ labEdges g
	pathEdges = zip ztPath $ drop 1 ztPath
	g' = (\(Right x) -> x) . buildGraph $ labEdges g
\end{code}

The condition that always returns True if both \ct{fglPath} and \ct{ztPath} are empty is only there to work around a likely bug in GHC 7.6.3, which makes the code here crash with ``Prelude.head: empty list''.
