\chapter{ztile-test}

Compile and run this file to test the functions in \ct{Ztile/Test.lhs}.

\begin{code}
module Main where

import Test.Framework (defaultMain)
import System.Random.MWC (withSystemRandom)

import qualified ZTile.Test as T1

main :: IO ()
main = withSystemRandom
	$ \g -> defaultMain
		[ T1.tests g
		]
\end{code}
