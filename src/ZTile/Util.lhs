\chapter{ZTile/Util}

\begin{code}
module ZTile.Util where

allElem :: Eq a => [a] -> [a] -> Bool
allElem members clan = and $ map (flip elem clan) members

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

fstSnd3 :: (a, b, c) -> (a, b)
fstSnd3 (a, b, _) = (a, b)
\end{code}
