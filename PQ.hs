module PQ
where

import Data.Maybe

type PQ a = Maybe (PQNode a)
data PQNode a = PQNode Int a ((PQ a), (PQ a))

empty :: PQ a
empty = Nothing

insert :: (Ord a) => a -> PQ a -> PQ a
insert v Nothing = Just (PQNode 1 v (Nothing, Nothing))
insert v (Just (PQNode n v' qs))
    | v <= v' = Just (PQNode (n + 1) v (pushdown v' qs))
    | v > v' = Just (PQNode (n + 1) v' (pushdown v qs))
    where
      pushdown v (q1@(Just _), Nothing) =
          (q1, Just (PQNode 0 v (Nothing, Nothing)))
      pushdown v (Nothing, q2@(Just _)) =
          (Just (PQNode 0 v (Nothing, Nothing)), q2)
      pushdown v (q1'@(Just (PQNode v1 n1 q1)), q2'@(Just (PQNode v2 n2 q2)))
          | n1 <= n2 && v <= v1 =
              (Just (PQNode v (n1 + 1) (pushdown v1 q1)), q2')
          | n1 <= n2 && v > v1 =
              (Just (PQNode v1 (n1 + 1) (pushdown v q1)), q2')
          | n2 < n1 && v <= v1 =
              (q1', Just (PQNode v (n2 + 1) (pushdown v2 q2)))
          | n2 < n1 && v > v1 =
              (q1', Just (PQNode v2 (n2 + 1) (pushdown v q2)))

extract_min :: (Ord a) => PQ a -> (PQ a, a)
extract_min (Just (PQNode 0 v (Nothing, Nothing))) = (Nothing, v)
extract_min (Just (PQNode _ v (q1@(Just _), Nothing))) = (q1, v)
extract_min (Just (PQNode _ v (Nothing, q2@(Just _)))) = (q2, v)
extract_min (Just (PQNode n v (q1@(Just (PQNode n1 v1 _)),
                               q2@(Just (PQNode n2 v2 _)))))
    | n1 <= n2 = let (v2', q2') = extract_min q2 in
                 (v, Just (n - 1) v2' (q1, q2'))
    | n1 > n2 = let (v1', q1') = extract_min q1 in
                (v, Just (n - 1) v1' (q1', q2))

isEmpty :: PQ a -> Bool
isEmpty Nothing = True
isEmpty _ = False
