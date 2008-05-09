module PQ
where

import Data.Maybe

type PQ a = Maybe (PQNode a)
data PQNode a = PQNode Int a (PQ a) (PQ a)

empty :: PQ a
empty = Nothing

insert :: (Ord a) => a -> PQ a -> PQ a
insert v q = Just (PQNode 0 v q Nothing)

extract_min :: (Ord a) => PQ a -> (PQ a, a)
extract_min (Just (PQNode _ v q q')) = (combine q q', v) where
    combine Nothing (Just q') = Just q'
    combine (Just q) Nothing = Just q
    combine    (Just (PQNode n1 v1 q1 q1'))
            q2@(Just (PQNode n2 v2 _ _)) | n1 < n2 =
               (Just (PQNode (n1 + n2) v1 (combine q1 q1') q2))
    combine q1@(Just (PQNode n1 v1 _ _))
               (Just (PQNode n2 v2 q2 q2')) | n2 <= n1 =
               (Just (PQNode (n1 + n2) v2 q1 (combine q2 q2')))

isEmpty :: PQ a -> Bool
isEmpty Nothing = True
isEmpty _ = False
