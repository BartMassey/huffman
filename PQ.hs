module PQ
where

import Data.Maybe

type PQ a = Maybe (PQNode a)
data PQNode a = PQNode a (PQ a) (PQ a)

empty :: (Ord a) => PQ a
empty = Nothing

insert :: (Ord a) => a -> PQ a -> PQ a
insert v q = Just (PQNode v q Nothing)

extract_min :: (Ord a) => PQ a -> (PQ a, a)
extract_min (Just (PQNode v q q')) = (combine q q', v) where
    combine Nothing (Just q') = Just q'
    combine (Just q) Nothing = Just q
    combine    (Just (PQNode v1 q1 q1'))
            q2@(Just (PQNode v2 _ _)) | v1 <= v2 =
               (Just (PQNode v1 (combine q1 q1') q2))
    combine q1@(Just (PQNode v1 _ _))
               (Just (PQNode v2 q2 q2')) | v2 < v1 =
               (Just (PQNode v2 q1 (combine q2 q2')))

isEmpty :: (Ord a) => PQ a -> Bool
isEmpty Nothing = True
isEmpty _ = False
