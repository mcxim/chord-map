module Datatypes where

data Chord = Chord Int Scale deriving (Eq)

type ChordSequence = [[Chord]]

data Scale = Major | Minor deriving (Show, Eq)
