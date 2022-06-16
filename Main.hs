{-# LANGUAGE ViewPatterns #-}

module Main where

import           System.IO                      ( readFile )
import           Data.Char
import           Data.List                      ( elemIndex
                                                , sort
                                                , (\\)
                                                , inits
                                                , intercalate
                                                )
import           Control.Monad                  ( msum )
import           Data.List.Split
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                , mapMaybe
                                                )
import           Data.Bifunctor
import           System.Environment             ( getArgs )
import           Utils
import           Datatypes


main :: IO ()
main = do
  path     <- head <$> getArgs
  database <- db path
  let songs =
        concat
          . map (\(name, progs) -> zip (repeat name) progs)
          . catMaybes
          $ database
  let csvData = convertToCSV songs
  writeFile "./data.csv" csvData

diffBeat :: [[Int]] -> [[Int]] -> Int
diffBeat (x1 : restx) (y1 : resty) =
  let x2 = if null restx then x1 else head restx
      y2 = if null resty then y1 else head resty
  in  sum . map length $ [x1 \\ y1, x2 \\ y2]

songDiff :: ChordSequence -> ChordSequence -> Int
songDiff song1 song2 = minimum tries
 where
  toNotes :: ChordSequence -> [[[Int]]]
  toNotes song =
    ((chordNotes (fromMaybe Major (guessScale . concat $ song))) <<$>> song)
  [notes1, notes2] = map toNotes [song1, song2]
  tries            = map
    (\offset -> sum
      [ diffBeat beat1 ((+% offset) <<$>> beat2)
      | (beat1, beat2) <- zip notes1 notes2
      ]
    )
    [0 .. 11]

scalePitches Major = [0, 2, 4, 5, 7, 9, 11]
scalePitches Minor = [0, 2, 3, 5, 7, 8, 10]

triadPitches Major = [0, 4, 7]
triadPitches Minor = [0, 3, 7]

scaleRules :: [(Chord, Scale)]
scaleRules =
  [ (Chord 1 Major, Major)
  , (Chord 1 Minor, Minor)
  , (Chord 6 Major, Minor)
  , (Chord 6 Minor, Major)
  , (Chord 3 Major, Minor)
  , (Chord 3 Minor, Major)
  , (Chord 4 Major, Major)
  , (Chord 4 Minor, Minor)
  ]

checkRule :: [Chord] -> (Chord, Scale) -> Maybe Scale
checkRule chords (chord, scale) | chord `elem` chords = Just scale
                                | otherwise           = Nothing

guessScale :: [Chord] -> Maybe Scale
guessScale chords = msum . map (checkRule chords) $ scaleRules

(+%) :: Int -> Int -> Int
a +% b = (a + b) `mod` 12

chordNotes :: Scale -> Chord -> [Int]
chordNotes context (Chord base scale) =
  sort . map ((scalePitches context !! (base - 1)) +%) $ triadPitches scale

chord :: String -> Maybe Chord
chord roman = do
  base <- fromRoman $ toUpper <$> roman
  let scale = if isUpper . head $ roman then Major else Minor
  return $ Chord base scale

parseResults = mapMaybe (traverse . traverse $ chord)
dashes = ((splitOn "-") <<$>>)
slashes = concat . fmap (sequence . fmap (splitOn "/") . splitOn " ")
commas = splitOn ", "

parseSequences :: String -> [ChordSequence]
parseSequences = parseResults . dashes . slashes . commas

parse :: [Char] -> Maybe (String, [ChordSequence])
parse string =
  (\dash -> bimap strip (parseSequences . strip . tail) . splitAt dash $ string)
    <$> elemIndex '-' string

db path = (map parse . lines) <$> readFile path

convertToCSV :: [(String, ChordSequence)] -> String
convertToCSV songs =
  "; " <> (intercalate "; " (map reprSong songs)) <> "\n" ++ unlines
    [ reprSong row
      <> "; "
      <> ( intercalate "; "
         . map
             ( show
             . (truncate' 2)
             . (\x -> 1.0 / (fromIntegral x + 1.0))
             . songDiff (snd row)
             . snd
             )
         . reverse
         $ cols
         )
    | (reverse -> (row:cols)) <- inits songs
    ]
