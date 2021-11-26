module Utils where

import           Data.Maybe                     ( catMaybes )
import           Data.Char                      ( toUpper
                                                , toLower
                                                , isSpace
                                                )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
import           Datatypes

instance Show Chord where
  show (Chord base scale) | scale == Major = map toUpper repr
                          | otherwise      = map toLower repr
    where repr = fromMaybe "" $ lookup base revRomans

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

truncate' :: Int -> Double -> Double
truncate' n x = (fromIntegral (floor (x * t))) / t where t = 10 ^ n

romans :: [(String, Int)]
romans = (flip zip $ [1 ..]) $ ["I", "II", "III", "IV", "V", "VI", "VII"]

revRomans :: [(Int, String)]
revRomans = map (uncurry $ flip (,)) romans

fromRoman :: String -> Maybe Int
fromRoman = (flip lookup) romans . (map toUpper)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

showSequence :: ChordSequence -> String
showSequence = intercalate " " . (fmap (intercalate "-")) . (show <<$>>)

reprSong :: (String, ChordSequence) -> String
reprSong (name, sequence) = name <> " - " <> (showSequence sequence)
