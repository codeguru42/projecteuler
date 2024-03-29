-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 54

import System.IO(openFile, hGetContents, hClose, IOMode(ReadMode))
import Data.List(maximumBy, sort, sortBy, find)
import ProjectEuler(count)

data FaceValue = Two
               | Three
               | Four
               | Five
               | Six
               | Seven
               | Eight
               | Nine
               | Ten
               | Jack
               | Queen
               | King
               | Ace
               deriving (Eq, Show, Ord, Enum)

data Suit = Hearts
          | Diamonds
          | Spades
          | Clubs
          deriving (Eq, Show)

data Card = Card {
  faceValue :: FaceValue, 
  suit :: Suit
  } deriving (Eq)

instance Show Card
         where show (Card face suit) = (show face) ++ " of " ++ (show suit)

data Rank  = HighCard [FaceValue]
             -- HighCard vs where vs is a list of the face values of the
             -- cards in descending order
           | OnePair FaceValue [FaceValue]
             -- OnePair v [v1,v2,v3] is a pair of vs with v1 high, v2 next,
             -- and v3 low
           | TwoPairs FaceValue FaceValue FaceValue
             -- TwoPairs v1 v2 v' is a pair of v1s and a pair of v2s 
             -- (with v1 > v2) and a v'
           | ThreeOfAKind FaceValue [FaceValue]
             -- ThreeOfAKind v [v1,v2] has three vs with v1 high and v2 low
           | Straight FaceValue
             -- Straght v has high card v
           | Flush [FaceValue]
             -- Flush vs where vs is a list of the face values of the cards
             -- in descending order
           | FullHouse FaceValue FaceValue
             -- FullHouse v1 v2 is v1 full of v2
           | FourOfAKind FaceValue FaceValue 
             -- FourOfAKind v v' has four vs with v' kicker
           | StraightFlush FaceValue
             -- StraightFlush v has high card v
           | RoyalFlush
           deriving (Ord, Eq)

instance Show Rank
         where show (HighCard (v:vs)) =
                 (show v) ++ " high"
               show (OnePair p (v:vs)) =
                 "Pair of " ++ (show p) ++ "s with " ++ (show v) ++ " high"
               show (TwoPairs p1 p2 v) =
                 (show p1) ++ "s and " ++ (show p2) ++ "s"
               show (ThreeOfAKind t v) =
                 "Three " ++ (show t) ++ "s"
               show (Straight high) =
                 "Straight " ++ (show high) ++ " high"
               show (Flush high) =
                 "Flush " ++ (show high) ++ " high"
               show (FullHouse v1 v2) =
                 (show v1) ++ " full of " ++ (show v2) ++ "s"
               show (FourOfAKind t v) =
                 "Four " ++ (show t) ++ "s"
               show (StraightFlush high) =
                 "Straight flush " ++ (show high) ++ " high"
               show RoyalFlush =
                 "Royal flush"

deck :: [Card]
deck = [Card face suit | 
        suit <- [Hearts, Spades, Diamonds, Clubs],
        face <- [Ace, Two, Three, Four, Five, Six, Seven, 
                 Eight, Nine, Ten, Jack, Queen, King]]

toFaceValue :: Char -> FaceValue
toFaceValue 'A' = Ace
toFaceValue '2' = Two
toFaceValue '3' = Three
toFaceValue '4' = Four
toFaceValue '5' = Five
toFaceValue '6' = Six
toFaceValue '7' = Seven
toFaceValue '8' = Eight
toFaceValue '9' = Nine
toFaceValue 'T' = Ten
toFaceValue 'J' = Jack
toFaceValue 'Q' = Queen
toFaceValue 'K' = King

toSuit :: Char -> Suit
toSuit 'H' = Hearts
toSuit 'D' = Diamonds
toSuit 'S' = Spades
toSuit 'C' = Clubs

toCard :: String -> Card
toCard (face:suit:[]) = Card (toFaceValue face) (toSuit suit)

suits :: [Card] -> [Suit]
suits = map suit

faceValues :: [Card] -> [FaceValue]
faceValues = map faceValue

rank :: [Card] -> Rank
rank hand
  | isRoyalFlush hand    = RoyalFlush
  | isStraightFlush hand = StraightFlush (maximum values)
  | isFourOfAKind hand   = let v = findForCount 4 values
                               v' = head (allExcept v values)
                           in FourOfAKind v v'
  | isFullHouse hand     = let v = findForCount 3 values
                               v' = head (allExcept v values)
                           in FullHouse v v'
  | isFlush hand         = Flush (sortBy compare values)
  | isStraight hand      = Straight (maximum values)
  | isThreeOfAKind hand  = let v = findForCount 3 values
                               vs = allExcept v values
                           in ThreeOfAKind v vs
  | isTwoPairs hand      = let p1 = findForCount 2 values
                               p2 = findForCount 2 (allExcept p1 values)
                               v' = head (allExcept p2 (allExcept p1 values))
                           in TwoPairs p1 p2 v'
  | isOnePair hand       = let p = findForCount 2 values
                               vs = allExcept p values
                           in OnePair p vs
  | otherwise            = HighCard (sortBy (flip compare) values)
    where values = faceValues hand
          allExcept v vs = filter (/= v) vs
          findForCount n vs = v
            where Just (v, _) = find (\(_,c) -> c == n) (count vs)

isRoyalFlush :: [Card] -> Bool
isRoyalFlush hand = isStraightFlush hand && (faceValue . highCard) hand == Ace

isStraightFlush :: [Card] -> Bool
isStraightFlush hand = isStraight hand && isFlush hand

isFourOfAKind :: [Card] -> Bool
isFourOfAKind hand = 4 `elem` (map snd ((count . faceValues) hand))

isFullHouse :: [Card] -> Bool
isFullHouse hand = length valueCounts == 2 &&
                   minimum valueCounts == 2 &&
                   maximum valueCounts == 3
  where valueCounts = map snd (count (faceValues hand))

isFlush :: [Card] -> Bool
isFlush hand = and (map (==firstSuit) handSuits)
  where handSuits = suits hand
        firstSuit = handSuits !! 0

isStraight :: [Card] -> Bool
isStraight hand = and (map
                       (\(f1,f2) -> succ f1 == f2)
                       (zip (init sortedFaceValues) (tail sortedFaceValues)))
  where sortedFaceValues = sort (faceValues hand)

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind hand = or (map ((==3) . snd) ((count . faceValues) hand))

isTwoPairs :: [Card] -> Bool
isTwoPairs hand = or (map
                      (\(n, m) -> n==2 && m==2)
                      (count (map snd ((count . faceValues) hand))))

isOnePair :: [Card] -> Bool
isOnePair hand = or (map
                     (\(n, m) -> n==2 && m==1)
                     (count (map snd ((count . faceValues) hand))))

highCard :: [Card] -> Card
highCard = maximumBy (\c1 c2 -> compare (faceValue c1) (faceValue c2))

-- win h1 h2 evaluates to True if h1 is a better hand than h2, False otherwise
win :: [Card] -> [Card] -> Bool
win h1 h2 = rank h1 > rank h2

main = do
    handle <- openFile "../input/poker.txt" ReadMode
    contents <- hGetContents handle
    let rawCards = map words (lines contents)
    let cards = map (map toCard) rawCards
    let hands = map (splitAt 5) cards
    let ranks = map (\(h1, h2) -> (rank h1, rank h2)) hands
    let wins = map (uncurry win) hands
    print (take 5 hands)
    print (take 5 ranks)
    print (take 5 wins)
    print (foldl (\n win -> if win == True then n + 1 else n) 0 wins)
    hClose handle
