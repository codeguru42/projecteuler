-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 54

import System.IO(openFile, hGetContents, hClose, IOMode(ReadMode))
import Data.List(maximumBy, sort, sortBy)

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

data Rank = Rank RankOrd RankInfo
            deriving (Eq)

instance Show Rank
         where show (Rank HighCardOrd (HighCardInfo (v:vs))) =
                 (show v) ++ " high"
               show (Rank OnePairOrd (OnePairInfo p (v:vs))) =
                 "Pair of " ++ (show p) ++ "s with " ++ (show v) ++ " high"
               show (Rank TwoPairsOrd (TwoPairsInfo p1 p2 v)) =
                 (show p1) ++ "s and " ++ (show p2) ++ "s"
               show (Rank ThreeOfAKindOrd (ThreeOfAKindInfo t v)) =
                 "Three " ++ (show t) ++ "s"
               show (Rank StraightOrd (StraightInfo high)) =
                 "Straight " ++ (show high) ++ " high"
               show (Rank FlushOrd (FlushInfo high)) =
                 "Flush " ++ (show high) ++ " high"
               show (Rank FullHouseOrd (FullHouseInfo v1 v2)) =
                 (show v1) ++ " full of " ++ (show v2) ++ "s"
               show (Rank FourOfAKindOrd (ThreeOfAKindInfo t v)) =
                 "Four " ++ (show t) ++ "s"
               show (Rank StraightFlushOrd (StraightFlushInfo high)) =
                 "Straight flush " ++ (show high) ++ " high"
               show (Rank RoyalFlushOrd RoyalFlushInfo) =
                 "Royal flush"

instance Ord Rank
         where compare 
                 (Rank HighCardOrd (HighCardInfo v1s))
                 (Rank HighCardOrd (HighCardInfo v2s)) = compare v1s v2s
--               compare (Rank OnePairOrd (OnePairInfo p (v:vs))) =
--               compare (Rank TwoPairsOrd (TwoPairsInfo p1 p2 v)) =
--               compare (Rank ThreeOfAKindOrd (ThreeOfAKindInfo t v)) =
--               compare (Rank StraightOrd (StraightInfo high)) =
--               compare (Rank FlushOrd (FlushInfo high)) =
--               compare (Rank FullHouseOrd (FullHouseInfo v1 v2)) =
--               compare (Rank FourOfAKindOrd (ThreeOfAKindInfo t v)) =
--               compare (Rank StraightFlushOrd (StraightFlushInfo high)) =
--               compare (Rank RoyalFlushOrd (RoyalFlushInfo high)) =

data RankOrd = HighCardOrd
             | OnePairOrd
             | TwoPairsOrd
             | ThreeOfAKindOrd
             | StraightOrd
             | FlushOrd
             | FullHouseOrd
             | FourOfAKindOrd
             | StraightFlushOrd
             | RoyalFlushOrd
             deriving (Show, Eq)

data RankInfo = HighCardInfo [FaceValue]
                -- HighCard vs where vs is a list of the face values of the
                -- cards in descending order
              | OnePairInfo FaceValue [FaceValue]
                -- OnePair v [v1,v2,v3] is a pair of vs with v1 high, v2 next,
                -- and v3 low
              | TwoPairsInfo FaceValue FaceValue FaceValue
                -- TwoPairs v1 v2 v' is a pair of v1s and a pair of v2s 
                -- (with v1 > v2) and a v'
              | ThreeOfAKindInfo FaceValue [FaceValue]
                -- ThreeOfAKind v [v1,v2] has three vs with v1 high and v2 low
              | StraightInfo FaceValue
                -- Straght v has high card v
              | FlushInfo [FaceValue]
                -- Flush vs where vs is a list of the face values of the cards
                -- in descending order
              | FullHouseInfo FaceValue FaceValue
                -- FullHouse v1 v2 is v1 full of v2
              | FourOfAKindInfo FaceValue FaceValue 
                -- FourOfAKind v v' has four vs with v' kicker
              | StraightFlushInfo FaceValue
                -- StraightFlush v has high card v
              | RoyalFlushInfo
              deriving (Show, Eq)

deck :: [Card]
deck = [Card face suit | 
        suit <- [Hearts, Spades, Diamonds, Clubs],
        face <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]

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
  | isRoyalFlush hand    = Rank RoyalFlushOrd RoyalFlushInfo
  | isStraightFlush hand = Rank
                           StraightFlushOrd
                           (StraightFlushInfo (maximum values))
  | isFourOfAKind hand   = let v = 1
                               v' = 1 
                           in Rank FourOfAKindOrd (FourOfAKindInfo v v')
  | isFullHouse hand     = let v = 1
                               v' = 1
                           in Rank FullHouseOrd (FullHouseInfo v v')
  | isFlush hand         = Rank FlushOrd (FlushInfo (sortBy compare values))
  | isStraight hand      = Rank StraightOrd (StraightInfo (maximum values))
  | isThreeOfAKind hand  = let v = 1
                               vs = 1
                           in Rank ThreeOfAKindOrd (ThreeOfAKindInfo v vs)
  | isTwoPairs hand      = let p1 = 1
                               p2 = 1
                               v' = 1
                           in Rank TwoPairsOrd (TwoPairsInfo p1 p2 v')
  | isOnePair hand       = let p = 1
                               vs = 1
                           in Rank OnePairOrd (OnePairInfo p vs)
  | otherwise            = Rank 
                           HighCardOrd 
                           (HighCardInfo (sortBy compare values))
    where values = faceValues hand
          countedValues = count values

isRoyalFlush :: [Card] -> Bool
isRoyalFlush hand = isStraightFlush hand && (faceValue . highCard) hand == Ace

isStraightFlush :: [Card] -> Bool
isStraightFlush hand = isStraight hand && isFlush hand

isFourOfAKind :: [Card] -> Bool
isFourOfAKind hand = or (map ((==4) . snd) (count hand))

isFullHouse :: [Card] -> Bool
isFullHouse hand = False

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
isThreeOfAKind hand = or (map ((==3) . snd) (count hand))

isTwoPairs :: [Card] -> Bool
isTwoPairs hand = or (map
                      (\(n, m) -> n==2 && m==2)
                      (count (map snd (count hand))))

isOnePair :: [Card] -> Bool
isOnePair hand = or (map
                     (\(n, m) -> n==2 && m==1)
                     (count (map snd (count hand))))

highCard :: [Card] -> Card
highCard = maximumBy (\c1 c2 -> compare (faceValue c1) (faceValue c2))

count :: (Eq a) => [a] -> [(a, Integer)]
count [] = []
count (x:[]) = [(x, 1)]
count (x:xs) 
  | x `elem` xs = map 
                  (\(y,n) -> if y == x then (y, n + 1) else (y, n)) 
                  (count xs)
  | otherwise = (x, 1) : (count xs)

filterRank :: Rank -> [((Rank, Rank), Integer)] -> [((Rank, Rank), Integer)]
filterRank (Rank ord _) = 
  filter (\((Rank ord1 _, Rank ord2 _), _) -> ord1 == ord || ord2 == ord)

-- win h1 h2 evaluates to True if h1 is a better hand than h2, False otherwise
win :: [Card] -> [Card] -> Bool
win h1 h2 = rank h1 > rank h2

main = do
    handle <- openFile "input/poker.txt" ReadMode
    contents <- hGetContents handle
    let rawCards = map words (lines contents)
    let cards = map (map toCard) rawCards
    let hands = map (splitAt 5) cards
    let ranks = map (\(h1, h2) -> (rank h1, rank h2)) hands
    let indexedRanks = zip ranks [1..]
    print (take 5 ranks)
    print (count (suits deck))
    print (count (faceValues deck))
    hClose handle
