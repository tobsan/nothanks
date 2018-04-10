module NoThanks (
    totalScore,
    playerCards,
    sumCards,

    Player(),
    name,
    cards,
    markers,

    Game(),
    currentBet,
    players,
    deck,

    scoreList,
    newGame,
    acceptCard,
    placeBet,
    topCard,
    getCurrentPlayer,
    canBet
  ) where

import System.Random.Shuffle
import System.Random
import Data.List

allCards :: [Int]
allCards = [3..35]

data Player = Player {
    name :: String,
    cards :: [Int],
    markers :: Int
} deriving Show

totalScore :: Player -> Int
totalScore (Player _ cs ms) = (sumCards cs) - ms

playerCards :: Player -> [[Int]]
playerCards (Player _ cs _) = groupCards cs

groupCards :: [Int] -> [[Int]]
groupCards xs = foldr comp [] $ sort xs
  where 
    comp :: Int -> [[Int]] -> [[Int]]
    comp x []     = [[x]]
    comp x (g:gs) | x+1 == head g = (x:g):gs
                  | otherwise     = [x]:(g:gs)

sumCards :: [Int] -> Int
sumCards xs = sum $ map head $ groupCards xs

newPlayer :: String -> Player
newPlayer n = Player n [] 11

modifyPlayer :: [Player] -> Int -> (Player -> Player) -> [Player]
modifyPlayer [] _ _     = []
modifyPlayer (p:ps) 0 f = f p : ps
modifyPlayer (p:ps) i f = p : modifyPlayer ps (i-1) f

canBet :: Player -> Bool
canBet (Player _ _ m) = m > 0

data Game = Game {
    deck :: [Int],
    unused :: [Int],
    players :: [Player],
    currentPlayer :: Int,
    currentBet :: Int
} deriving Show

scoreList :: Game -> [(String, Int)]
scoreList (Game _ _ ps _ _) = sortBy (\(_,a) (_,b) -> compare a b) $ map (\p -> (name p, totalScore p)) ps

getCurrentPlayer :: Game -> Player
getCurrentPlayer (Game _ _ ps curr _) = ps !! curr

topCard :: Game -> Int
topCard (Game d _ _ _ _) = head d

topCardValue :: Game -> Int
topCardValue (Game d _ _ _ b) = (head d) - b

acceptCard :: Game -> Game
acceptCard (Game d u ps cp cb) = Game (tail d) u ps' cp 0
  where
    ps' = modifyPlayer ps cp (\p -> addCard p (head d) cb)

    addCard :: Player -> Int -> Int -> Player
    addCard (Player nm cs ms) card bet = Player nm (card:cs) (ms+bet)

placeBet :: Game -> Game
placeBet (Game d u ps cp cb) = Game d u ps' cp' (cb+1)
  where
    ps' = modifyPlayer ps cp (\(Player nm cs ms) -> Player nm cs (ms -1))
    cp' = (cp + 1) `mod` (length ps)

newGame :: StdGen -> [String] -> Game
newGame gen names = Game (drop 9 cards) (take 9 cards) (map newPlayer names) 0 0
  where cards = shuffle' allCards (length allCards) gen
