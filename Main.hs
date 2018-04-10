module Main where

import System.Random (newStdGen)
import Control.Monad.State
import Data.List (sortBy)
import Data.Char (ord)

import NoThanks

type NoThanksM a = StateT Game IO a
data Decision = Accept | Bet

-- Convenience function to putStrLn anything
ioPrint :: String -> NoThanksM ()
ioPrint = lift . putStrLn

gameLoop :: NoThanksM [Player]
gameLoop = do
    emptyDeck <- liftM null $ gets deck
    case emptyDeck of
        True  -> gets players
        False -> runGame >> gameLoop

prettyPrint :: [Player] -> [String]
prettyPrint ps = map pretty' ps
  where pretty' :: Player -> String
        pretty' p = name p ++ ": " ++ (show $ playerCards p) ++ ", " ++ (show $ markers p) ++ " markers, score: " ++ show (totalScore p)

runGame :: NoThanksM ()
runGame = do
    curr <- gets getCurrentPlayer
    let nm = name curr
        score = totalScore curr
    top    <- liftM show $ gets topCard
    ms     <- liftM show $ gets currentBet
    futureScore <- liftM show $ gets (totalScore . getCurrentPlayer . acceptCard)

    ioPrint $ "Current state:"
    gets players >>= mapM_ ioPrint . prettyPrint
    gets (length . deck) >>= \cs -> ioPrint $ "Cards left: " ++ (show cs)
    ioPrint ""

    case canBet curr of
        False -> do
            ioPrint $ nm ++ " has no markers, accepting card"
            ioPrint $ nm ++ " got " ++ top ++ " and " ++ ms ++ " markers"
            ioPrint ""
            modify acceptCard
        True -> do
            ioPrint $ nm ++ ": the top card is " ++ top ++ ", the bet is " ++ ms
            ioPrint $ nm ++ " score: " ++ (show score) ++ ", if accepting: " ++ futureScore
            c <- decidePrompt nm
            ioPrint ""
            case c of
                Bet    -> modify placeBet
                Accept -> modify acceptCard
  where
    decidePrompt nm = do
        ioPrint $ nm ++ ": bet (b) or accept (a)?"
        c <- lift $ getChar
        case c of
            'b' -> return Bet
            'a' -> return Accept
            _   -> do ioPrint ""
                      ioPrint "Invalid command, try again"
                      decidePrompt nm

getPlayers :: IO [String]
getPlayers = do
    putStr "Select number of players (2-5): "
    c <- getChar
    putStrLn ""
    case c `elem` ['2'..'5'] of
        True -> replicateM ((ord c) - 48) $ do
            putStr "Enter player name: "
            getLine
        False -> putStrLn "Invalid number, try again" >> getPlayers

main :: IO ()
main = do
    gen <- newStdGen
    players <- getPlayers
    let game = newGame gen players
    endGame <- execStateT gameLoop game
    putStrLn "*********"
    putStrLn "Game over"
    putStrLn "*********"
    forM_ (scoreList endGame) $ \(n,sc) -> putStrLn $ n ++ " final score: " ++ show sc

