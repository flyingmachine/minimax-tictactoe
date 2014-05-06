-- Created 14-05-01 by Luis Gerhorst http://luisgerhorst.de

import qualified Data.Sequence as Seq
import Text.Read


data Player = O | Neither | X deriving (Show, Eq, Ord)
oppositePlayer X = O
oppositePlayer O = X

type Board = Seq.Seq Player -- use seq instead of list because we'll
             -- do a lot of indexing, its not just a collection of values
emptyBoard = Seq.replicate 9 Neither


-- interface

main = gameLoop

gameLoop :: IO ()
gameLoop = do
  boardWinner <- movesLoop emptyBoard X
  putStrLn $ case boardWinner of
    X -> "X won!"
    O -> "O won!"
    Neither -> "It was a draw!"
  putStrLn "Play again y/n"
  answer <- getLine
  case answer of
    "y" -> gameLoop
    _ -> return ()

movesLoop :: Board -> Player -> IO Player
movesLoop board currentPlayer = do
  board' <- case currentPlayer of
    X -> do putStrLn "\n==============="
            putStrLn "X's move:"
            let board' = nextMove board
            putStr $ boardString board'
            return board'
    O -> do board' <- humanMove board
            putStrLn "The result of your move:"
            putStr $ boardString board'
            putStrLn ""
            return board'
  case boardWinner board' of
    Nothing -> movesLoop board' $ oppositePlayer currentPlayer
    Just player -> return player

humanMove :: Board -> IO Board
humanMove board = do
  putStrLn "Enter square # to place your 'O' in:"
  numberStr <- getLine
  move (readMaybe numberStr :: Maybe Int)
  where move (Just nth)
          | index <= 8 &&
            index >= 0 &&
            (Seq.index board index) == Neither = do
              return $ Seq.update index O board
          where index = nth - 1
        move _ = do putStrLn "That's not a valid move"
                    humanMove board

boardString :: Board -> String
boardString b =
  Seq.foldlWithIndex appender [] b
  where appender s i x | i == 8           = s ++ playerStr x i ++ " \n"
        appender s i x | i == 2 || i == 5 = s ++ playerStr x i ++ "\n-----------\n"
        appender s i x                    = s ++ playerStr x i ++ "|"
        playerStr Neither i = " " ++ show (i + 1) ++ " "
        playerStr x _    = " " ++ show x ++ " "
                
-- end interface


-- Nothing if game has not ended yet
-- Just Player if game is over
boardWinner :: Board -> Maybe Player
boardWinner b =
  if gameOver || ownsCombination /= Neither
  then Just ownsCombination
  else Nothing
  where gameOver = Seq.null $ Seq.filter (== Neither) b
        ownsCombination = case filter (/= Neither) $ map getCombinationOwner combinations of
          [] -> Neither
          (player:_) -> player
        getCombinationOwner indexes = if allTheSame xs then head xs else Neither
          where xs = map (Seq.index b) indexes
                allTheSame xs = and $ map (== head xs) (tail xs)
        combinations = [[0, 1, 2],
                        [3, 4, 5],
                        [6, 7, 8],
                        [0, 3, 6],
                        [1, 4, 7],
                        [2, 5, 8],
                        [0, 4, 8],
                        [6, 4, 2]]

nextMove :: Board -> Board
nextMove b | b == emptyBoard = Seq.update 0 X b -- just a shortcut for
                               -- better performance
nextMove b = b'
  where GameState b' _ _ = maximum moves
        GameState _ moves _ = gameTree X b


-- Board: board after current player made his move
-- [GameState]: Possible moves the next player can do
-- Player: winner of this game path
data GameState = GameState Board [GameState] Player deriving (Show, Eq)

instance Ord GameState where
  compare (GameState _ _ p1) (GameState _ _ p2) = compare p1 p2 -- compare the winners of the game states


gameTree :: Player -> Board -> GameState
gameTree p b = GameState b moves winner
  where moves = Seq.foldlWithIndex canInsert [] b
        canInsert ns i Neither = (gameTree (oppositePlayer p) (Seq.update i p b)):ns
        canInsert ns i _ = ns
        winner = case boardWinner b of
          Nothing -> case p of -- intermediate game state
            -- players are ordered by who should win, X > Neither > O
            -- comparing GameStates compares the winners of the GameStates
            X -> let GameState _ _ winner = maximum moves in winner
            O -> let GameState _ _ winner = minimum moves in winner
          Just winner -> winner
