module AppState where

import Tak(Stack, Board, Player(..), Placement(..), placePiece, emptyBoard, invertPlayer, Piece(..))
import Notation(Turn(..))


data AppState = AppState {
  history :: CommandHistory
  , gameState :: GameState
}


data CommandHistory = CommandHistory {
  commands :: [String]
  , index :: Int
} deriving Show

emptyCommandHistory :: CommandHistory
emptyCommandHistory = CommandHistory [] 0

data GameState = GameState {
  currentTurn :: Player,
  board       :: Board,
  turns       :: [Turn]
}