module Program where

import Control.Monad.State.Lazy
import AppState(AppState(..), GameState(..), CommandHistory(..), emptyCommandHistory)
import Tak(Stack, Board, Player(..), Placement(..), placePiece, emptyBoard, invertPlayer, Piece(..))
import Notation(Turn(..))

type Program a = StateT AppState IO a

getOrElse :: a -> Maybe a -> a
getOrElse a Nothing = a
getOrElse _ (Just a) = a

applyTurn :: Turn -> Program ()
applyTurn (Left turn) = modify go
  where go :: AppState -> AppState
        go st =
          let s = gameState st
              newBoard = getOrElse (board s) (placePiece (currentTurn s) turn (board s))
              newTurn = invertPlayer (currentTurn s)
          in AppState (history st) (GameState newTurn newBoard ((Left turn) : (turns s)))
applyTurn (Right movement) = modify id


printBoard :: Board -> String
printBoard b = "\n" ++ (foldMap go b)
  where go :: ((Int, Char), Stack) -> String
        go ((r, c), s) = (show r) ++ " " ++ [c] ++ " " ++ (show s) ++ "\n"

printState :: AppState -> String
printState as = (printGameState $ gameState as)

printGameState :: GameState -> String
printGameState gs = "Board\n  " ++ printBoard (board gs) ++ "\n\nTurn\n" ++ (show (currentTurn gs)) ++ "\n\nturns\n" ++ (show (turns gs))

initialGameState :: Int -> GameState
initialGameState n = GameState PWhite (emptyBoard n) []

initialState :: Int -> AppState
initialState n = AppState emptyCommandHistory (initialGameState n)


-- would like a lens here
addHistoryCommand :: String -> Program ()
addHistoryCommand s = modify go
  where go :: AppState -> AppState
        go as = 
          let h = history as 
            in AppState (goo h) (gameState as)
            where goo :: CommandHistory -> CommandHistory
                  goo (CommandHistory h _) = CommandHistory (s:h) 0 
