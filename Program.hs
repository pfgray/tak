module Program where

import Control.Monad.State.Lazy
import AppState(AppState(..), GameState(..), CommandHistory(..), emptyCommandHistory)
import Tak(Stack, Board, Player(..), Placement(..), placePiece, applyMovement, emptyBoard, invertPlayer, Piece(..), TurnResult)
import Notation(Turn(..))

type Program a = StateT AppState IO a

getOrElse :: a -> Maybe a -> a
getOrElse a Nothing = a
getOrElse _ (Just a) = a

applyTurn :: Turn -> Program ()
applyTurn (Left turn) = do
  st       <- get
  let s = gameState st
      newTurn = invertPlayer (currentTurn s)
      newBoardO = (placePiece (currentTurn s) turn (board s))
  case newBoardO of
    (Nothing) ->  liftIO $ putStrLn $ "Couldn't place a piece there..."
    (Just newBoard) -> do
      _ <- liftIO $ putStrLn $ printBoard newBoard
      _ <- put (AppState (history st) (GameState newTurn newBoard ((Left turn) : (turns s))))
      return ()
applyTurn (Right movement) = do
  st       <- get
  let s = gameState st
      newTurn = invertPlayer (currentTurn s)
      newBoardE = (applyMovement (currentTurn s) movement (board s))
  case newBoardE of
    (Left err) -> (liftIO $ putStrLn err)
    (Right newBoard) -> do
      _ <- liftIO $ putStrLn $ printBoard $ newBoard
      _ <- put (AppState (history st) (GameState newTurn newBoard ((Right movement) : (turns s))))
      return ()

handleTurnResult :: a -> TurnResult a -> Program a
handleTurnResult a (Left err) = do 
  _ <- (liftIO $ putStrLn err)
  return a
handleTurnResult _ (Right a) = return a

getOrElseE :: z -> Either b z -> z
getOrElseE z (Left _) = z
getOrElseE _ (Right z) = z

printBoard :: Board -> String
printBoard b = "\n" ++ (foldMap go b)
  where go :: ((Int, Char), Stack) -> String
        go ((r, c), s) = [c] ++ (show r) ++ " " ++ " " ++ (show s) ++ "\n"

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
