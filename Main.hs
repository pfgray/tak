import System.IO
import Data.List

import System.Console.ANSI
import Control.Monad.State.Lazy
import Tak(Stack, Board, Player(..), Placement(..), placePiece, emptyBoard, invertPlayer, Piece(..))
import AppState(AppState, GameState(..))
import Program(Program, printState, printBoard, applyTurn, initialState, addHistoryCommand)
import Notation(parseTurn, Turn)
-- import qualified Tak(Player(White, Black)) as Foo(White, Black)
import Text.ParserCombinators.Parsec

import System.Console.Readline

mainLoop :: Program ()
mainLoop = do 
  command <- liftIO takeCommand
  case command of
    Exit -> return ()
    Echo s -> (liftIO $ putStrLn s) >>= (\_ -> mainLoop)
    Unrecognized s -> (liftIO $ putStrLn ("\"" ++ s ++ "\" is not a valid command")) >>= (\_ -> mainLoop)
    Move t -> applyTurn t >>= (\_ -> mainLoop)
    Show   -> get >>= (\s -> liftIO $ putStrLn (printState s) ) >>= (\_ -> mainLoop)
    Empty  -> mainLoop

data Position = Position Int String

-- groupByValue :: Eq a => (b -> a) -> [b] -> [[b]]
-- groupByValue f bs = 

main :: IO ()
main = () <$ (runStateT mainLoop (initialState 3))

takeCommand :: IO Command
takeCommand = do
  hSetBuffering stdout NoBuffering
  setSGR [SetColor Foreground Vivid Blue]
  maybeLine <- readline "τ> "
  setSGR [Reset]
  case maybeLine of
    Nothing -> return Exit
    Just line -> do addHistory line
                    return $ parseCommand line

data Command =
  Exit
  | Echo String
  | Unrecognized String
  | Empty
  --  | StartGame Integer
  | Move Turn
  | Show
  deriving Show

parseCommand :: String -> Command
parseCommand s = 
  case parse (optionMaybe (parseExit <|> parseShow <|> parseMove <|> parseEmpty)) "command" s of
    (Left _)           -> Unrecognized s
    (Right Nothing)    -> Unrecognized s
    (Right (Just cmd)) -> cmd
  -- if s == "exit" then Exit 
  -- else if (isPrefixOf "echo " s) then Echo (drop 5 s)
  -- -- else if (isPrefixOf "start " s) then Echo (drop 5 s)
  -- else if (isPrefixOf "move " s) then Move $ Turn (Placement FlatStone (1, 'a'))
  -- else if (isPrefixOf "show " s) then Show
  -- else Unrecognized s

parseExit :: GenParser Char st Command
parseExit = Exit <$ string "exit"

parseShow :: GenParser Char st Command
parseShow = Show <$ string "show"

parseMove :: GenParser Char st Command
parseMove = do
  _ <- string "move "
  _ <- spaces
  t <- parseTurn
  return (Move t)

parseEmpty :: GenParser Char st Command
parseEmpty = Empty <$ spaces
