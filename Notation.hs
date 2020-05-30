module Notation where
  
import Text.Parsec.Char (char)
import Text.Parsec (Parsec)
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Tak(Pos, Placement(..), Piece(..), Movement(..), Direction(..))
import Data.Maybe
-- 1. a6 f6
-- 2. d4 c4
-- 3. d3 c3
-- 4. d5 c5
-- 5. d2 Ce4
-- 6. c2 e3
-- 7. e2 b2
-- 8. Cb3 1e4<1
-- 9. 1d3<1 Sd1
-- 10. a3 1d1+1
-- ...

-- charAParser :: Parsec String 
-- charAParser = (char 'a')

location :: GenParser Char st Pos
location = do
  file <- oneOf ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
  rank <- oneOf ['1', '2', '3', '4', '5', '6', '7', '8']
  return (read [rank] :: Int, file)

parsePiece :: GenParser Char st Piece
parsePiece = do 
  piece <- optionMaybe $ oneOf ['C', 'S', 'F']
  return (fromMaybe FlatStone (fmap toPiece piece))
    where toPiece c = case c of
                        'C' -> CapStone
                        'S' -> StandingStone
                        'F' -> FlatStone

parseDirection :: GenParser Char st Direction
parseDirection = do
  dir <- oneOf ['<', '>', '+', '-', '↑', '↓', '←', '→']
  return (case dir of
            '<' -> DLeft
            '>' -> DRight
            '+' -> DUp
            '-' -> DDown 
            '↑' -> DUp
            '↓' -> DDown
            '←' -> DLeft
            '→' -> DRight)
-- Moves

charToInt :: Char -> Int
charToInt c = read [c] :: Int

getOrElse :: a -> Maybe a -> a
getOrElse _ (Just x) = x
getOrElse x Nothing  = x

parsePlacement :: GenParser Char st Placement
parsePlacement = do
  piece <- parsePiece
  loc   <- location
  return (Placement piece loc)

parseMovement :: GenParser Char st Movement
parseMovement = do
  takeN <- optionMaybe (oneOf ['1', '2', '3', '4', '5', '6', '7', '8'])
  loc   <- location
  dir   <- parseDirection
  dropN <- optionMaybe (many1 (oneOf ['1', '2', '3', '4', '5', '6', '7', '8']))
  return (Movement loc (getOrElse 1 (fmap charToInt takeN)) dir (getOrElse [1] (fmap (fmap charToInt) dropN) ))

type Turn = Either Placement Movement

parseTurn :: GenParser Char st Turn
parseTurn = (fmap Left parsePlacement) <|> (fmap Right parseMovement)

-- 5b4>212

