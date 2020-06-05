module Tak where
-- This week's homework has three parts.  The first part will be a few warmups to get used to
-- working with lists and tuples.  The second will be to adapt the tak 'Stack' manipulation
-- functions from last week to use 'Maybe' to indicate failure (rather than exploding with @error@).
-- Third, we'll start looking at how we might implement the board for our tak game.

import Data.List
import Data.Monoid
import Data.Tuple

import Debug.Trace

-- Let's find the index of an element in a list (i.e., the position of the first occurence)---the
-- element might not be there:
--
-- >>> index 4 [3,1,4,5]
-- Just 2
--
-- >>> index 9 [3,1,4,5]
-- Nothing
--
-- NB: Remember that @Eq a@ means that you can use @(==)@ to compare values of type @a@.
index :: Eq a => a -> [a] -> Maybe Int
index a xs = go 0 xs
  where go _ []     = Nothing
        go i (x:xs) =
          if x == a then Just i else go (i + 1) xs


-- An "association list" or "alist" is a list of @(key, value)@ pairs, providing a basic means of
-- associating a value with a given key.  As with 'index' above, a given key might not be present in
-- the alist, and so looking it up might fail.
--
-- >>> lookupAlist "foo" [("bar", 3), ("baz", 1), ("foo", 4)]
-- Just 4
--
-- >>> lookupAlist "quux" [("bar", 3), ("baz", 1), ("foo", 4)]
-- Nothing
--
-- NB: Notice how we're relying on as /little/ information as required to implement the function.
-- We have to be able to compare keys, so we need @Eq k@, but we don't actually care what the type
-- of @k@ or @v@ is.  We know that we may fail to find a @v@, so we are compelled to give back a
-- @Maybe v@, just in case there's no matching key in the alist.  If you meditate on the type for a
-- bit, you can pretty much figure out exactly what 'lookupAlist' is going to do even without the
-- description above.
lookupAlist :: Eq k => k -> [(k, v)] -> Maybe v
lookupAlist k []              = Nothing
lookupAlist k ((k', v'):rest) =
  if k == k' then Just v' else lookupAlist k rest

-- You may be familiar with a function named 'zip'.  Here's its type:
--
-- zip :: [a] -> [b] -> [(a, b)]
--
-- That type should be enough to tell you what 'zip' does.
--
-- Try writing 'index' again using 'zip' and 'lookupAlist'.
index' :: Eq a => a -> [a] -> Maybe Int
index' a xs = lookupAlist a (zip xs [0..])


-- Part 2, Maybe-ify last week's code

data Player = PWhite | PBlack
  deriving (Eq, Ord, Show)

invertPlayer :: Player -> Player
invertPlayer PWhite = PBlack
invertPlayer PBlack = PWhite

data Stack
  = Top
  | Cap   Player
  | Stand Player
  | Flat  Player Stack
    deriving (Eq, Ord, Show)

data Piece
  = CapStone
  | StandingStone
  | FlatStone
    deriving (Eq, Ord, Show)

-- | Create a 'Stack' containing a single 'Piece' owned by the given 'Player'.
buildStack :: Player -> Piece -> Stack
buildStack player CapStone      = Cap player
buildStack player StandingStone = Stand player
buildStack player FlatStone     = Flat player Top

-- | Given two stacks, place the second one atop the first:
--
-- >>> stackStack (Flat PBlack Top) (Flat PWhite Top)
-- Just (Flat PBlack (Flat PWhite Top))
--
-- >>> stackStack (Cap PBlack) (Flat PWhite Top)
-- Nothing
stackStack :: Stack -> Stack -> Maybe Stack
stackStack Top st              = Just st
stackStack (Flat p st1) st2    = fmap (\s -> Flat p s) (stackStack st1 st2)
stackStack (Stand p1) (Cap p2) = Just (Flat p1 (Cap p2))
stackStack (Cap _) _           = Nothing
stackStack (Stand _) _         = Nothing

stackHeight :: Stack -> Int
stackHeight Top         = 0
stackHeight (Cap _)     = 1
stackHeight (Stand _)   = 1
stackHeight (Flat _ st) = 1 + stackHeight st

-- | Create the 'Stack' with the given number of stones taken from the top of the given stack:
--
-- >>> takeStack 2 (Flat PWhite (Flat PBlack (Cap PBlack)))
-- Just (Flat PBlack (Cap PBlack))
--
-- >>> takeStack 2 (Cap PBlack)
-- Nothing
-- takeStack :: Int -> Stack -> Maybe Stack
-- takeStack m stack = go (stackHeight stack - m) stack
--   where go 0 st = Just st
--         go n (Flat _ st) = go (n - 1) st
--         go _ _ = Nothing

-- hmm no bifunctor in core?
leftMap :: (a -> z) -> (a, b) -> (z, b)
leftMap f (x, y) = (f x, y)

takeStack :: Int -> Stack -> Maybe (Stack, Stack)
takeStack m stack = go (stackHeight stack - m) stack
  where go 0 st = Just (Top, st)
        go n (Flat p st) = fmap (leftMap (\s -> Flat p s)) (go (n - 1) st)
        go _ _ = Nothing


-- | Remove the given number of stones from the given 'Stack':
--
-- >>> dropStack 2 (Flat PWhite (Flat PBlack (Cap PBlack)))
-- Just (Flat PWhite Top)
--
-- >>> dropStack 2 (Cap PBlack)
-- Nothing
dropStack :: Int -> Stack -> Maybe (Stack, Stack)
dropStack 0 s           = Just (Top, s)
dropStack 1 (Cap p)     = Just (Cap p, Top)
dropStack 1 (Stand p)   = Just (Stand p, Top)
dropStack 1 (Flat p st) = Just (Flat p Top, st)
dropStack n (Flat p st) = fmap (\(d, s) -> (Flat p d, s)) (dropStack (n - 1) st)
dropStack _ _           = Nothing

-- Part 3, A Simple Tak Board

-- We've seen the syntax for creating new data types (@data@).  There is also syntax for creating
-- type aliases:

type Row    = Int
type Column = Char

-- These mean that I can write @Row@ in a type signature, and it would be /the same/ as if I had
-- written @Int@.  This is a useful feature because it allows us to use functions that work on data
-- we already have but wish to interpret in more specific way.  You've already seen this:
--
-- type String = [Char]

-- We can use these types immediately to describe a position on the Tak board:

type Pos = (Row, Column)

-- RANT:
-- 'Pos' is mildly annoying as, so far as I can tell, people don't like thinking about positions
-- on a grid-like board as @(Column, Row)@, even though that's how chess notation ("knight takes
-- b2") and x-y coordinates work.  So if you prefer @type Pos = (Column, Row)@, we can switch, but I
-- feel like it's counter-intuitive for one reason or another either way, so I flipped a coin.
-- END RANT
--
-- Anyway, let's think of a couple operations we'd like to do on our Tak board---I'm going to refer
-- to a type 'Board' that we'll describe below, but we can talk about the operations first.  Read
-- through the following operations we're going to implement first, and then choose which
-- representation of a 'Board' you'd prefer.

-- | Retrieve the 'Stack' at the given position on the 'Board', failing if the position is out of
-- bounds.
getPos :: Pos -> Board -> Maybe Stack
getPos = lookupAlist 

-- | Unconditionally update the 'Board' to hold the given 'Stack' at the given 'Pos', failing if the
-- position is out of bounds.
--
-- NB: "updating" here means producing a new Board that's just like the old one, except with the
-- stack in the appropriate position.
updatePos :: Pos -> Stack -> Board -> Maybe Board
updatePos pos stack b = 
  case foldr go ([], False) b of
       (_, False) -> Nothing
       (board, _) -> Just board
  where go :: (Pos, Stack) -> (Board, Bool) -> (Board, Bool)
        go  (pos', stack') (board, found) =
          if pos' == pos then ((pos, stack):board, True) else ((pos, stack'):board, found)

files :: String
files = "abcdefgh"

-- | Create an empty 'Board' of the given size, i.e., one for which 'getPos' gives back 'Just Top'
-- for all in-bounds positions.  Fail when the given dimension doesn't correspond to a legal tak
-- board.  The legal sizes are 3 through 8.
emptyBoard :: Int -> Board
emptyBoard n =
  if n < 3 then emptyBoard 3
  else if n > 8 then emptyBoard 8
  else do
    file <- (take n files)
    rank <- ([1..n])
    [((rank, file), Top)]

-- without do:
emptyBoard' :: Int -> Maybe Board
emptyBoard' n
  | n < 3 || n > 8 = Nothing 
  | otherwise = Just board
    where board = take n files >>= (\file -> [1..n] >>= (\rank -> [((rank, file), Top)]))

-- | A 'Placement' represents placing a stone on an unoccupied position on the board.  (As opposed
-- to a @Movement@, which we'll work on next week).
data Placement = Placement Piece Pos
  deriving (Eq, Ord, Show)

data Direction = DUp | DRight | DDown | DLeft
  deriving (Eq, Ord, Show)

data Movement = Movement {
  from :: Pos
  , takeStones :: Int
  , direction :: Direction 
  , drops :: [Int]
} deriving (Eq, Ord, Show)

-- | Place a piece on the 'Board', failing if the position is already occupied /or/ is out of
-- bounds.
placePiece :: Player -> Placement -> Board -> Maybe Board
placePiece _ _ [] = Nothing
placePiece player place@(Placement piece pos) ((pos', stack): restBoard) =
  case (pos == pos', stack) of
    (False, _) -> fmap ((pos', stack):) (placePiece player place restBoard)
    (True, Top) -> Just ((pos, buildStack player piece) : restBoard)
    (True, _) -> Nothing

owner :: Stack -> Maybe Player
owner Top          = Nothing
owner (Cap p)      = Just p
owner (Stand p)    = Just p
owner (Flat p Top) = Just p
owner (Flat _ s)   = owner s

  -- = Top
  -- | Cap   Player
  -- | Stand Player
  -- | Flat  Player Stack

-- take n stones from a stack, and drop

toEither :: e -> Maybe a -> Either e a
toEither e Nothing = Left e
toEither _ (Just a) = Right a

type TurnResult t = Either String t

stackAt :: Pos -> Board -> TurnResult Stack
stackAt p b = fmap snd $ toEither ("Position " ++ show p ++ "  doesn't exit") $ find (\(pos,_) -> pos == p) b

posExists :: Pos -> Board -> Bool
posExists p b = getAny $ foldMap ((\p' -> Any (p' == p)) . fst) b

posIsOwnedBy :: Pos -> Board -> Player -> TurnResult ()
posIsOwnedBy p b player = do
  s <- stackAt p b
  o <- toEither ("Position " ++ (show p) ++ " is not owned by anyone")  (owner s)
  if o == player
    then return ()
    else Left ("Position " ++ (show p) ++ " is not owned by " ++ (show player))

modifyStackAt :: Pos -> Board -> (Stack -> TurnResult Stack) -> TurnResult Board
modifyStackAt p b f = do
  s <- stackAt p b
  s' <- f s
  return (replaceStack p b s')

replaceStack :: Pos -> Board -> Stack -> Board
replaceStack p b s = fmap go b
  where go (p', s') = if p' == p then (p, s) else (p', s')

moveRight :: Char -> Maybe Char
moveRight 'a' = Just 'b'
moveRight 'b' = Just 'c'
moveRight 'c' = Just 'd'
moveRight 'd' = Just 'e'
moveRight 'e' = Just 'f'
moveRight 'f' = Just 'g'
moveRight 'g' = Just 'h'
moveRight 'h' = Nothing

moveLeft :: Char -> Maybe Char
moveLeft 'a' = Nothing
moveLeft 'b' = Just 'a'
moveLeft 'c' = Just 'b'
moveLeft 'd' = Just 'c'
moveLeft 'e' = Just 'd'
moveLeft 'f' = Just 'e'
moveLeft 'g' = Just 'f'
moveLeft 'h' = Just 'g'

moveDirection :: Pos -> Direction -> Maybe Pos
moveDirection p DDown  = Just $ swap $ fmap (\x -> x-1) (swap p)
moveDirection p DUp    = Just $ swap $ fmap (+1) (swap p)
moveDirection p DLeft  = fmap (\r -> (fst p, r)) (moveLeft $ snd p)
moveDirection p DRight = fmap (\r -> (fst p, r)) (moveRight $ snd p)

applyMovement :: Player -> Movement -> Board -> TurnResult Board
applyMovement pl m b = do
  _  <- posIsOwnedBy (from m) b pl
  s  <- stackAt (from m) b
  (d, s') <- toEither (takeError m) (takeStack (takeStones m) s)
  -- newPosition <- toEither ("couldnt move " ++ (show (direction m)) ++ " from " ++ (show $ from m) ) (moveDirection (from m) (direction m))
  go (from m) (replaceStack (from m) b d) s' (drops m)
  where go :: Pos -> Board -> Stack -> [Int] -> TurnResult Board
        go _ b _ []      = return b
        go prevP b taken (d:ds)  = do
          p   <- toEither ("couldnt move " ++ (show (direction m)) ++ " from " ++ (show prevP)) (moveDirection prevP (direction m)) -- don't move if it's the last step...
          _  <- if posExists p b then Right () else Left (dropError "pos doesn't exist" d taken p)
          dropOn <- stackAt p b
          (droppedPieces, stackLeft) <- toEither (dropError "couldn't drop pieces" d taken p) $ dropStack d taken
          newStack <- toEither (dropError "couldn't drop on pieces" d taken p) $ stackStack dropOn droppedPieces
          go p (replaceStack p b newStack) stackLeft ds



dropError :: String -> Int -> Stack -> Pos -> String
dropError cause n s p = " " ++ (show n) ++ " stones from stack: " ++ (show s) ++ " onto position: " ++ (show p) ++ " cause" ++ cause

takeError :: Movement ->  String
takeError m = "Cannot take" ++ (show $ takeStones m) ++ " from position: " ++ (show $ from m)
-- posIsOwnedBy
-- replace stack at pos by taking n pieces
-- for each drop:
--   get new position at Direction
--   replace stack at new position by adding

type Board = [(Pos, Stack)]

data Move =
  Place Pos
  | Move Int Direction [Int]