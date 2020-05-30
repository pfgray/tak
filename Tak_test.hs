
-- ######
-- Pieces/Board
-- ######
data Color = White | Black
data PieceType = Stone | Capstone
data Piece = Piece {
    color  :: Color
  , typ    :: PieceType
}
type Hand = [Piece]
type BoardSize = Int
data Point = Point {
    x :: Int
  , y :: Int
}

-- ######
-- Game State
-- ######
type SquareState = [Piece]
type BoardState = [[SquareState]]
data GameState = GameState {
    whiteHand   :: Hand
  , blackHand   :: Hand
  , board       :: BoardState
  , currentTurn :: Color
}

-- ######
-- Movement/Turns
-- ######
data StonePlacementType = Wall | Flat
data Placement = StonePlacement {
    point         :: Point
  , placementType :: StonePlacementType
} | CapstonePlacement {
  point :: Point
}
data Direction = Up | Down | Left | Right
data Move = Move {
    direction :: Direction
  , drop      :: Int
}
data Turn = Placement | Movement [Move]


-- ######
-- Errors, todo: fill this out with explicit errors
-- ######
type Error = String


-- ######
-- Potentially useful functions
-- ######

-- Takes a Turn and applies it to the GameState, returning the
-- new, updated GameState
doTurn :: Turn -> GameState -> Either Error GameState
doTurn = error "implement me"

-- Returns the winning color if a win is present in this GameState.
hasWin :: GameState -> Maybe Color
hasWin = error "implement me"

-- Creates an initial state based on the board size
initialState :: BoardSize -> Either Error GameState
initialState = error "implement me"

-- Returns the color that controls a given Pointm if any
controllingColor :: Point -> Maybe Color
controllingColor = error "implement me"

lookupSquare :: Point -> GameState -> [Piece]
lookupSquare = error "implement me"


-- take the initial state, apply a list of turns,
-- checking if there's a win after every turn
