--source: https://gist.github.com/dancannon/4108039


module Minesweeper
  ( Pos
  , MoveType(..)
  , Move(..)
  , EndOfGame(..)
  , Square(..)
  , InternalSquare(..)
  , GameState
  , generateBoard
  , getBounds
  , getBoard
  , getInternal
  , makeMove
  , validMove
  , allClickedPositions
  , adjPos
  , flagAdj
  , adjacentPositions
  , validSMove
  , allFreePositions
) where
 
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import System.Random
 
-- | A board position
type Pos = (Int, Int) -- ^ (x, y) coordinates
 
data MoveType = Click | Flag deriving (Eq, Show)
data Move = Move MoveType Pos deriving (Eq, Show)
 
data EndOfGame = Win | Lose deriving (Eq, Show)
 
-- | The possible states of a square in the board
data Square
  = Undiscovered -- ^ An unclicked square
  | Clicked      -- ^ A clicked square
  | Flagged      -- ^ A square with a flag
  | Adjacent Int -- ^ A discovered square, where the number represents

                 --   the number of adjacent mines
  deriving (Eq, Show)
 
-- | Information about mines, not visible to the user
data InternalSquare
  = Mine          -- ^ This square is a mine
  | IAdjacent Int -- ^ This square is not a mine, but has n adjacent mines
  deriving (Eq, Show)
 
-- | The current state of the board
data GameState
  = GameState
    (Int, Int)               -- ^ Size of the board (horizontal, vertical)
    (Map Pos Square)         -- ^ The user-visible board
    (Map Pos InternalSquare) -- ^ Internal mine information
  deriving Show
 
inBounds :: Pos -> (Int, Int) -> Bool
inBounds (x,y) (w,h) = x >= 1 && y >= 1 && x <= w && y <= h
 
getBounds :: GameState -> (Int, Int)
getBounds (GameState bounds _ _) = bounds
 
getBoard :: GameState -> Map Pos Square
getBoard (GameState _ board _) = board
 
getInternal :: GameState -> Map Pos InternalSquare
getInternal (GameState _ _ internal) = internal
 
--adjacentPositions :: Pos -> [Pos]
--adjacentPositions (x,y) = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1)]
 
movePos :: Move -> Pos
movePos (Move _ pos) = pos
 
validMove :: Move -> GameState -> Bool
validMove (Move _ (x,y)) (GameState (w,h) squares _) =
  if (not(inBounds (x,y) (w,h))) then
    False
  else
    case square of Just Undiscovered -> True
                   Just Flagged      -> True
                   _                 -> False
  where
    square = M.lookup (x,y) squares

validSMove :: Move -> GameState -> Bool
validSMove (Move _ (x,y)) (GameState (w,h) squares _) =
  if (not(inBounds (x,y) (w,h))) then
    False
  else
    case square of Just Undiscovered -> True
                   Just Flagged      -> False
                   _                 -> False
  where
    square = M.lookup (x,y) squares
 
allPositions :: (Int, Int) -> [Pos]
allPositions (w,h) = getRow w h
  where
    getCellsOfRow 1 y = [(1,y)]
    getCellsOfRow x y = (x,y) : getCellsOfRow (x-1) y
    getRow w 1 = getCellsOfRow w 1
    getRow w y = getCellsOfRow w y ++ getRow w (y-1)
 
generateBoard :: (Int, Int) -> Int -> IO GameState
generateBoard (w,h) num = do
  let poss = allPositions (w,h)
  mines <- getRandMines num (w,h) []
  let board = foldr (\pos acc -> (pos,Undiscovered):acc) [] poss
  let internal = foldr (\pos acc -> if (elem pos mines) then (pos,Mine):acc else let adjacent = foldr (\adjPos acc -> if (elem adjPos mines) then 1+acc else acc) 0 (adjacentPositions pos) in (pos,IAdjacent adjacent):acc) [] poss
  let state = GameState (w,h) (M.fromList board) (M.fromList internal)
  return state
 
  where
    getRandMines 0 _ mines =
      return mines
    getRandMines n (w, h) mines = do
      x <- getStdRandom(randomR (1, w))
      y <- getStdRandom(randomR (1, h))
      if elem (x,y) mines then
        getRandMines n (w, h) mines
      else
        getRandMines (n-1) (w, h) ((x, y):mines)
 
checkState :: GameState -> Either EndOfGame GameState
checkState gs =
  if (not(M.null(M.intersection clicked mines))) then
    Left Lose
  else if (M.null (M.difference unclicked mines)) then
    Left Win
  else
    Right gs
  where
    clicked   = M.filter (\square -> square == Clicked) (getBoard gs)
    unclicked = M.filter (\square -> square == Undiscovered || square == Flagged) (getBoard gs)
    mines     = M.filter (\square -> square == Mine) (getInternal gs)
 
 
makeMove :: Move -> GameState -> Either EndOfGame GameState
makeMove (Move mvtype pos) (gs) =
  if (mvtype == Click) then
    checkState(click pos gs)
  else
    checkState(flag pos gs)
 
fill :: Pos -> S.Set Pos -> GameState -> (S.Set Pos, GameState)
fill pos visited gs =
  if (S.member pos visited) then
    (visited,gs)
  else
    case isquare of (Just (IAdjacent n)) -> if (n == 0) then
                                              fillHelper (adjacentPositions pos) (S.insert pos visited) (GameState (getBounds gs) (M.alter (\x -> Just (Adjacent n)) pos (getBoard gs)) (getInternal gs))
                                            else
                                              fillHelper [] (S.insert pos visited) (GameState (getBounds gs) (M.alter (\x -> Just (Adjacent n)) pos (getBoard gs)) (getInternal gs))
                    _                    -> (visited, gs)
  where
    isquare = M.lookup pos (getInternal gs)
    fillHelper [] visited gs = (visited,gs)
    fillHelper (pos:poss) visited gs =
      if (inBounds pos (getBounds gs)) then
        let (visited', gs') = fill pos visited gs in
        fillHelper poss visited' gs'
      else
        fillHelper poss (visited) gs
 
click :: Pos -> GameState -> GameState
click pos gs = case isquare of Just Mine           -> (GameState (getBounds gs) (M.alter (\x -> Just Clicked) pos (getBoard gs)) (getInternal gs))
                               Just (IAdjacent an) -> gs'
  where
    isquare = M.lookup pos (getInternal gs)
    (_, gs') = fill pos (S.empty) gs
 
flag :: Pos -> GameState -> GameState
flag pos gs = case square of Just Flagged          ->  (GameState (getBounds gs) (M.alter (\x -> Just Undiscovered) pos (getBoard gs)) (getInternal gs))
                             Just Undiscovered     ->  (GameState (getBounds gs) (M.alter (\x -> Just Flagged   )   pos (getBoard gs)) (getInternal gs))
                             Just Clicked          ->  (GameState (getBounds gs) (M.alter (\x -> Just Clicked   )   pos (getBoard gs)) (getInternal gs))
                             Just (Adjacent an)    ->  (GameState (getBounds gs) (M.alter (\x -> Just (Adjacent an))pos (getBoard gs)) (getInternal gs))
                             Nothing               ->  (GameState (getBounds gs) (M.alter (\x -> Nothing     )      pos (getBoard gs)) (getInternal gs))
  where
    square = M.lookup pos (getBoard gs)

allClickedPositions :: GameState -> (Int, Int) -> [Pos]
allClickedPositions gs (w,h) = getRow w h
  where
    getCellsOfRow 0 y = []
    getCellsOfRow x y = 
      if ( validMove (Move Flag (x,y) ) (gs)  == False) then
        
        --if () then
            (x,y) : getCellsOfRow (x-1) y
          --else

      else
        getCellsOfRow (x-1) y
    getRow w 1 = getCellsOfRow w 1
    getRow w y = getCellsOfRow w y ++ getRow w (y-1)

allFreePositions :: GameState -> (Int, Int) -> [Pos]
allFreePositions gs (w,h) = getRow w h
  where
    getCellsOfRow 0 y = []
    getCellsOfRow x y = 
      if ( validSMove (Move Click (x,y) ) (gs)  == True) then
        
        --if () then
            (x,y) : getCellsOfRow (x-1) y
          --else

      else
        getCellsOfRow (x-1) y
    getRow w 1 = getCellsOfRow w 1
    getRow w y = getCellsOfRow w y ++ getRow w (y-1)


adjacentPositions :: Pos -> [Pos]
adjacentPositions (x,y) = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1)]

adjPos :: [Pos] -> GameState ->  GameState
adjPos [] gs = gs
adjPos (x:xs) gs = do
  let adj = adjacentPositions x
  let gs' = flagAdj adj gs
  adjPos xs gs'
  
  
flagAdj :: [Pos] -> GameState -> GameState
flagAdj [] gs = gs
flagAdj (pos:xs) gs = do
    let gs' = flag pos gs
    flagAdj xs gs'