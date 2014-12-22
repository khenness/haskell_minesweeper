import Minesweeper
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import Data.Char
import System.Random
 
showBoard :: GameState -> IO ()
showBoard gs = do  -- Print col names
  putStr "   "
  putStrLn . unwords $ map show [1..w]
 
  -- Print rows
  printRow 1 h gs
 
  where
    (w,h) = getBounds gs
    printRow n t gs =
      if (n > t) then do
        return ()
      else do
        putStr ((show n) ++ " :")
        putStrLn . unwords $ rowHelper (rowSquares n gs)
        printRow (n+1) t gs
 
 
      where
        rowSquares n gs = M.filterWithKey  (\(x,y) s -> y == n) (getBoard gs)
        cellHelper Flagged      = "F"
        cellHelper (Adjacent n) = show n
        cellHelper _            = " "
        rowHelper squares = [cellHelper s|(_,s) <- (M.toList squares)]
 
showInternal :: GameState -> IO ()
showInternal gs = do  -- Print col names
  putStr "   "
  putStrLn . unwords $ map show [1..w]
 
  -- Print rows
  printRow 1 h gs
 
  where
    (w,h) = getBounds gs
    printRow n t gs =
      if (n > t) then do
        return ()
      else do
        putStr ((show n) ++ " :")
        putStrLn . unwords $ rowHelper (rowSquares n gs)
        printRow (n+1) t gs
 
 
      where
        rowSquares n gs = M.filterWithKey  (\(x,y) s -> y == n) (getInternal gs)
        cellHelper Mine      = "M"
        cellHelper (IAdjacent n) = show n
        rowHelper squares = [cellHelper s|(_,s) <- (M.toList squares)]
 
prompt :: GameState -> (Int, Int) -> IO ()
prompt gs (w, h)= do
  putStrLn "Enter your move (f = flag, c = click, rm = random move, sm = smart move,   q = quit)"
  line <- getLine
  case (dropWhile isSpace $ takeWhile (not . isSpace) line) of -- checks for space and retu
    "f" -> do
      putStrLn "Enter the coordinate of the square you wish to flag"
      line <- getLine
      let pos = read line :: (Int, Int)
 
      case (makeMove (Move Flag pos) gs) of
        (Left Win) -> do
          putStrLn "You won!"
        (Left Lose) -> do
          putStrLn "You lost!"
        (Right gs')  -> do
          showBoard gs'
          showInternal gs'
          prompt gs' (w, h)
    "c" -> do
      putStrLn "Enter the coordinate of the square you wish to click"
      line <- getLine
      let pos = read line :: (Int, Int)
 
      case (makeMove (Move Click pos) gs) of
        (Left Win) -> do
          showBoard gs
          putStrLn "You won!"
        (Left Lose) -> do
          showBoard gs
          putStrLn "You lost!"
        (Right gs')  -> do
          showBoard gs'
          prompt gs' (w, h)
    "rm" -> do
      promptRandom (w,h) gs
      --return ()
    "q" -> do
      return ()
    _ -> do
      prompt gs (w, h)
 

promptRandom   (w,h)  gs = do
      x     <- getStdRandom(randomR (1, w))
      y     <- getStdRandom(randomR (1, h))
      let position   = (x, y)
      let z = 1
      let squares = getBoard gs
      let squares_int = getInternal gs
      print position
      if ( validMove (Move Click position) (gs)  == True) then

        case (makeMove (Move Click position) gs) of
          (Left Win) -> do
            putStrLn "You won!"
          (Left Lose) -> do
            putStrLn "You lost!"
          (Right gs')  -> do
            showBoard gs'
            prompt gs' (w, h)
      else do
        print "Retrying..."
        promptRandom (w,h) gs
 
main :: IO ()
main = do
  putStrLn "Please enter the board width"
  line <- getLine 
  let w = read line :: Int
 
  putStrLn "Please enter the board height"
  line <- getLine
  let h = read line :: Int
 
  putStrLn "Please enter the number of mines"
  line <- getLine
  let n = read line :: Int
 
  putStr "\n\n"
 
  if (w < 0 || h < 0 || (w*h) < n) then
    putStrLn "The data you entered was invalid, please try again\n" >> main
  else do
    gs <- (generateBoard (w,h) n)
    showBoard gs
    showInternal gs
    prompt gs (w,h)
    return ()