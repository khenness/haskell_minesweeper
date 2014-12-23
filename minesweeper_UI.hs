{- ghc -package wx --make minesweeper_UI.hs -}

import Graphics.UI.WX
import Minesweeper
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import Data.Char
import System.Random
import System.IO.Unsafe


--show player gamestate in console
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





--show internal gamestate in console 
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
    showInternal gs
    start (createGUI  (w,h) n gs)
    return ()

--Print internal board in window
printInternalBoard x y w h p gs = 
	 if (y > h) then do
		return ()
	 else do
	     set p [visible := True]
	     printInternalRow 1 y w h p gs
             printInternalBoard x (y+1) w h p gs

--Print internal row in window
printInternalRow x y w h p gs =
      if (x > w) then do
        return ()
      else do
	butn <- bitmapButton p [ position := pt (x*24) (y*24) ,size := Size 24 24, enabled := False]
        randomMoveButton <- button p [text := "Random move!", position := pt (w*5) ((h*27)), size := Size 105 27, enabled := False]
        autoMoveButton   <- button p [text := "AI move!", position := pt (w*16) ((h*27)), size := Size 90 27, enabled := False]
        -- NOTE: (M.!) is the same as data.Map's lookup function but you know its not a "Maybe X", its definitely an X
	--use visible := False to get rid of stuff
	
	if ((getInternal gs) M.! (x,y)) == Mine then set butn [ picture := "mine.png"]  
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 1 then set butn [ picture := "1.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 2 then set butn [ picture := "2.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 3 then set butn [ picture := "3.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 4 then set butn [ picture := "4.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 5 then set butn [ picture := "5.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 6 then set butn [ picture := "6.png"]
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 7 then set butn [ picture := "7.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 8 then set butn [ picture := "8.png"] 
	else if ((getInternal gs) M.! (x,y)) == IAdjacent 9 then set butn [ picture := "9.png"] 
        else return ()
	printInternalRow (x+1) y w h p gs


smartFlag (w, h) gs [] = gs
smartFlag (w,h) gs (x : xs) = do
  --print x
  let gs' = adjPos [x] gs
  --showBoard gs'
  smartFlag (w,h) gs' (xs)


smartMove w h p gs  = do
  let clicked_pos = allClickedPositions gs (w,h)
  --print $ length clicked_pos
  --print clicked_pos
  let adj = adjacentPositions $ head clicked_pos
  --print adj
  let gs' = smartFlag (w,h) gs clicked_pos
  let free_pos = allFreePositions gs' (w,h)
  
  let x  = unsafePerformIO (getStdRandom (randomR (0, length free_pos-1)))
  let  ranPos = (free_pos !! x )
  print ranPos
  if ( validMove (Move Click ranPos) (gs')  == True) then
    case (makeMove (Move Click ranPos) gs') of
     	 (Left Win) -> do
	  putStrLn "You won!"
	  printInternalBoard 1 1 w h p gs
	  ctext <- staticText p [ text := "You won!"]
	  return ()
	 (Left Lose) -> do
	  putStrLn "You lost!"
	  printInternalBoard 1 1 w h p gs
	  ctext <- staticText p [ text := "You lost!"]
	  return ()
	 (Right gs')  -> do
	 --p2   <- panel f []
	 --set p [visible := False]
	 printBoard 1 1 w h p gs'
  else do
        print "Retrying smartMove..."
        smartMove w h p gs




randomMove w h p gs = do
      x_rand     <- getStdRandom(randomR (1, w))
      y_rand     <- getStdRandom(randomR (1, h))
      let position   = (x_rand, y_rand)
      let z = 1
      let squares = getBoard gs
      let squares_int = getInternal gs
      print position
      if ( validMove (Move Click position) (gs)  == True) then
          case (makeMove (Move Click (x_rand,y_rand)) gs) of
		(Left Win) -> do
		  putStrLn "You won!"
                  printInternalBoard 1 1 w h p gs
                  ctext <- staticText p [ text := "You won!"]
		  return ()
		(Left Lose) -> do
		  putStrLn "You lost!"
		  printInternalBoard 1 1 w h p gs
	          ctext <- staticText p [ text := "You lost!"]
		  return ()
		(Right gs')  -> do
		 --p2   <- panel f []
		 --set p [visible := False]
 		 printBoard 1 1 w h p gs'

      else do
        print "Retrying..."
        randomMove w h p gs



printBoard x y w h p gs = 
	 if (y > h) then do
		return ()
	 else do
	     set p [visible := True]
	     printRow 1 y w h p gs
             printBoard x (y+1) w h p gs

printRow x y w h p gs =
      if (x > w) then do
        return ()
      else do
	randomMoveButton <- button p [text := "Random move!", position := pt (w*5) ((h*27)), size := Size 105 27, on click := (\pt -> randomMove w h p gs )]
        autoMoveButton   <- button p [text := "AI move!", position := pt (w*16) ((h*27)), size := Size 90 27, 
                                     on click := (\pt -> do smartMove w h p gs
                                                            putStrLn "autoMoveButton pressed!"
                                                 )
                                     ]
	butn <- bitmapButton p [ position := pt (x*24) (y*24) ,size := Size 24 24, on click := (\pt ->
		case (makeMove (Move Click (x,y)) gs) of
		(Left Win) -> do
		  putStrLn "You won!"
                  printInternalBoard 1 1 w h p gs
                  ctext <- staticText p [ text := "You won!"]
		  return ()
		(Left Lose) -> do
		  putStrLn "You lost!"
		  printInternalBoard 1 1 w h p gs
	          ctext <- staticText p [ text := "You lost!"]
		  return ()
		(Right gs')  -> do
		 --p2   <- panel f []
		 --set p [visible := False]
 		 printBoard 1 1 w h p gs')
		 ,  on clickRight :=(\pt ->  
		case (makeMove (Move Flag (x,y)) gs) of
		(Left Win) -> do
		  putStrLn "You won!"
                  printInternalBoard 1 1 w h p gs
		(Left Lose) -> do
		  putStrLn "You lost!"
	          ctext <- staticText p [ text := "You lost!"]
                  printInternalBoard 1 1 w h p gs
		(Right gs')  -> do
                 --set p [visible := False]
		 --p   <- panel f []
 		 printBoard 1 1 w h p gs')
		  --start (createGUI  (w,h) 1 gs'))
		]
        -- NOTE: (M.!) is the same as data.Map's lobutnup function but you know its not a "Maybe X", its definitely an X
	--use visible := False to get rid of stuff
	if ((getBoard gs) M.! (x,y)) == Undiscovered then do set butn []
	else if ((getBoard gs) M.! (x,y)) == Flagged then set butn [ picture := "flag.png" ]
	else if ((getBoard gs) M.! (x,y)) == Adjacent 0 then set butn [  enabled := False]  
	else if ((getBoard gs) M.! (x,y)) == Adjacent 1 then set butn [ picture := "1.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 2 then set butn [ picture := "2.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 3 then set butn [ picture := "3.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 4 then set butn [ picture := "4.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 5 then set butn [ picture := "5.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 6 then set butn [ picture := "6.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 7 then set butn [ picture := "7.png", enabled := False] 
	else if ((getBoard gs) M.! (x,y)) == Adjacent 8 then set butn [ picture := "8.png", enabled := False]
	else if ((getBoard gs) M.! (x,y)) == Adjacent 9 then set butn [ picture := "9.png", enabled := False]
	else if ((getBoard gs) M.! (x,y)) == Clicked then set butn [ picture := "0.png" , enabled := False]
	else return () 
	printRow (x+1) y w h p gs




--createGUI :: (Int, Int) -> Int -> IO ()
createGUI  (w,h) n gs = do -- the application frame
       f      <- frame         [text := "Minesweeper-o-matic!", clientSize := sz (w*30) (h*33)]                               
       p   <- panel f []
       printBoard 1 1 w h p gs
      
       -- create file menu  
       file   <- menuPane      [text := "&File"]
       quit   <- menuQuit file [help := "Quit the demo", on command :=  close f]
       return ()
