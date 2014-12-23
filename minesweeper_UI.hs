{- demonstrates the use of a simple menu, statusbar, and dialog -}
--module Main where

import Graphics.UI.WX


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
 
prompt :: GameState -> IO ()
prompt gs = do
  putStrLn "Enter your move (f = flag, c = click, q = quit)"
  line <- getLine
  case (dropWhile isSpace $ takeWhile (not . isSpace) line) of
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
          prompt gs'
    "c" -> do
      putStrLn "Enter the coordinate of the square you wish to click"
      line <- getLine
      let pos = read line :: (Int, Int)
 
      case (makeMove (Move Click pos) gs) of
        (Left Win) -> do
          putStrLn "You won!"
        (Left Lose) -> do
          putStrLn "You lost!"
        (Right gs')  -> do
          showBoard gs'
          prompt gs'
    "q" -> do
      return ()
    _ -> do
      prompt gs
 
 
 
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
    --showBoard gs
    --showInternal gs
    --prompt gs
    return ()


--used in GUI
--waitForInput p gs = 


--used in GUI
printInternalBoard x y w h p gs = 
	 if (y > h) then do
		return ()
	 else do
	     set p [visible := True]
	     printInternalRow 1 y w h p gs
             printInternalBoard x (y+1) w h p gs

--used in GUI
printInternalRow x y w h p gs =
      if (x > w) then do
        return ()
      else do
	--butn   <- button p [ position := pt (n*21) (t), size := Size 20 20 ]
	
	butn <- bitmapButton p [ position := pt (x*24) (y*24) ,size := Size 24 24, enabled := False]
        randomMoveButton <- button p [text := "Random move!", position := pt (w*5) ((h*27)), size := Size 105 27, enabled := False]
        autoMoveButton   <- button p [text := "AI move!", position := pt (w*16) ((h*27)), size := Size 90 27, enabled := False]
        -- NOTE: (M.!) is the same as data.Map's lobutnup function but you know its not a "Maybe X", its definitely an X
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


--capturing common pattern
updateBoard x y w h p gs =  do 
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
 		 printBoard 1 1 w h p gs'


randomMove w h p gs = do
      x_rand     <- getStdRandom(randomR (1, w))
      y_rand     <- getStdRandom(randomR (1, h))
      let position   = (x_rand, y_rand)
      let z = 1
      let squares = getBoard gs
      let squares_int = getInternal gs
      print position
      if ( validMove (Move Click position) (gs)  == True) then
          updateBoard x_rand y_rand w h p gs
          {-case (makeMove (Move Click (x_rand,y_rand)) gs) of
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
-}

{-
        case (makeMove (Move Click position) gs) of
          (Left Win) -> do
            putStrLn "You won!"
          (Left Lose) -> do
            putStrLn "You lost!"
          (Right gs')  -> do
            showBoard gs'
            prompt gs' (w, h)-}
      else do
        print "Retrying..."
        randomMove w h p gs


--used in GUI
printBoard x y w h p gs = 
	 if (y > h) then do
		return ()
	 else do
	     set p [visible := True]
	     printRow 1 y w h p gs
             printBoard x (y+1) w h p gs

--used in GUI
printRow x y w h p gs =
      if (x > w) then do
        return ()
      else do
	--butn   <- button p [ position := pt (n*21) (t), size := Size 20 20 ]
	randomMoveButton <- button p [text := "Random move!", position := pt (w*5) ((h*27)), size := Size 105 27, on click := (\pt -> randomMove w h p gs )]
        autoMoveButton   <- button p [text := "AI move!", position := pt (w*16) ((h*27)), size := Size 90 27, on click := (\pt ->  putStrLn "autoMoveButton pressed !" )]
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
		 --start (createGUI  (w,h) 1 gs'))
		 ,  on clickRight :=(\pt ->  
		case (makeMove (Move Flag (x,y)) gs) of
		(Left Win) -> do
		  putStrLn "You won!"
                  printInternalBoard 1 1 w h p gs
		(Left Lose) -> do
		  putStrLn "You lost!"
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
	{-then 
	        if ((getInternal gs) M.! (n,h)) == IAdjacent 1 then set butn [ picture := "1.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 2 then set butn [ picture := "2.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 3 then set butn [ picture := "3.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 4 then set butn [ picture := "4.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 5 then set butn [ picture := "5.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 6 then set butn [ picture := "6.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 7 then set butn [ picture := "7.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 8 then set butn [ picture := "8.png"] 
		else if ((getInternal gs) M.! (n,h)) == IAdjacent 9 then set butn [ picture := "9.png"] 
		else if ((getInternal gs) M.! (n,h)) == Mine then set butn [ picture := "mine.png"]   
	        else return ()
	else return ()-}
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

{-
	if ((getInternal gs) M.! (n,h)) == Mine then set butn [ picture := "mine.png"]  else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 1 then set butn [ picture := "1.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 2 then set butn [ picture := "2.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 3 then set butn [ picture := "3.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 4 then set butn [ picture := "4.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 5 then set butn [ picture := "5.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 6 then set butn [ picture := "6.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 7 then set butn [ picture := "7.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 8 then set butn [ picture := "8.png"] else return ()
	if ((getInternal gs) M.! (n,h)) == IAdjacent 9 then set butn [ picture := "9.png"] else return () 
  -}    
	printRow (x+1) y w h p gs




--createGUI :: (Int, Int) -> Int -> IO ()
createGUI  (w,h) n gs = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz (w*30) (h*33)]                               
       --nb      <- notebobutn p []
       --radio button panel
       p   <- panel f []
       printBoard 1 1 w h p gs
       --waitForInput p gs
      
       -- create file menu  
       file   <- menuPane      [text := "&File"]
       quit   <- menuQuit file [help := "Quit the demo", on command :=  close f]
       return ()
       -- create Help menu
      -- hlp    <- menuHelp      []
       --about  <- menuAbout hlp [help := "About wxHaskell"]

       -- create statusbar field
       --status <- statusField   [text := "Welcome to Minesweeper!"]

       -- set the statusbar and menubar
       --set f [ statusBar := [status]]
 
  





{-
main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               
       --nb      <- notebobutn p []
       --radio button panel
       p   <- panel f []
       let rlabels = ["click mode", "flag mode"]
       r1   <- radioBox p Vertical rlabels   [text := "Select mode:"]
       butn   <- button p [text := "butn", on click := (\pt -> hello), on clickRight :=(\pt -> hello) ]

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       quit   <- menuQuit file [help := "Quit the demo", on command := close f]

       -- create Help menu
      -- hlp    <- menuHelp      []
       --about  <- menuAbout hlp [help := "About wxHaskell"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to Minesweeper!"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             
             ]


createButton p = do  butn   <- button p [text := "butn", on click := (\pt -> hello), on clickRight :=(\pt -> hello) ]

-}
