{- demonstrates the use of a simple menu, statusbar, and dialog -}
--module Main where

import Graphics.UI.WX


import Minesweeper
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import Data.Char
 
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
    start (initializeGUI (w,h) n)
    showBoard gs
    showInternal gs
    prompt gs
    return ()


initializeGUI :: (Int, Int) -> Int -> IO ()
initializeGUI (_,_) _ = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               
       --nb      <- notebook p []
       --radio button panel
       p   <- panel f []
       let rlabels = ["click mode", "flag mode"]
       r1   <- radioBox p Vertical rlabels   [text := "Select mode:"]
       ok   <- button p [text := "Ok" ] 

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       quit   <- menuQuit file [help := "Quit the demo", on command :=  close f]

       -- create Help menu
      -- hlp    <- menuHelp      []
       --about  <- menuAbout hlp [help := "About wxHaskell"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to Minesweeper!"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             
             ]





{-
main :: IO ()
main
  = start hello
hello :: IO ()
hello
  = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               
       --nb      <- notebook p []
       --radio button panel
       p   <- panel f []
       let rlabels = ["click mode", "flag mode"]
       r1   <- radioBox p Vertical rlabels   [text := "Select mode:"]
       ok   <- button p [text := "Ok", on click := (\pt -> hello), on clickRight :=(\pt -> hello) ]
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
createButton p = do  ok   <- button p [text := "Ok", on click := (\pt -> hello), on clickRight :=(\pt -> hello) ]
-}
